package actors

import actors.PortStateMessageConversion._
import actors.restore.RestorerWithLegacy
import akka.actor._
import akka.persistence._
import drt.shared.CrunchApi._
import drt.shared.FlightsApi._
import drt.shared.SplitRatiosNs.SplitSources
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import scalapb.GeneratedMessage
import server.protobuf.messages.CrunchState._
import server.protobuf.messages.FlightsMessage.UniqueArrivalMessage
import services.SDate
import services.graphstages.PortStateWithDiff


class CrunchStateActor(initialMaybeSnapshotInterval: Option[Int],
                       initialSnapshotBytesThreshold: Int,
                       name: String,
                       portQueues: Map[TerminalName, Seq[QueueName]],
                       now: () => SDateLike,
                       expireAfterMillis: MillisSinceEpoch,
                       purgePreviousSnapshots: Boolean,
                       acceptFullStateUpdates: Boolean) extends PersistentActor with RecoveryActorLike with PersistentDrtActor[PortStateMutable] {
  override def persistenceId: String = name

  override val maybeSnapshotInterval: Option[Int] = initialMaybeSnapshotInterval
  override val snapshotBytesThreshold: Int = initialSnapshotBytesThreshold

  val log: Logger = LoggerFactory.getLogger(s"$name-$getClass")

  def logInfo(msg: String, level: String = "info"): Unit = if (name.isEmpty) log.info(msg) else log.info(s"$name $msg")

  def logDebug(msg: String, level: String = "info"): Unit = if (name.isEmpty) log.debug(msg) else log.debug(s"$name $msg")

  val restorer = new RestorerWithLegacy[Int, UniqueArrival, ApiFlightWithSplits]

  var state: PortStateMutable = initialState

  def initialState: PortStateMutable = PortStateMutable.empty

  def processSnapshotMessage: PartialFunction[Any, Unit] = {
    case snapshot: CrunchStateSnapshotMessage => setStateFromSnapshot(snapshot)
  }

  def processRecoveryMessage: PartialFunction[Any, Unit] = {
    case diff: CrunchDiffMessage =>
      applyRecoveryDiff(diff)
      logRecoveryState()
      bytesSinceSnapshotCounter += diff.serializedSize
      messagesPersistedSinceSnapshotCounter += 1
  }

  override def postRecoveryComplete(): Unit = {
    restorer.finish()
    state.flights ++= restorer.items
    restorer.clear()

    state.purgeOlderThanDate(now().millisSinceEpoch - expireAfterMillis)

    super.postRecoveryComplete()
  }

  def logRecoveryState(): Unit = {
    val apiCount = state.flights.count {
      case (_, f) => f.splits.exists {
        case Splits(_, SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages, _, _) => true
        case _ => false
      }
    }
    logDebug(s"Recovery: state contains ${state.flights.size} flights " +
      s"with $apiCount Api splits " +
      s", ${state.crunchMinutes.size} crunch minutes " +
      s", ${state.staffMinutes.size} staff minutes ")
  }

  override def stateToMessage: GeneratedMessage = portStateToSnapshotMessage(state)

  override def receiveCommand: Receive = {
    case PortStateWithDiff(None, _, CrunchDiffMessage(_, _, _, fr, fu, cu, su, _)) if fr.isEmpty && fu.isEmpty && cu.isEmpty && su.isEmpty =>
      log.info(s"Received port state with empty diff and no fresh port state")

    case PortStateWithDiff(Some(ps), _, CrunchDiffMessage(_, _, _, fr, fu, cu, su, _)) if fr.isEmpty && fu.isEmpty && cu.isEmpty && su.isEmpty =>
      log.info(s"Received port state with empty diff, but with fresh port state")
      updateFromFullState(ps)

    case PortStateWithDiff(maybeState, _, diffMsg) =>
      maybeState match {
        case Some(fullState) if acceptFullStateUpdates =>
          updateFromFullState(fullState)
          logInfo("Received full port state")
        case _ =>
          applyDiff(diffMsg)
          logInfo(s"Received port state with diff")
      }

      persistAndMaybeSnapshot(diffMsg)

    case GetState =>
      logDebug(s"Received GetState request. Replying with PortState containing ${state.crunchMinutes.size} crunch minutes")
      sender() ! Option(state.immutable)

    case GetPortState(start, end) =>
      logDebug(s"Received GetPortState Request from ${SDate(start).toISOString()} to ${SDate(end).toISOString()}")
      sender() ! stateForPeriod(start, end)

    case GetPortStateForTerminal(start, end, terminalName) =>
      logDebug(s"Received GetPortState Request from ${SDate(start).toISOString()} to ${SDate(end).toISOString()}")
      sender() ! stateForPeriodForTerminal(start, end, terminalName)

    case GetUpdatesSince(millis, start, end) =>
      val updates = state.window(SDate(start), SDate(end), portQueues).updates(millis)
      sender() ! updates

    case SaveSnapshotSuccess(SnapshotMetadata(_, seqNr, _)) =>
      logInfo(s"Snapshot success. Purging previous snapshots (with sequence number < $seqNr)")
      deleteSnapshots(SnapshotSelectionCriteria(maxSequenceNr = seqNr - 1))

    case SaveSnapshotFailure(md, cause) =>
      logInfo(s"Snapshot failed $md\n$cause")

    case DeleteSnapshotsSuccess(_) =>
      logInfo(s"Purged snapshots")

    case u =>
      log.error(s"Received unexpected message $u")
  }

  def updateFromFullState(ps: PortState): Unit = {
    state.flights ++= ps.flights.map { case (_, fws) => (fws.apiFlight.unique, fws) }
    state.crunchMinutes ++= ps.crunchMinutes
    state.staffMinutes ++= ps.staffMinutes
  }

  def stateForPeriod(start: MillisSinceEpoch, end: MillisSinceEpoch): Option[PortState] = Option(state.window(SDate(start), SDate(end), portQueues))

  def stateForPeriodForTerminal(start: MillisSinceEpoch, end: MillisSinceEpoch, terminalName: TerminalName): Option[PortState] = Option(state.windowWithTerminalFilter(SDate(start), SDate(end), portQueues.filterKeys(_ == terminalName)))

  def setStateFromSnapshot(snapshot: CrunchStateSnapshotMessage, timeWindowEnd: Option[SDateLike] = None): Unit = {
    snapshotMessageToState(snapshot, timeWindowEnd, state)
  }

  def applyRecoveryDiff(cdm: CrunchDiffMessage): Unit = {
    val (flightRemovals, flightUpdates, crunchMinuteUpdates, staffMinuteUpdates) = crunchDiffFromMessage(cdm)
    val nowMillis = now().millisSinceEpoch
    restorer.update(flightUpdates)
    restorer.removeLegacies(cdm.flightIdsToRemoveOLD)
    restorer.remove(flightRemovals)
    state.applyCrunchDiff(crunchMinuteUpdates, nowMillis)
    state.applyStaffDiff(staffMinuteUpdates, nowMillis)
  }

  def uniqueArrivalFromMessage(uam: UniqueArrivalMessage) = {
    UniqueArrival(uam.getNumber, uam.getTerminalName, uam.getScheduled)
  }

  def applyDiff(cdm: CrunchDiffMessage): Unit = {
    val (flightRemovals, flightUpdates, crunchMinuteUpdates, staffMinuteUpdates) = crunchDiffFromMessage(cdm)
    val nowMillis = now().millisSinceEpoch
    state.applyFlightsWithSplitsDiff(flightRemovals, flightUpdates, nowMillis)
    state.applyCrunchDiff(crunchMinuteUpdates, nowMillis)
    state.applyStaffDiff(staffMinuteUpdates, nowMillis)

    state.purgeOlderThanDate(now().millisSinceEpoch - expireAfterMillis)
  }

  def crunchDiffFromMessage(diffMessage: CrunchDiffMessage): (Seq[UniqueArrival], Seq[ApiFlightWithSplits], Seq[CrunchMinute], Seq[StaffMinute]) = (
    diffMessage.flightsToRemove.collect {
      case m if portQueues.contains(m.getTerminalName) => uniqueArrivalFromMessage(m)
    },
    diffMessage.flightsToUpdate.collect {
      case m if portQueues.contains(m.getFlight.getTerminal) => flightWithSplitsFromMessage(m)
    },
    diffMessage.crunchMinutesToUpdate.collect {
      case m if portQueues.contains(m.getTerminalName) => crunchMinuteFromMessage(m)
    },
    diffMessage.staffMinutesToUpdate.collect {
      case m if portQueues.contains(m.getTerminalName) => staffMinuteFromMessage(m)
    }
  )
}
