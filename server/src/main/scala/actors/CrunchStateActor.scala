package actors

import actors.PortStateMessageConversion._
import actors.acking.AckingReceiver.{Ack, StreamCompleted}
import actors.restore.RestorerWithLegacy
import akka.actor._
import akka.persistence._
import drt.shared.CrunchApi._
import drt.shared.FlightsApi._
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import scalapb.GeneratedMessage
import server.protobuf.messages.CrunchState._
import server.protobuf.messages.FlightsMessage.UniqueArrivalMessage
import services.SDate
import services.graphstages.Crunch


class CrunchStateActor(initialMaybeSnapshotInterval: Option[Int],
                       initialSnapshotBytesThreshold: Int,
                       name: String,
                       portQueues: Map[TerminalName, Seq[QueueName]],
                       now: () => SDateLike,
                       expireAfterMillis: MillisSinceEpoch,
                       purgePreviousSnapshots: Boolean,
                       acceptFullStateUpdates: Boolean,
                       forecastMaxMillis: () => MillisSinceEpoch) extends PersistentActor with RecoveryActorLike with PersistentDrtActor[PortStateMutable] {
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
    case snapshot: CrunchStateSnapshotMessage => setStateFromSnapshot(snapshot, timeWindowEnd = Option(SDate(forecastMaxMillis())))
  }

  def processRecoveryMessage: PartialFunction[Any, Unit] = {
    case diff: CrunchDiffMessage =>
      applyRecoveryDiff(diff, forecastMaxMillis())
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
    logDebug(s"Recovery: state contains ${state.flights.count} flights " +
      s", ${state.crunchMinutes.count} crunch minutes " +
      s", ${state.staffMinutes.count} staff minutes ")
  }

  override def stateToMessage: GeneratedMessage = portStateToSnapshotMessage(state)

  override def receiveCommand: Receive = {
    case psd: PortStateDiff =>
      if (!psd.isEmpty) {
        val diffMsg = diffMessage(psd)
        applyDiff(psd, Long.MaxValue)
        persistAndMaybeSnapshot(diffMsg)
      }

      sender() ! Ack

    case GetState =>
      log.debug(s"Received GetState request. Replying with PortState containing ${state.crunchMinutes.count} crunch minutes")
      sender() ! Option(state.immutable)

    case SaveSnapshotSuccess(SnapshotMetadata(_, seqNr, _)) =>
      logInfo("Snapshot success")
      if (purgePreviousSnapshots) {
        logInfo(s"Purging previous snapshots (with sequence number < $seqNr)")
        deleteSnapshots(SnapshotSelectionCriteria(maxSequenceNr = seqNr - 1))
      }

    case SaveSnapshotFailure(md, cause) =>
      log.error(s"Save snapshot failure: $md", cause)

    case DeleteSnapshotsSuccess(_) =>
      logInfo(s"Purged snapshots")

    case StreamCompleted => log.warn("Received shutdown")

    case unexpected => log.error(s"Received unexpected message $unexpected")
  }

  def diffMessage(diff: PortStateDiff): CrunchDiffMessage = CrunchDiffMessage(
    createdAt = Option(now().millisSinceEpoch),
    crunchStart = Option(0),
    flightsToRemove = diff.flightRemovals.values.map { case RemoveFlight(ua) => UniqueArrivalMessage(Option(ua.number), Option(ua.terminalName), Option(ua.scheduled)) }.toSeq,
    flightsToUpdate = diff.flightUpdates.values.map(FlightMessageConversion.flightWithSplitsToMessage).toList,
    crunchMinutesToUpdate = diff.crunchMinuteUpdates.values.map(crunchMinuteToMessage).toList,
    staffMinutesToUpdate = diff.staffMinuteUpdates.values.map(staffMinuteToMessage).toList
  )

  def stateForPeriod(start: MillisSinceEpoch, end: MillisSinceEpoch): Option[PortState] = Option(state.window(SDate(start), SDate(end)))

  def stateForPeriodForTerminal(start: MillisSinceEpoch, end: MillisSinceEpoch, terminalName: TerminalName): Option[PortState] = Option(state.windowWithTerminalFilter(SDate(start), SDate(end), portQueues.keys.filter(_ == terminalName).toSeq))

  def setStateFromSnapshot(snapshot: CrunchStateSnapshotMessage, timeWindowEnd: Option[SDateLike] = None): Unit = {
    snapshotMessageToState(snapshot, timeWindowEnd, state)
  }

  def applyRecoveryDiff(cdm: CrunchDiffMessage, maxMillis: MillisSinceEpoch): Unit = {
    val (flightRemovals, flightUpdates, crunchMinuteUpdates, staffMinuteUpdates) = crunchDiffFromMessage(cdm, maxMillis)
    val nowMillis = now().millisSinceEpoch
    restorer.update(flightUpdates)
    restorer.removeLegacies(cdm.flightIdsToRemoveOLD)
    restorer.remove(flightRemovals)
    state.applyCrunchDiff(crunchMinuteUpdates, nowMillis)
    state.applyStaffDiff(staffMinuteUpdates, nowMillis)
  }

  def uniqueArrivalFromMessage(uam: UniqueArrivalMessage): UniqueArrival = {
    UniqueArrival(uam.getNumber, uam.getTerminalName, uam.getScheduled)
  }

  def applyDiff(cdm: PortStateDiff, maxMillis: MillisSinceEpoch): Unit = {
    val nowMillis = now().millisSinceEpoch
    state.applyFlightsWithSplitsDiff(cdm.flightRemovals.keys.toSeq, cdm.flightUpdates, nowMillis)
    log.info(s"$getClass applyFlightsWithSplitsDiff took ${now().millisSinceEpoch - nowMillis}ms")
    state.applyCrunchDiff(cdm.crunchMinuteUpdates, nowMillis)
    log.info(s"$getClass applyCrunchDiff took ${now().millisSinceEpoch - nowMillis}ms")
    state.applyStaffDiff(cdm.staffMinuteUpdates, nowMillis)
    log.info(s"$getClass applyStaffDiff took ${now().millisSinceEpoch - nowMillis}ms")

    state.purgeOlderThanDate(nowMillis - expireAfterMillis)
    log.info(s"$getClass purgeOlderThanDate took ${now().millisSinceEpoch - nowMillis}ms")
    state.purgeRecentUpdates(nowMillis - Crunch.oneMinuteMillis * 5)
    log.info(s"$getClass purgeRecentUpdates took ${now().millisSinceEpoch - nowMillis}ms")
  }

  def crunchDiffFromMessage(diffMessage: CrunchDiffMessage, maxMillis: MillisSinceEpoch): (Seq[UniqueArrival], Seq[ApiFlightWithSplits], Seq[CrunchMinute], Seq[StaffMinute]) = (
    diffMessage.flightsToRemove.collect {
      case m if portQueues.contains(m.getTerminalName) => uniqueArrivalFromMessage(m)
    },
    diffMessage.flightsToUpdate.collect {
      case m if portQueues.contains(m.getFlight.getTerminal) && m.getFlight.getScheduled < maxMillis => flightWithSplitsFromMessage(m)
    },
    diffMessage.crunchMinutesToUpdate.collect {
      case m if portQueues.contains(m.getTerminalName) && m.getMinute < maxMillis => crunchMinuteFromMessage(m)
    },
    diffMessage.staffMinutesToUpdate.collect {
      case m if portQueues.contains(m.getTerminalName) && m.getMinute < maxMillis => staffMinuteFromMessage(m)
    }
  )
}
