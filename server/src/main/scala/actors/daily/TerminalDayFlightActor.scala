package actors.daily

import actors.PortStateMessageConversion.flightsFromMessages
import actors.{FlightMessageConversion, GetState, RecoveryActorLike, Sizes}
import akka.actor.Props
import akka.persistence.{Recovery, SnapshotSelectionCriteria}
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.FlightsApi.{FlightsWithSplits, FlightsWithSplitsDiff}
import drt.shared.Terminals.Terminal
import drt.shared.{SDateLike, UniqueArrival, UtcDate}
import org.slf4j.{Logger, LoggerFactory}
import scalapb.GeneratedMessage
import server.protobuf.messages.CrunchState.{FlightWithSplitsMessage, FlightsWithSplitsDiffMessage, FlightsWithSplitsMessage}
import server.protobuf.messages.FlightsMessage.UniqueArrivalMessage
import services.SDate


object TerminalDayFlightActor {
  def props(terminal: Terminal, date: UtcDate, now: () => SDateLike): Props =
    Props(new TerminalDayFlightActor(date.year, date.month, date.day, terminal, now, None))

  def propsPointInTime(terminal: Terminal, date: UtcDate, now: () => SDateLike, pointInTime: MillisSinceEpoch): Props =
    Props(new TerminalDayFlightActor(date.year, date.month, date.day, terminal, now, Option(pointInTime)))
}

class TerminalDayFlightActor(
                              year: Int,
                              month: Int,
                              day: Int,
                              terminal: Terminal,
                              val now: () => SDateLike,
                              maybePointInTime: Option[MillisSinceEpoch]
                            ) extends RecoveryActorLike {

  val loggerSuffix: String = maybePointInTime match {
    case None => ""
    case Some(pit) => f"@${SDate(pit).toISOString()}"
  }

  val firstMinuteOfDay = SDate(year, month, day, 0, 0)
  val lastMinuteOfDay = firstMinuteOfDay.addDays(1).addMinutes(-1)

  override val log: Logger = LoggerFactory.getLogger(f"$getClass-$terminal-$year%04d-$month%02d-$day%02d$loggerSuffix")


  var state: FlightsWithSplits = FlightsWithSplits.empty

  override def persistenceId: String = f"terminal-flights-${terminal.toString.toLowerCase}-$year-$month%02d-$day%02d"


  override val snapshotBytesThreshold: Int = Sizes.oneMegaByte
  private val maxSnapshotInterval = 250
  override val maybeSnapshotInterval: Option[Int] = Option(maxSnapshotInterval)
  override val recoveryStartMillis: MillisSinceEpoch = now().millisSinceEpoch

  override def recovery: Recovery = maybePointInTime match {
    case None =>
      Recovery(SnapshotSelectionCriteria(Long.MaxValue, maxTimestamp = Long.MaxValue, 0L, 0L))
    case Some(pointInTime) =>
      val criteria = SnapshotSelectionCriteria(maxTimestamp = pointInTime)
      Recovery(fromSnapshot = criteria, replayMax = maxSnapshotInterval)
  }

  override def receiveCommand: Receive = {
    case diff: FlightsWithSplitsDiff =>

      val filteredDiff = diff.forTerminal(terminal)
        .window(firstMinuteOfDay.millisSinceEpoch, lastMinuteOfDay.millisSinceEpoch)

      if (diff == filteredDiff)
        log.info(s"Received FlightsWithSplits for persistence ${diff.flightsToUpdate.size}")
      else
        logDifferences(diff, filteredDiff)

      updateAndPersistDiff(filteredDiff)

    case GetState =>
      log.debug(s"Received GetState")
      sender() ! state

    case m => log.warn(s"Got unexpected message: $m")
  }

  def logDifferences(diff: FlightsWithSplitsDiff, filteredDiff: FlightsWithSplitsDiff): Unit = log.error(
    s"Received flights for wrong day or terminal " +
      s"${diff.flightsToUpdate.size} flights sent in ${filteredDiff.flightsToUpdate.size} persisted," +
      s"${diff.arrivalsToRemove} removals sent in ${filteredDiff.arrivalsToRemove.size} persisted"
  )

  def updateAndPersistDiff(diff: FlightsWithSplitsDiff): Unit = {

    val (updatedState, minutesToUpdate) = diff.applyTo(state, now().millisSinceEpoch)
    state = updatedState

    val replyToAndMessage = Option(sender(), minutesToUpdate)
    persistAndMaybeSnapshot(FlightMessageConversion.flightWithSplitsDiffToMessage(diff), replyToAndMessage)
  }

  override def processRecoveryMessage: PartialFunction[Any, Unit] = {
    case diff: FlightsWithSplitsDiffMessage => handleDiffMessage(diff)
  }

  override def processSnapshotMessage: PartialFunction[Any, Unit] = {
    case FlightsWithSplitsMessage(flightMessages) =>
      log.info(s"Processing snapshot message")
      setStateFromSnapshot(flightMessages)
  }

  override def stateToMessage: GeneratedMessage = FlightMessageConversion.flightsToMessage(state.flights.toMap.values)

  def handleDiffMessage(diff: FlightsWithSplitsDiffMessage): Unit = {
    state = state -- diff.removals.map(uniqueArrivalFromMessage)
    state = state ++ flightsFromMessages(diff.updates)
    log.debug(s"Recovery: state contains ${state.flights.size} flights")
  }

  def uniqueArrivalFromMessage(uam: UniqueArrivalMessage): UniqueArrival =
    UniqueArrival(uam.getNumber, uam.getTerminalName, uam.getScheduled)

  def setStateFromSnapshot(flightMessages: Seq[FlightWithSplitsMessage]): Unit = {
    state = FlightsWithSplits(flightsFromMessages(flightMessages))
  }
}