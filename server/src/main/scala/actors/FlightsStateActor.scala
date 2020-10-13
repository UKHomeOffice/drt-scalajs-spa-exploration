package actors

import actors.PartitionedPortStateActor.{DateRangeLike, GetFlights, GetFlightsForTerminal, GetStateForDateRange, GetStateForTerminalDateRange, GetUpdatesSince, PointInTimeQuery}
import actors.PortStateMessageConversion._
import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import actors.daily.{RequestAndTerminate, RequestAndTerminateActor}
import actors.pointInTime.{CrunchStateReadActor, FlightsStateReadActor}
import actors.queues.QueueLikeActor.UpdatedMillis
import akka.actor._
import akka.pattern.{ask, pipe}
import akka.persistence._
import akka.stream.ActorMaterializer
import akka.util.Timeout
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{FlightsWithSplits, FlightsWithSplitsDiff}
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.Arrival
import org.slf4j.{Logger, LoggerFactory}
import scalapb.GeneratedMessage
import server.protobuf.messages.CrunchState._
import server.protobuf.messages.FlightsMessage.UniqueArrivalMessage
import services.SDate

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.language.postfixOps


object FlightsStateActor {
  def tempPitActorProps(pointInTime: SDateLike,
                        now: () => SDateLike,
                        queues: Map[Terminal, Seq[Queue]],
                        expireAfterMillis: Int,
                        legacyDataCutoff: SDateLike,
                        replayMaxCrunchStateMessages: Int): Props = {
    Props(new FlightsStateReadActor(now, expireAfterMillis, pointInTime.millisSinceEpoch, queues, legacyDataCutoff, replayMaxCrunchStateMessages))
  }
}

class FlightsStateActor(val now: () => SDateLike,
                        expireAfterMillis: Int,
                        queues: Map[Terminal, Seq[Queue]],
                        legacyDataCutoff: SDateLike,
                        replayMaxCrunchStateMessages: Int)
  extends PersistentActor with RecoveryActorLike with PersistentDrtActor[FlightsWithSplits] {

  import FlightsStateActor._

  override def persistenceId: String = "flights-state-actor"

  val snapshotInterval = 1000
  override val maybeSnapshotInterval: Option[Int] = Option(snapshotInterval)
  override val snapshotBytesThreshold: Int = Sizes.oneMegaByte
  override val recoveryStartMillis: MillisSinceEpoch = now().millisSinceEpoch

  implicit val ec: ExecutionContextExecutor = context.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer.create(context)
  implicit val timeout: Timeout = new Timeout(30 seconds)

  val log: Logger = LoggerFactory.getLogger(getClass)

  var state: FlightsWithSplits = FlightsWithSplits.empty

  var maybeUpdatesSubscriber: Option[ActorRef] = None

  val killActor: ActorRef = context.system.actorOf(Props(new RequestAndTerminateActor()))

  def initialState: FlightsWithSplits = FlightsWithSplits.empty

  def purgeExpired(): Unit = state = state.scheduledSince(expiryTimeMillis)

  def expiryTimeMillis: MillisSinceEpoch = now().addMillis(-1 * expireAfterMillis).millisSinceEpoch

  override def postRecoveryComplete(): Unit = {
    purgeExpired()
    log.info(s"Recovery complete. ${state.flights.size} flights")
  }

  override def processSnapshotMessage: PartialFunction[Any, Unit] = {
    case FlightsWithSplitsMessage(flightMessages) =>
      log.info(s"Processing snapshot message")
      setStateFromSnapshot(flightMessages)
  }

  def setStateFromSnapshot(flightMessages: Seq[FlightWithSplitsMessage]): Unit = {
    state = FlightsWithSplits(flightsFromMessages(flightMessages))
  }

  override def processRecoveryMessage: PartialFunction[Any, Unit] = {
    case diff: FlightsWithSplitsDiffMessage => handleDiffMessage(diff)
  }

  def handleDiffMessage(diff: FlightsWithSplitsDiffMessage): Unit = {
    state = state -- diff.removals.map(uniqueArrivalFromMessage)
    state = state ++ flightsFromMessages(diff.updates)
    logRecoveryState()
  }

  def logRecoveryState(): Unit = {
    log.debug(s"Recovery: state contains ${state.flights.size} flights")
  }

  override def stateToMessage: GeneratedMessage = FlightMessageConversion.flightsToMessage(state.flights.toMap.values)

  override def receiveCommand: Receive =
    historicRequests
      .orElse(standardRequests)
      .orElse(updatesRequests)
      .orElse(utilityRequests)
      .orElse(unexpected)

  private def utilityRequests: Receive = {
    case SetCrunchQueueActor(actor) =>
      log.info(s"Received crunch queue actor")
      maybeUpdatesSubscriber = Option(actor)

    case StreamInitialized => sender() ! Ack

    case StreamCompleted => log.info(s"Stream completed")

    case StreamFailure(t) => log.error(s"Stream failed", t)

    case _: SaveSnapshotSuccess =>
      log.info("Snapshot success")
      ackIfRequired()

    case SaveSnapshotFailure(md, cause) =>
      log.error(s"Save snapshot failure: $md", cause)
  }

  def unexpected: Receive = {
    case unexpected => log.error(s"Received unexpected message $unexpected")
  }

  def updatesRequests: PartialFunction[Any, Unit] = {
    case flightUpdates: FlightsWithSplitsDiff =>
      if (flightUpdates.nonEmpty)
        handleDiff(flightUpdates)
      else
        sender() ! Ack
  }

  def historicRequests: Receive = {
    case PointInTimeQuery(pitMillis, request) =>
      replyWithPointInTimeQuery(SDate(pitMillis), request)

    case request: DateRangeLike if SDate(request.to).isHistoricDate(now()) =>
      replyWithDayViewQuery(request)
  }

  def standardRequests: Receive = {
    case GetStateForDateRange(startMillis, endMillis) =>
      sender() ! state.window(startMillis, endMillis)

    case GetFlights(startMillis, endMillis) =>
      sender() ! state.window(startMillis, endMillis)

    case GetStateForTerminalDateRange(startMillis, endMillis, terminal) =>
      sender() ! state.forTerminal(terminal).window(startMillis, endMillis)

    case GetFlightsForTerminal(startMillis, endMillis, terminal) =>
      sender() ! state.forTerminal(terminal).window(startMillis, endMillis)

    case GetUpdatesSince(sinceMillis, startMillis, endMillis) =>
      sender() ! state.window(startMillis, endMillis).updatedSince(sinceMillis)
  }

  def replyWithDayViewQuery(message: DateRangeLike): Unit = {
    val pointInTime = SDate(message.to).addHours(4)
    replyWithPointInTimeQuery(pointInTime, message)
  }

  def replyWithPointInTimeQuery(pointInTime: SDateLike, message: DateRangeLike): Unit = {
    val tempActor = tempPointInTimeActor(pointInTime)
    killActor
      .ask(RequestAndTerminate(tempActor, message))
      .pipeTo(sender())
  }

  def tempPointInTimeActor(pointInTime: SDateLike): ActorRef =
    context.actorOf(tempPitActorProps(pointInTime, now, queues, expireAfterMillis, legacyDataCutoff, replayMaxCrunchStateMessages))

  def handleDiff(flightUpdates: FlightsWithSplitsDiff): Unit = {
    val (updatedState, updatedMinutes) = flightUpdates.applyTo(state, now().millisSinceEpoch)
    state = updatedState
    purgeExpired()

    if (updatedMinutes.nonEmpty)
      maybeUpdatesSubscriber.foreach(_ ! UpdatedMillis(updatedMinutes))

    val diffMsg = diffMessageForFlights(flightUpdates.flightsToUpdate, flightUpdates.arrivalsToRemove)
    persistAndMaybeSnapshotWithAck(diffMsg, Option((sender(), Ack)))
  }

  def diffMessageForFlights(updates: List[ApiFlightWithSplits],
                            removals: List[UniqueArrival]): FlightsWithSplitsDiffMessage = FlightsWithSplitsDiffMessage(
    createdAt = Option(now().millisSinceEpoch),
    removals = removals.map { ua =>
      UniqueArrivalMessage(Option(ua.number), Option(ua.terminal.toString), Option(ua.scheduled))
    },
    updates = updates.map(FlightMessageConversion.flightWithSplitsToMessage)
  )

  def uniqueArrivalFromMessage(uam: UniqueArrivalMessage): UniqueArrival =
    UniqueArrival(uam.getNumber, uam.getTerminalName, uam.getScheduled)
}
