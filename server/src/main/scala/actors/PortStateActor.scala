package actors

import java.util.UUID

import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import akka.actor.{Actor, Props}
import akka.pattern.AskableActorRef
import akka.util.Timeout
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{FlightsWithSplits, TerminalName}
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.crunch.deskrecs.GetFlights
import services.graphstages.Crunch
import services.graphstages.Crunch.{LoadMinute, Loads}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.postfixOps


object PortStateActor {
  def props(liveStateActor: AskableActorRef, forecastStateActor: AskableActorRef, airportConfig: AirportConfig, expireAfterMillis: Long, now: () => SDateLike, liveDaysAhead: Int): Props =
    Props(new PortStateActor(liveStateActor, forecastStateActor, airportConfig, expireAfterMillis, now, 2))
}

class PortStateActor(liveStateActor: AskableActorRef,
                     forecastStateActor: AskableActorRef,
                     airportConfig: AirportConfig,
                     expireAfterMillis: Long,
                     now: () => SDateLike,
                     liveDaysAhead: Int) extends Actor {
  val log: Logger = LoggerFactory.getLogger(getClass)

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  implicit val timeout: Timeout = new Timeout(15 seconds)

  val state: PortStateMutable = PortStateMutable.empty

  var maybeCrunchActor: Option[AskableActorRef] = None
  var awaitingCrunchActor: Boolean = false
  var maybeSimActor: Option[AskableActorRef] = None
  var awaitingSimulationActor: Boolean = false

  override def receive: Receive = {
    case SetCrunchActor(crunchActor) =>
      log.info(s"Received crunchSourceActor")
      maybeCrunchActor = Option(crunchActor)

    case SetSimulationActor(simActor) =>
      log.info(s"Received simulationSourceActor")
      maybeSimActor = Option(simActor)

    case ps: PortState =>
      log.info(s"Received initial PortState")
      state.crunchMinutes ++= ps.crunchMinutes
      state.staffMinutes ++= ps.staffMinutes
      state.flights ++= ps.flights
      log.info(s"Finished setting state")

    case StreamInitialized => sender() ! Ack

    case StreamCompleted => log.info(s"Stream completed")

    case StreamFailure(t) => log.error(s"Stream failed", t)

    case updates: PortStateMinutes =>
      val uuid = s"${UUID.randomUUID()}-${updates.getClass}"
      splitDiffAndSend(updates.applyTo(state, nowMillis), uuid)

    case GetState =>
      log.debug(s"Received GetState request. Replying with PortState containing ${state.crunchMinutes.count} crunch minutes")
      sender() ! Option(state.immutable)

    case GetPortState(start, end) =>
      log.debug(s"Received GetPortState Request from ${SDate(start).toISOString()} to ${SDate(end).toISOString()}")
      sender() ! stateForPeriod(start, end)

    case GetPortStateForTerminal(start, end, terminalName) =>
      log.debug(s"Received GetPortState Request from ${SDate(start).toISOString()} to ${SDate(end).toISOString()}")
      sender() ! stateForPeriodForTerminal(start, end, terminalName)

    case GetUpdatesSince(millis, start, end) =>
      val updates: Option[PortStateUpdates] = state.updates(millis, start, end)
      sender() ! updates

    case GetFlights(startMillis, endMillis) =>
      val start = SDate(startMillis)
      val end = SDate(endMillis)
      log.info(s"Got request for flights between ${start.toISOString()} - ${end.toISOString()}")
      val flightsToSend = state.flights.range(start, end).values.toList
      sender() ! FlightsWithSplits(flightsToSend, List())

    case unexpected => log.warn(s"Got unexpected: $unexpected")
  }

  def stateForPeriod(start: MillisSinceEpoch, end: MillisSinceEpoch): Option[PortState] = Option(state.window(SDate(start), SDate(end)))

  def stateForPeriodForTerminal(start: MillisSinceEpoch, end: MillisSinceEpoch, terminalName: TerminalName): Option[PortState] = Option(state.windowWithTerminalFilter(SDate(start), SDate(end), Seq(terminalName)))

  val flightMinutesBuffer: mutable.Set[MillisSinceEpoch] = mutable.Set[MillisSinceEpoch]()
  val loadMinutesBuffer: mutable.Map[TQM, LoadMinute] = mutable.Map[TQM, LoadMinute]()

  def splitDiffAndSend(diff: PortStateDiff, uuid: String): Unit = {
    val replyTo = sender()

    log.debug(s"Processing incoming PortStateMinutes: $uuid")

    splitDiff(diff) match {
      case (live, forecast) =>
        val asks = List(
          (liveStateActor, live) -> "live crunch persistence request failed",
          (forecastStateActor, forecast) -> "forecast crunch persistence request failed")

        handleCrunchRequest(diff)
        handleSimulationRequest(diff)

        Future
          .sequence(asks.map {
            case ((actor, question), failureMsg) => askAndLogOnFailure(actor, question, failureMsg)
          })
          .recover { case t => log.error("A future failed", t) }
          .onComplete { _ =>
            log.debug(s"Sending Ack for $uuid")
            replyTo ! Ack
          }
    }
  }

  private def handleSimulationRequest(diff: PortStateDiff) = {
    if (maybeSimActor.isDefined && diff.crunchMinuteUpdates.nonEmpty)
      if (!awaitingSimulationActor) {
        maybeSimActor.get
          .ask(Loads(crunchMinutesToLoads(diff).toSeq))(new Timeout(10 minutes))
          .recover {
            case t => log.error("Error sending loads to simulate", t)
          }
          .onComplete { _ =>
            awaitingSimulationActor = false
            loadMinutesBuffer.clear()
          }
        awaitingSimulationActor = true
      } else loadMinutesBuffer ++= crunchMinutesToLoads(diff).map(lm => (lm.uniqueId, lm))
  }

  private def handleCrunchRequest(diff: PortStateDiff) = {
    if (maybeCrunchActor.isDefined && diff.flightMinuteUpdates.nonEmpty) {
      if (!awaitingCrunchActor) {
        maybeCrunchActor.get
          .ask(diff.flightMinuteUpdates.toList)(new Timeout(10 minutes))
          .recover {
            case t => log.error("Error sending minutes to crunch", t)
          }
          .onComplete { _ =>
            awaitingCrunchActor = false
            flightMinutesBuffer.clear()
          }
        awaitingCrunchActor = true
      }
      else flightMinutesBuffer ++= diff.flightMinuteUpdates
    }
  }

  private def askAndLogOnFailure[A](actor: AskableActorRef, question: Any, msg: String): Future[Any] = actor
    .ask(question)
    .recover {
      case t => log.error(msg, t)
    }

  private def crunchMinutesToLoads(diff: PortStateDiff): Iterable[LoadMinute] = diff.crunchMinuteUpdates.map {
    case (_, cm) => LoadMinute(cm)
  }

  private def splitDiff(diff: PortStateDiff): (PortStateDiff, PortStateDiff) = {
    val liveDiff = diff.window(liveStart(now).millisSinceEpoch, liveEnd(now, liveDaysAhead).millisSinceEpoch)
    val forecastDiff = diff.window(forecastStart(now).millisSinceEpoch, forecastEnd(now).millisSinceEpoch)
    (liveDiff, forecastDiff)
  }

  private def nowMillis: MillisSinceEpoch = now().millisSinceEpoch

  def liveStart(now: () => SDateLike): SDateLike = Crunch.getLocalLastMidnight(now()).addDays(-1)

  def liveEnd(now: () => SDateLike, liveStateDaysAhead: Int): SDateLike = Crunch.getLocalNextMidnight(now()).addDays(liveStateDaysAhead)

  def forecastEnd(now: () => SDateLike): SDateLike = Crunch.getLocalNextMidnight(now()).addDays(360)

  def forecastStart(now: () => SDateLike): SDateLike = Crunch.getLocalNextMidnight(now())
}
