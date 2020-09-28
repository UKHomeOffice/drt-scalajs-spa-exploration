package actors.daily

import java.util.UUID

import actors.PartitionedPortStateActor.GetUpdatesSince
import actors.daily.StreamingUpdatesLike.StopUpdates
import akka.NotUsed
import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.pattern.{AskTimeoutException, ask, pipe}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.Timeout
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared.Terminals.Terminal
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.SDate

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.language.postfixOps





object FlightUpdatesSupervisor {
  case class UpdateLastRequest(terminal: Terminal, day: MillisSinceEpoch, lastRequestMillis: MillisSinceEpoch)
}

class FlightUpdatesSupervisor(now: () => SDateLike,
                                                  terminals: List[Terminal],
                                                  updatesActorFactory: (Terminal, SDateLike) => Props) extends Actor {
  val log: Logger = LoggerFactory.getLogger(getClass)

  import FlightUpdatesSupervisor._

  implicit val ex: ExecutionContextExecutor = context.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer.create(context)
  implicit val timeout: Timeout = new Timeout(30 seconds)

  val cancellableTick: Cancellable = context.system.scheduler.schedule(10 seconds, 10 seconds, self, PurgeExpired)
  val killActor: ActorRef = context.system.actorOf(Props(new RequestAndTerminateActor()), s"flight-updates-supervisor-kill-actor-flight")

  var streamingUpdateActors: Map[(Terminal, MillisSinceEpoch), ActorRef] = Map[(Terminal, MillisSinceEpoch), ActorRef]()
  var lastRequests: Map[(Terminal, MillisSinceEpoch), MillisSinceEpoch] = Map[(Terminal, MillisSinceEpoch), MillisSinceEpoch]()

  override def postStop(): Unit = {
    log.warn("Actor stopped. Cancelling scheduled tick")
    cancellableTick.cancel()
    super.postStop()
  }

  def startUpdatesStream(terminal: Terminal,
                         day: SDateLike): ActorRef = streamingUpdateActors.get((terminal, day.millisSinceEpoch)) match {
    case Some(existing) => existing
    case None =>
      log.info(s"Starting supervised updates stream for $terminal / ${day.toISODateOnly}")
      val actor = context.system.actorOf(updatesActorFactory(terminal, day), s"flight-updates-actor-$terminal-${day.toISOString()}-${UUID.randomUUID().toString}")
      streamingUpdateActors = streamingUpdateActors + ((terminal, day.millisSinceEpoch) -> actor)
      lastRequests = lastRequests + ((terminal, day.millisSinceEpoch) -> now().millisSinceEpoch)
      actor
  }

  override def receive: Receive = {
    case PurgeExpired =>
      log.info("Received PurgeExpired")
      val expiredToRemove = lastRequests.collect {
        case (tm, lastRequest) if now().millisSinceEpoch - lastRequest > MilliTimes.oneMinuteMillis =>
          (tm, streamingUpdateActors.get(tm))
      }
      streamingUpdateActors = streamingUpdateActors -- expiredToRemove.keys
      lastRequests = lastRequests -- expiredToRemove.keys
      expiredToRemove.foreach {
        case ((terminal, day), Some(actor)) =>
          log.info(s"Shutting down streaming updates for $terminal/${SDate(day).toISODateOnly}")
          actor ! StopUpdates
        case _ =>
      }

    case GetUpdatesSince(sinceMillis, fromMillis, toMillis) =>
      val replyTo = sender()
      val terminalDays = terminalDaysForPeriod(fromMillis, toMillis)

      terminalsAndDaysUpdatesSource(terminalDays, sinceMillis)
        .runWith(Sink.fold(FlightsWithSplits.empty)(_ ++ _))
        .pipeTo(replyTo)

    case UpdateLastRequest(terminal, day, lastRequestMillis) =>
      lastRequests = lastRequests + ((terminal, day) -> lastRequestMillis)
  }

  def terminalDaysForPeriod(fromMillis: MillisSinceEpoch,
                            toMillis: MillisSinceEpoch): List[(Terminal, MillisSinceEpoch)] = {
    val daysMillis: Seq[MillisSinceEpoch] = (fromMillis to toMillis by MilliTimes.oneHourMillis)
      .map(m => SDate(m).getUtcLastMidnight.millisSinceEpoch)
      .distinct

    for {
      terminal <- terminals
      day <- daysMillis
    } yield (terminal, day)
  }

  def updatesActor(terminal: Terminal, day: MillisSinceEpoch): ActorRef =
    streamingUpdateActors.get((terminal, day)) match {
      case Some(existingActor) => existingActor
      case None => startUpdatesStream(terminal, SDate(day))
    }

  def terminalsAndDaysUpdatesSource(terminalDays: List[(Terminal, MillisSinceEpoch)],
                                    sinceMillis: MillisSinceEpoch): Source[FlightsWithSplits, NotUsed] =
    Source(terminalDays)
      .mapAsync(1) {
        case (terminal, day) =>
          updatesActor(terminal, day)
            .ask(GetAllUpdatesSince(sinceMillis))
            .mapTo[FlightsWithSplits]
            .map { container =>
              self ! UpdateLastRequest(terminal, day, now().millisSinceEpoch)
              container
            }
            .recoverWith {
              case t: AskTimeoutException =>
                log.warn(s"Timed out waiting for updates. Actor may have already been terminated", t)
                Future(FlightsWithSplits.empty)
              case t =>
                log.error(s"Failed to fetch updates from streaming updates actor: ${SDate(day).toISOString()}", t)
                Future(FlightsWithSplits.empty)
            }
      }
}