package actors.minutes

import actors.PartitionedPortStateActor.{GetStateForDateRange, GetStateForTerminalDateRange, PointInTimeQuery}
import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import actors.minutes.MinutesActorLike.{MinutesLookup, MinutesUpdate, ProcessNextUpdateRequest}
import akka.NotUsed
import akka.actor.{Actor, ActorRef}
import akka.pattern.pipe
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import drt.shared.CrunchApi.{MillisSinceEpoch, MinuteLike, MinutesContainer}
import drt.shared.Terminals.Terminal
import drt.shared.{MilliTimes, SDateLike, Terminals, WithTimeAccessor}
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.graphstages.Crunch

import scala.collection.immutable
import scala.concurrent.{ExecutionContextExecutor, Future}


object MinutesActorLike {
  type MinutesLookup[A, B <: WithTimeAccessor] = (Terminals.Terminal, SDateLike, Option[MillisSinceEpoch]) => Future[Option[MinutesContainer[A, B]]]
  type MinutesUpdate[A, B <: WithTimeAccessor] = (Terminals.Terminal, SDateLike, MinutesContainer[A, B]) => Future[MinutesContainer[A, B]]

  case object ProcessNextUpdateRequest

}

abstract class MinutesActorLike[A, B <: WithTimeAccessor](now: () => SDateLike,
                                                          terminals: Iterable[Terminal],
                                                          lookup: MinutesLookup[A, B],
                                                          lookupLegacy: MinutesLookup[A, B],
                                                          updateMinutes: MinutesUpdate[A, B]) extends Actor {
  implicit val dispatcher: ExecutionContextExecutor = context.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer.create(context)

  val log: Logger = LoggerFactory.getLogger(getClass)

  var updateRequestsQueue: List[(ActorRef, MinutesContainer[A, B])] = List()
  var processingRequest: Boolean = false

  def isHistoric(date: SDateLike): Boolean = MilliTimes.isHistoric(now, date)

  override def receive: Receive = {
    case StreamInitialized => sender() ! Ack

    case StreamCompleted => log.info(s"Stream completed")

    case StreamFailure(t) => log.error(s"Stream failed", t)

    case PointInTimeQuery(pit, GetStateForDateRange(startMillis, endMillis)) =>
      handleAllTerminalLookupsStream(startMillis, endMillis, Option(pit)).pipeTo(sender())

    case PointInTimeQuery(pit, GetStateForTerminalDateRange(startMillis, endMillis, terminal)) =>
      handleLookups(terminal, SDate(startMillis), SDate(endMillis), Option(pit)).pipeTo(sender())

    case GetStateForDateRange(startMillis, endMillis) =>
      handleAllTerminalLookupsStream(startMillis, endMillis, None).pipeTo(sender())

    case GetStateForTerminalDateRange(startMillis, endMillis, terminal) =>
      handleLookups(terminal, SDate(startMillis), SDate(endMillis), None).pipeTo(sender())

    case container: MinutesContainer[A, B] =>
      log.info(s"Adding ${container.minutes.size} minutes to requests queue")
      updateRequestsQueue = (sender(), container) :: updateRequestsQueue
      self ! ProcessNextUpdateRequest

    case ProcessNextUpdateRequest =>
      if (!processingRequest) {
        updateRequestsQueue match {
          case (replyTo, container) :: tail =>
            handleUpdatesAndAck(container, replyTo)
            updateRequestsQueue = tail
          case Nil =>
            log.debug("Update requests queue is empty. Nothing to do")
        }
      }

    case u => log.warn(s"Got an unexpected message: $u")
  }

  def handleAllTerminalLookupsStream(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch, maybePit: Option[MillisSinceEpoch]): Future[MinutesContainer[A, B]] = {
    val eventualMinutesForAllTerminals = Source(terminals.toList)
      .mapAsync(1) { terminal =>
        handleLookups(terminal, SDate(startMillis), SDate(endMillis), maybePit)
      }
    combineEventualMinutesContainersStream(eventualMinutesForAllTerminals)
  }

  def handleUpdatesAndAck(container: MinutesContainer[A, B],
                          replyTo: ActorRef): Future[Option[MinutesContainer[A, B]]] = {
    processingRequest = true
    val eventualUpdatesDiff = updateByTerminalDayAndGetDiff(container)
    eventualUpdatesDiff.onComplete { _ =>
      processingRequest = false
      replyTo ! Ack
      self ! ProcessNextUpdateRequest
    }
    eventualUpdatesDiff
  }

  def handleLookups(terminal: Terminal,
                    start: SDateLike,
                    end: SDateLike,
                    maybePointInTime: Option[MillisSinceEpoch]): Future[MinutesContainer[A, B]] = {
    val eventualContainerWithBookmarks: Future[immutable.Seq[MinutesContainer[A, B]]] =
      Source(Crunch.utcDaysInPeriod(start, end).toList)
        .mapAsync(1) {
          case day if isHistoric(day) =>
            log.debug(s"${day.toISOString()} is historic. Will use secondary source if primary data doesn't exist")
            handleLookup(lookup(terminal, day, maybePointInTime), Option(() => lookupLegacy(terminal, day, maybePointInTime))).map(r => (day, r))
          case day =>
            log.debug(s"${day.toISOString()} is live. Look up live data from terminal/day actor")
            handleLookup(lookup(terminal, day, maybePointInTime), None).map(r => (day, r))
        }
        .collect {
          case (_, Some(container)) => container.window(start, end)
          case (day, None) =>
            log.info(s"No minutes found for for ${day.toISOString()}")
            MinutesContainer.empty[A, B]
        }
        .fold(MinutesContainer[A, B](Seq())) {
          case (soFarContainer, dayContainer) => soFarContainer ++ dayContainer
        }
        .runWith(Sink.seq)

    eventualContainerWithBookmarks.map {
      case cs if cs.nonEmpty => cs.reduce(_ ++ _)
      case _ => MinutesContainer.empty[A, B]
    }
  }

  def handleLookup(eventualMaybeResult: Future[Option[MinutesContainer[A, B]]],
                   maybeFallback: Option[() => Future[Option[MinutesContainer[A, B]]]]): Future[Option[MinutesContainer[A, B]]] =
    eventualMaybeResult.flatMap {
      case Some(minutes) =>
        log.debug(s"Got some minutes. Sending them")
        Future(Option(minutes))
      case None =>
        maybeFallback match {
          case None =>
            log.debug(s"Got no minutes. Sending None")
            Future(None)
          case Some(fallback) =>
            log.info(s"Got no minutes. Querying the fallback")
            handleLookup(fallback(), None)
        }
    }

  def updateByTerminalDayAndGetDiff(container: MinutesContainer[A, B]): Future[Option[MinutesContainer[A, B]]] = {
    val eventualUpdatedMinutesDiff = Source(groupByTerminalAndDay(container)).mapAsync(1) {
      case ((terminal, day), terminalDayMinutes) => handleUpdateAndGetDiff(terminal, day, terminalDayMinutes)
    }
    combineEventualMinutesContainersStream(eventualUpdatedMinutesDiff).map(Option(_))
  }

  def groupByTerminalAndDay(container: MinutesContainer[A, B]): Map[(Terminal, SDateLike), Iterable[MinuteLike[A, B]]] =
    container.minutes
      .groupBy(simMin => (simMin.terminal, SDate(simMin.minute).getUtcLastMidnight))

  private def combineEventualMinutesContainersStream(eventualUpdatedMinutesDiff: Source[MinutesContainer[A, B], NotUsed]): Future[MinutesContainer[A, B]] = {
    eventualUpdatedMinutesDiff
      .fold(MinutesContainer.empty[A, B])(_ ++ _)
      .runWith(Sink.seq)
      .map {
        case containers if containers.nonEmpty => containers.reduce(_ ++ _)
        case _ => MinutesContainer.empty[A, B]
      }
      .recoverWith {
        case t =>
          log.error("Failed to combine containers", t)
          Future(MinutesContainer.empty[A, B])
      }
  }

  def handleUpdateAndGetDiff(terminal: Terminal,
                             day: SDateLike,
                             minutesForDay: Iterable[MinuteLike[A, B]]): Future[MinutesContainer[A, B]] =
    updateMinutes(terminal, day, MinutesContainer(minutesForDay))

}
