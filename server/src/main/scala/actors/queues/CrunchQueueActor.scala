package actors.queues

import actors.{SetDaysQueueSource, StreamingJournalLike}
import actors.acking.AckingReceiver.Ack
import actors.queues.CrunchQueueActor.{ReadyToEmit, Tick, UpdatedMillis}
import akka.actor.Cancellable
import akka.persistence._
import akka.stream.Supervision.Stop
import akka.stream.scaladsl.SourceQueueWithComplete
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.SDateLike
import org.slf4j.{Logger, LoggerFactory}
import server.protobuf.messages.CrunchState.DaysSnapshotMessage
import services.SDate
import services.graphstages.Crunch

import scala.collection.immutable.SortedSet
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._


object CrunchQueueActor {

  case object Tick

  case object ReadyToEmit

  case class UpdatedMillis(millis: Iterable[MillisSinceEpoch])

}

class CrunchQueueActor(val journalType: StreamingJournalLike, crunchOffsetMinutes: Int) extends PersistentActor {
  val log: Logger = LoggerFactory.getLogger(getClass)

  override val persistenceId: String = "crunch-queue"

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  val cancellableTick: Cancellable = context.system.scheduler.schedule(1 second, 1 second, self, Tick)

  val crunchStartFn: SDateLike => SDateLike = Crunch.crunchStartWithOffset(crunchOffsetMinutes)

  var maybeDaysQueueSource: Option[SourceQueueWithComplete[MillisSinceEpoch]] = None
  var queuedDays: SortedSet[MillisSinceEpoch] = SortedSet[MillisSinceEpoch]()
  var readyToEmit: Boolean = false

  override def postStop(): Unit = {
    log.info(s"I've stopped so I'll cancel my tick now")
    cancellableTick.cancel()
    super.postStop()
  }

  override def receiveRecover: Receive = {
    case SnapshotOffer(SnapshotMetadata(_, sequenceNr, _), DaysSnapshotMessage(days)) =>
      log.info(s"Restoring queue to ${days.size} days")
      queuedDays = queuedDays ++ days
      deleteSnapshot(sequenceNr)

    case RecoveryCompleted =>
      log.info(s"Recovery completed. ${queuedDays.map(m => SDate(m, Crunch.europeLondonTimeZone).toISODateOnly).mkString(", ")}")
      emitNextDayIfReady()

    case u =>
      log.warn(s"Unexpected recovery message: $u")
  }

  override def receiveCommand: Receive = {
    case Tick =>
      log.debug(s"Got a tick. Will try to emit if I'm able to")
      emitNextDayIfReady()

    case ReadyToEmit =>
      readyToEmit = true
      log.debug(s"Got a ReadyToEmit. Will emit if I have something in the queue")
      emitNextDayIfReady()

    case SetDaysQueueSource(source) =>
      log.info(s"Received daysQueueSource")
      maybeDaysQueueSource = Option(source)
      readyToEmit = true
      emitNextDayIfReady()

    case UpdatedMillis(millis) =>
      log.debug(s"Received ${millis.size} UpdatedMillis")
      val days = uniqueDays(millis)
      updateState(days)
      sender() ! Ack
      emitNextDayIfReady()

    case Stop =>
      log.info("Being asked to stop. Taking snapshot of the current queue")
      saveSnapshot(DaysSnapshotMessage(queuedDays.toList))

    case _: SaveSnapshotSuccess =>
      log.info(s"Successfully saved snapshot. Shutting down now")
      context.stop(self)

    case _: DeleteSnapshotSuccess =>
      log.info(s"Successfully deleted snapshot")

    case u =>
      log.warn(s"Unexpected message: ${u.getClass}")
  }

  def uniqueDays(millis: Iterable[MillisSinceEpoch]): Set[MillisSinceEpoch] =
    millis.map(m => crunchStartFn(SDate(m)).millisSinceEpoch).toSet

  def emitNextDayIfReady(): Unit = if (readyToEmit)
    queuedDays.headOption match {
      case Some(day) =>
        log.debug(s"Emitting ${SDate(day, Crunch.europeLondonTimeZone).toISODateOnly}")
        readyToEmit = false
        maybeDaysQueueSource.foreach { sourceQueue =>
          sourceQueue.offer(day).foreach { _ =>
            self ! ReadyToEmit
          }
        }
        queuedDays = queuedDays.drop(1)
      case None =>
        log.debug(s"Nothing in the queue to emit")
    }

  def updateState(days: Iterable[MillisSinceEpoch]): Unit = {
    queuedDays = queuedDays ++ days
  }

}
