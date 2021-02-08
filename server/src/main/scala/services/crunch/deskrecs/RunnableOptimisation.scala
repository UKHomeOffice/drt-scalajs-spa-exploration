package services.crunch.deskrecs

import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import akka.actor.ActorRef
import akka.stream.{ClosedShape, KillSwitches, OverflowStrategy, UniqueKillSwitch}
import akka.stream.scaladsl.GraphDSL.Implicits
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source, SourceQueueWithComplete}
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.{PortStateQueueMinutes, SDateLike, SimulationMinutes}
import drt.shared.dates.{DateLike, LocalDate, UtcDate}
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.graphstages.Crunch.{CrunchRequest, europeLondonTimeZone}

import scala.collection.immutable.NumericRange
import scala.concurrent.ExecutionContext

object RunnableOptimisation {
  val log: Logger = LoggerFactory.getLogger(getClass)
  val timeLogger: TimeLogger = TimeLogger("Optimisation", 1000, log)

  type CrunchRequestsToQueueMinutes = Implicits.PortOps[CrunchRequest] => Implicits.PortOps[PortStateQueueMinutes]

  case class CrunchRequest(localDate: LocalDate, offsetMinutes: Int, durationMinutes: Int) extends Ordered[CrunchRequest] {
    lazy val start: SDateLike = SDate(localDate).addMinutes(offsetMinutes)
    lazy val end: SDateLike = start.addMinutes(durationMinutes)
    lazy val minutesInMillis: NumericRange[MillisSinceEpoch] = start.millisSinceEpoch until end.millisSinceEpoch by 60000

    override def compare(that: CrunchRequest): Int =
      if (localDate < that.localDate) -1
      else if (localDate > that.localDate) 1
      else 0
  }

  object CrunchRequest {
    def apply(millis: MillisSinceEpoch, offsetMinutes: Int, durationMinutes: Int): CrunchRequest = {
      val midnight = SDate(millis, europeLondonTimeZone)
        .addMinutes(-1 * offsetMinutes)
        .getLocalLastMidnight
      val localDate = midnight
        .toLocalDate

      CrunchRequest(localDate, offsetMinutes, durationMinutes)
    }
  }

  def createGraph(deskRecsSinkActor: ActorRef, crunchRequestsToQueueMinutes: CrunchRequestsToQueueMinutes)
                 (implicit ec: ExecutionContext): RunnableGraph[(SourceQueueWithComplete[CrunchRequest], UniqueKillSwitch)] = {

    val crunchRequestSource = Source.queue[CrunchRequest](1, OverflowStrategy.backpressure)
    val deskRecsSink = Sink.actorRefWithAck(deskRecsSinkActor, StreamInitialized, Ack, StreamCompleted, StreamFailure)
    val ks = KillSwitches.single[PortStateQueueMinutes]

    val graph = GraphDSL.create(crunchRequestSource, ks)((_, _)) {
      implicit builder =>
        (crunchRequests, killSwitch) =>
          crunchRequestsToQueueMinutes(crunchRequests.out) ~> killSwitch ~> deskRecsSink
          ClosedShape
    }

    RunnableGraph.fromGraph(graph)
  }
}
