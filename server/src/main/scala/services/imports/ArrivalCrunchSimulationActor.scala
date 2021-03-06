package services.imports

import actors.PartitionedPortStateActor.GetFlights
import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamInitialized}
import actors.persistent.staffing.GetState
import akka.actor.{Actor, ActorLogging, PoisonPill}
import akka.pattern.pipe
import akka.stream.scaladsl.Source
import drt.shared.CrunchApi.DeskRecMinutes
import drt.shared.FlightsApi.FlightsWithSplits

import scala.concurrent.{ExecutionContextExecutor, Promise}
import scala.util.Try

class ArrivalCrunchSimulationActor(fws: FlightsWithSplits) extends Actor with ActorLogging {
  implicit val dispatcher: ExecutionContextExecutor = context.dispatcher

  var promisedResult: Promise[DeskRecMinutes] = Promise[DeskRecMinutes]

  override def receive: Receive = {
    case GetFlights(_, _) =>
      sender() ! Source(List(fws))

    case GetState =>
      val replyTo = sender()
      promisedResult.future.pipeTo(replyTo)

    case m: DeskRecMinutes =>
      promisedResult.complete(Try(m))

    case StreamInitialized =>
      sender() ! Ack

    case StreamCompleted =>
      self ! PoisonPill

    case unexpected =>
      log.warning(s"Got and unexpected message $unexpected")
  }
}
