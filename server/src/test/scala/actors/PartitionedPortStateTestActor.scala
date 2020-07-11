package actors

import actors.acking.AckingReceiver.{Ack, StreamFailure}
import actors.minutes.{QueueMinutesActor, StaffMinutesActor}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import drt.shared.CrunchApi.{CrunchMinute, MinutesContainer, StaffMinute}
import drt.shared.FlightsApi.{FlightsWithSplits, FlightsWithSplitsDiff}
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared._
import services.SDate
import services.crunch.deskrecs.GetStateForDateRange

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext

object PartitionedPortStateTestActor {
  def apply(testProbe: TestProbe, flightsActor: ActorRef, now: () => SDateLike, airportConfig: AirportConfig)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    val lookups = MinuteLookups(system, now, MilliTimes.oneDayMillis, airportConfig.queuesByTerminal)
    val queuesActor = lookups.queueMinutesActor
    val staffActor = lookups.staffMinutesActor
    system.actorOf(Props(new PartitionedPortStateTestActor(testProbe.ref, flightsActor, queuesActor, staffActor, now, airportConfig.queuesByTerminal)))
  }
}

class PartitionedPortStateTestActor(probe: ActorRef,
                                    flightsActor: ActorRef,
                                    queuesActor: ActorRef,
                                    staffActor: ActorRef,
                                    now: () => SDateLike,
                                    queues: Map[Terminal, Seq[Queue]]) extends PartitionedPortStateActor(flightsActor, queuesActor, staffActor, now, queues, InMemoryStreamingJournal, SDate("1970-01-01"), PartitionedPortStateActor.tempLegacyActorProps) {
  var state: PortState = PortState.empty

  override def receive: Receive = processMessage orElse {
    case StreamFailure(_) =>
      log.info(s"Stream shut down")

    case ps: PortState =>
      val replyTo = sender()
      log.info(s"Setting initial port state")
      state = ps
      flightsActor.ask(FlightsWithSplitsDiff(ps.flights.values.toList, List())).flatMap { _ =>
        queuesActor.ask(MinutesContainer(ps.crunchMinutes.values)).flatMap { _ =>
          staffActor.ask(MinutesContainer(ps.staffMinutes.values))
        }
      }.foreach(_ => replyTo ! Ack)
  }

  override def askThenAck(message: Any, replyTo: ActorRef, actor: ActorRef): Unit = {
    actor.ask(message).foreach { _ =>
      message match {
        case flightsWithSplitsDiff@FlightsWithSplitsDiff(_, _) if flightsWithSplitsDiff.nonEmpty =>
          actor.ask(GetStateForDateRange(0L, Long.MaxValue)).mapTo[Option[FlightsWithSplits]].foreach {
            case None => sendStateToProbe()
            case Some(FlightsWithSplits(flights)) =>
              val updatedFlights: SortedMap[UniqueArrival, ApiFlightWithSplits] = SortedMap[UniqueArrival, ApiFlightWithSplits]() ++ flights
              state = state.copy(flights = updatedFlights)
              sendStateToProbe()
          }
        case mc: MinutesContainer[_, _] =>
          val minuteMillis = mc.minutes.map(_.minute)
          mc.minutes.headOption match {
            case None => sendStateToProbe()
            case Some(minuteLike) if minuteLike.toMinute.isInstanceOf[CrunchMinute] =>
              actor.ask(GetStateForDateRange(minuteMillis.min, minuteMillis.max)).mapTo[MinutesContainer[CrunchMinute, TQM]]
                .foreach { container =>
                  val updatedMinutes = state.crunchMinutes ++ container.minutes.map(ml => (ml.key, ml.toMinute))
                  state = state.copy(crunchMinutes = updatedMinutes)
                  sendStateToProbe()
                }
            case Some(minuteLike) if minuteLike.toMinute.isInstanceOf[StaffMinute] =>
              actor.ask(GetStateForDateRange(minuteMillis.min, minuteMillis.max)).mapTo[MinutesContainer[StaffMinute, TM]]
                .foreach { container =>
                  val updatedMinutes = state.staffMinutes ++ container.minutes.map(ml => (ml.key, ml.toMinute))
                  state = state.copy(staffMinutes = updatedMinutes)
                  sendStateToProbe()
                }
          }
        case _ => sendStateToProbe()
      }
      replyTo ! Ack
    }
  }

  def sendStateToProbe(): Unit = {
    probe ! state
  }
}
