package services.crunch

import actors.{FlightsStateActor, GetPortState, MinuteLookups, MinutesActor, PartitionedPortStateActor, Sizes}
import akka.actor.Props
import akka.pattern.AskableActorRef
import akka.util.Timeout
import controllers.ArrivalGenerator
import drt.shared.CrunchApi.{CrunchMinute, DeskRecMinute, DeskRecMinutes, StaffMinute, StaffMinutes}
import drt.shared.FlightsApi.{FlightsWithSplits, FlightsWithSplitsDiff}
import drt.shared.Queues.EeaDesk
import drt.shared.Terminals.{T1, Terminal}
import drt.shared.{ApiFlightWithSplits, MilliTimes, PortState, SDateLike, TM, TQM, UniqueArrival}
import services.SDate
import services.crunch.deskrecs.GetFlights
import services.graphstages.Crunch.{LoadMinute, Loads}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class PartitionedPortStateSpec extends CrunchTestLike {
  implicit val timeout: Timeout = new Timeout(1 second)

  "Given an empty PartitionedPortState" >> {
    val scheduled = "2020-01-01T00:00"
    val now = () => SDate(scheduled)
    val expireAfterMillis = MilliTimes.oneDayMillis
    val queues = defaultAirportConfig.queuesByTerminal
    val flightsActor = system.actorOf(Props(new FlightsStateActor(None, Sizes.oneMegaByte, "flights", queues, now, expireAfterMillis)))
    val minuteLookups = MinuteLookups(system, now, expireAfterMillis, queues)
    val queuesActor = minuteLookups.queueMinutesActor(classOf[MinutesActor[CrunchMinute, TQM]])
    val staffActor = minuteLookups.staffMinutesActor(classOf[MinutesActor[StaffMinute, TM]])
    val ps: AskableActorRef = system.actorOf(Props(new PartitionedPortStateActor(flightsActor, queuesActor, staffActor, now)))

    "When I send it a flight and then ask for its flights" >> {
      val fws = flightsWithSplits(List(("BA1000", scheduled, T1)))
      val eventualAck = ps.ask(FlightsWithSplitsDiff(fws, List()))

      "Then I should see the flight I sent it" >> {
        val result = Await.result(eventualFlights(eventualAck, now, ps), 1 second)

        result === FlightsWithSplits(flightsToMap(now, fws))
      }
    }

    def flightsWithSplits(params: Iterable[(String, String, Terminal)]): List[ApiFlightWithSplits] = params.map { case (flightNumber, scheduled, terminal) =>
      val flight = ArrivalGenerator.arrival("BA1000", schDt = scheduled, terminal = T1)
      ApiFlightWithSplits(flight, Set())
    }.toList

    "When I send it 2 flights consecutively and then ask for its flights" >> {
      val scheduled2 = "2020-01-01T00:25"
      val fws1 = flightsWithSplits(List(("BA1000", scheduled, T1)))
      val fws2 = flightsWithSplits(List(("FR5000", scheduled2, T1)))
      val eventualAck = ps.ask(FlightsWithSplitsDiff(fws1, List())).flatMap(_ => ps.ask(FlightsWithSplitsDiff(fws2, List())))

      "Then I should see both flights I sent it" >> {
        val result = Await.result(eventualFlights(eventualAck, now, ps), 1 second)

        result === FlightsWithSplits(flightsToMap(now, fws1 ++ fws2))
      }
    }

    "When I send it a DeskRecMinute and then ask for its crunch minutes" >> {
      val lm = DeskRecMinute(T1, EeaDesk, now().millisSinceEpoch, 1, 2, 3, 4)

      val eventualAck = ps.ask(DeskRecMinutes(Seq(lm)))

      "Then I should a matching crunch minute" >> {
        val result = Await.result(eventualPortState(eventualAck, now, ps), 1 second)
        val expectedCm = CrunchMinute(T1, EeaDesk, now().millisSinceEpoch, 1, 2, 3, 4)

        result === Option(PortState(Seq(), Seq(expectedCm), Seq()))
      }
    }

    "When I send it two DeskRecMinutes consecutively and then ask for its crunch minutes" >> {
      val lm1 = DeskRecMinute(T1, EeaDesk, now().millisSinceEpoch, 1, 2, 3, 4)
      val lm2 = DeskRecMinute(T1, EeaDesk, now().addMinutes(1).millisSinceEpoch, 2, 3, 4, 5)

      val eventualAck = ps.ask(DeskRecMinutes(Seq(lm1))).flatMap( _ => ps.ask(DeskRecMinutes(Seq(lm2))))

      "Then I should a matching crunch minute" >> {
        val result = Await.result(eventualPortState(eventualAck, now, ps), 1 second)
        val expectedCms = Seq(
          CrunchMinute(T1, EeaDesk, now().millisSinceEpoch, 1, 2, 3, 4),
          CrunchMinute(T1, EeaDesk, now().addMinutes(1).millisSinceEpoch, 2, 3, 4, 5))

        result === Option(PortState(Seq(), expectedCms, Seq()))
      }
    }

    "When I send it a StaffMinute and then ask for its staff minutes" >> {
      val sm = StaffMinute(T1, now().millisSinceEpoch, 1, 2, 3)

      val eventualAck = ps.ask(StaffMinutes(Seq(sm)))

      "Then I should a matching staff minute" >> {
        val result = Await.result(eventualPortState(eventualAck, now, ps), 1 second)

        result === Option(PortState(Seq(), Seq(), Seq(sm)))
      }
    }

    "When I send it two StaffMinutes consecutively and then ask for its staff minutes" >> {
      val sm1 = StaffMinute(T1, now().millisSinceEpoch, 1, 2, 3)
      val sm2 = StaffMinute(T1, now().addMinutes(1).millisSinceEpoch, 1, 2, 3)

      val eventualAck = ps.ask(StaffMinutes(Seq(sm1))).flatMap(_ => ps.ask(StaffMinutes(Seq(sm2))))

      "Then I should a matching staff minute" >> {
        val result = Await.result(eventualPortState(eventualAck, now, ps), 1 second)

        result === Option(PortState(Seq(), Seq(), Seq(sm1, sm2)))
      }
    }
  }

  def eventualFlights(eventualAck: Future[Any], now: () => SDateLike, ps: AskableActorRef): Future[FlightsWithSplits] = eventualAck.flatMap { _ =>
    val startMillis = now().getLocalLastMidnight.millisSinceEpoch
    val endMillis = now().getLocalNextMidnight.millisSinceEpoch
    ps.ask(GetFlights(startMillis, endMillis)).mapTo[FlightsWithSplits]
  }

  def eventualPortState(eventualAck: Future[Any], now: () => SDateLike, ps: AskableActorRef): Future[Option[PortState]] = eventualAck.flatMap { _ =>
    val startMillis = now().getLocalLastMidnight.millisSinceEpoch
    val endMillis = now().getLocalNextMidnight.millisSinceEpoch
    ps.ask(GetPortState(startMillis, endMillis)).mapTo[Option[PortState]]
  }

  def flightsToMap(now: () => SDateLike, flights: Seq[ApiFlightWithSplits]): Map[UniqueArrival, ApiFlightWithSplits] = flights.map { fws1 =>
    (fws1.unique -> fws1.copy(lastUpdated = Option(now().millisSinceEpoch)))
  }.toMap
}
