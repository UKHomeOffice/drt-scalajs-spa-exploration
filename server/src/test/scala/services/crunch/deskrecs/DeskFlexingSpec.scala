package services.crunch.deskrecs

import drt.shared.Queues._
import org.specs2.mutable.Specification
import services.graphstages.Crunch
import services.graphstages.Crunch.crunchLoadsWithFlexing
import services.{OptimizerConfig, OptimizerCrunchResult, TryCrunch}

import scala.util.{Failure, Success, Try}

class DeskFlexingSpec extends Specification {
  val totalDesks = 20
  val eeaMinDesks = 1
  val roWMinDesks = 2
  val ftMinDesks = 3
  val egateMinDesks = 4

  val totalDesks24: List[Int] = List.fill(24)(totalDesks)
  val eeaMinDesks24: List[Int] = List.fill(24)(eeaMinDesks)
  val roWMinDesks24: List[Int] = List.fill(24)(roWMinDesks)
  val ftMinDesks24: List[Int] = List.fill(24)(ftMinDesks)
  val egateMinDesks24: List[Int] = List.fill(24)(egateMinDesks)

  val minDesks: Map[Queue, List[Int]] = Map(
    FastTrack -> ftMinDesks24,
    NonEeaDesk -> roWMinDesks24,
    EeaDesk -> eeaMinDesks24,
    EGate -> egateMinDesks24
  )

  val slas: Map[Queue, Int] = List(FastTrack, NonEeaDesk, EeaDesk, EGate).map(q => (q, 20)).toMap

  class MockWithObserver {
    var observedMaxDesks: List[List[Int]] = List()

    val mockDeskRecs: (Seq[Double], Seq[Int], Seq[Int], OptimizerConfig) => Try[OptimizerCrunchResult] =
      (_: Seq[Double], minDesks: Seq[Int], maxDesks: Seq[Int], _: OptimizerConfig) => {
        observedMaxDesks = observedMaxDesks ++ List(maxDesks.toList)
        Success(OptimizerCrunchResult(minDesks.toIndexedSeq, minDesks))
      }
  }

  val flexedQueuesPriority: List[Queue] = List(EeaDesk, NonEeaDesk, FastTrack)

  s"Given $totalDesks desks, and a set of minimums for RoW ($roWMinDesks) " >> {
    "When I ask for max desks for EEA " >> {
      s"I should see $totalDesks minus the RoW min desks passed in" >> {
        val eeaMaxDesks = totalDesks24.zip(roWMinDesks24).map { case (a, b) => a - b }
        val expectedEeaMaxDesks = List.fill(24)(totalDesks - roWMinDesks)

        eeaMaxDesks === expectedEeaMaxDesks
      }
    }
  }

  s"Given workload for EEA & RoW, their minimum desks ($eeaMinDesks & $roWMinDesks), SLAs, and $totalDesks terminal's total desks " >> {
    "When I ask for desk recommendations using a mock optimiser " >> {
      s"I should observe the max desks as EEA: ${totalDesks - roWMinDesks}, RoW: ${totalDesks - eeaMinDesks}" >> {

        val observer = new MockWithObserver

        crunchLoadsWithFlexing(mockLoads(List(EeaDesk, NonEeaDesk)), totalDesks, minDesks, Map(), slas, flexedQueuesPriority, observer.mockDeskRecs)

        val expectedMaxEea = totalDesks24.map(_ - roWMinDesks)
        val expectedMaxRoW = totalDesks24.map(_ - eeaMinDesks)

        val expectedObservedMaxDesks = List(expectedMaxEea, expectedMaxRoW)

        observer.observedMaxDesks === expectedObservedMaxDesks
      }
    }
  }

  s"Given workload for EEA & RoW, FastTrack & EGates " >> {
    "When I ask for desk recommendations using a mock optimiser " >> {
      val observer = new MockWithObserver

      val queues = List(FastTrack, NonEeaDesk, EeaDesk)
      val eeaMaxDesks = totalDesks - roWMinDesks - ftMinDesks
      val roWMaxDesks = totalDesks - ftMinDesks - eeaMinDesks
      val ftMaxDesks = totalDesks - eeaMinDesks - roWMinDesks

      crunchLoadsWithFlexing(mockLoads(queues), totalDesks, minDesks, Map(), slas, flexedQueuesPriority, observer.mockDeskRecs)

      s"I should observe the max desks as EEA: $eeaMaxDesks, RoW: $roWMaxDesks, FT: $ftMaxDesks" >> {
        val expectedMaxEea = List.fill(24)(eeaMaxDesks)
        val expectedMaxRoW = List.fill(24)(roWMaxDesks)
        val expectedMaxFt = List.fill(24)(ftMaxDesks)
        val expectedObservedMaxDesks = List(expectedMaxEea, expectedMaxRoW, expectedMaxFt)

        observer.observedMaxDesks === expectedObservedMaxDesks
      }
    }
  }

  s"Given workload for EGates, EEA & RoW, FastTrack & EGates " >> {
    "When I ask for desk recommendations using a mock optimiser " >> {
      val observer = new MockWithObserver

      val queues = List(EGate, FastTrack, NonEeaDesk, EeaDesk)

      val egateMaxDesks = 15
      val egateMaxDesks24 = List.fill(24)(egateMaxDesks)
      val eeaMaxDesks = totalDesks - roWMinDesks - ftMinDesks
      val roWMaxDesks = totalDesks - ftMinDesks - eeaMinDesks
      val ftMaxDesks = totalDesks - eeaMinDesks - roWMinDesks

      crunchLoadsWithFlexing(mockLoads(queues), totalDesks, minDesks, Map(EGate -> egateMaxDesks24), slas, flexedQueuesPriority, observer.mockDeskRecs)

      s"I should observe the max desks as EEA: $eeaMaxDesks, RoW: $roWMaxDesks, FT: $ftMaxDesks, EGate: $egateMaxDesks" >> {
        val expectedMaxEea = List.fill(24)(eeaMaxDesks)
        val expectedMaxRoW = List.fill(24)(roWMaxDesks)
        val expectedMaxFt = List.fill(24)(ftMaxDesks)
        val expectedMaxEGate = egateMaxDesks24

        val expectedObservedMaxDesks = List(expectedMaxEea, expectedMaxRoW, expectedMaxFt, expectedMaxEGate)

        observer.observedMaxDesks === expectedObservedMaxDesks
      }
    }
  }

  private def mockLoads(queues: List[Queue]): Map[Queue, Seq[Double]] = queues.map(q => (q, List())).toMap

}
