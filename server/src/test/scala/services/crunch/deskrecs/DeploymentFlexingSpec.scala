package services.crunch.deskrecs

import drt.shared.Queues._
import services.crunch.CrunchTestLike
import services.{OptimizerConfig, OptimizerCrunchResult}

import scala.util.{Success, Try}

class DeploymentFlexingSpec extends CrunchTestLike {
  val totalDesks = 20
  val totalStaff = 18
  val eeaMinDesks = 1
  val roWMinDesks = 2
  val ftMinDesks = 3
  val egateMinDesks = 4
  val egateMaxDesks = 15

  val minsToCrunch = 30

  val totalDesks24: List[Int] = List.fill(minsToCrunch)(totalDesks)
  val eeaMinDesks24: List[Int] = List.fill(minsToCrunch)(eeaMinDesks)
  val roWMinDesks24: List[Int] = List.fill(minsToCrunch)(roWMinDesks)
  val ftMinDesks24: List[Int] = List.fill(minsToCrunch)(ftMinDesks)
  val egateMinDesks24: List[Int] = List.fill(minsToCrunch)(egateMinDesks)
  val egateMaxDesks24: List[Int] = List.fill(minsToCrunch)(egateMaxDesks)

  val minDesks: Map[Queue, List[Int]] = Map(
    FastTrack -> ftMinDesks24,
    NonEeaDesk -> roWMinDesks24,
    EeaDesk -> eeaMinDesks24,
    EGate -> egateMinDesks24
    )

  val maxDesks: Map[Queue, List[Int]] = Map(EGate -> egateMaxDesks24)

  val slas: Map[Queue, Int] = List(FastTrack, NonEeaDesk, EeaDesk, EGate).map(q => (q, 20)).toMap

  class MockWithObserver {
    var observedMaxDesks: List[List[Int]] = List()

    val mockDeskRecs: (Seq[Double], Seq[Int], Seq[Int], OptimizerConfig) => Try[OptimizerCrunchResult] =
      (_: Seq[Double], minDesks: Seq[Int], maxDesks: Seq[Int], _: OptimizerConfig) => {
        observedMaxDesks = observedMaxDesks ++ List(maxDesks.toList)
        Success(OptimizerCrunchResult(minDesks.toIndexedSeq, minDesks))
      }
  }

  val queuePriority: List[Queue] = List(EeaDesk, NonEeaDesk, QueueDesk, EGate, FastTrack)

  "When I ask for the max staff available for a queue" >> {
    val totalDesks = 20
    val totalStaff = 18
    s"Given a single queue with $totalDesks total desks, and $totalStaff available staff" >> {
      s"I should get the minimum of the 2 numbers, ie $totalStaff" >> {
        val maxStaffAvailable = List(totalStaff, totalDesks).min

        maxStaffAvailable === totalStaff
      }
    }

    val minDesks = 2
    s"Given a single queue with $minDesks min desks $totalDesks total desks, and $totalStaff available staff" >> {
      s"I should get the minimum of the 2 numbers, ie $totalStaff, minus $minDesks min desks = ${totalStaff - minDesks}" >> {
        val maxStaffAvailable = List(totalStaff, totalDesks).min - minDesks

        maxStaffAvailable === (totalStaff - minDesks)
      }
    }

    val alreadyDeployed = 3
    s"Given a single queue with $minDesks min desks $totalDesks total desks, $totalStaff available staff and $alreadyDeployed staff already deployed" >> {
      s"I should get the minimum of the 2 numbers, ie $totalStaff, minus $minDesks min desks = ${totalStaff - minDesks - alreadyDeployed}" >> {
        val maxStaffAvailable = List(totalStaff, totalDesks).min - minDesks - alreadyDeployed

        maxStaffAvailable === (totalStaff - minDesks) - alreadyDeployed
      }
    }

    import services.graphstages.Crunch.listOp

    val list1 = List(1, 2, 3)
    val list2 = List(5, 5, 5)
    s"Given 2 Lists of Ints - $list1 & $list2" >> {
      "When I ask for them to be reduced with a + operation " >> {
        val expected = List(6, 7, 8)
        s"I should get $expected" >> {
          val result = List(list1, list2).reduce(listOp[Int](_ + _))
          result === expected
        }
      }

      "When I ask for them to be reduced with a - operation " >> {
        val expected = List(-4, -3, -2)
        s"I should get $expected" >> {
          val result = List(list1, list2).reduce(listOp[Int](_ - _))
          result === expected
        }
      }
    }

    val list3 = List(1, 2, 3)
    s"Given 3 Lists of Ints - $list1 & $list2 & $list3" >> {
      "When I ask for them to be reduced with a - operation " >> {
        val expected = List(-5, -5, -5)
        s"I should get $expected" >> {
          val result = List(list1, list2, list3).reduce(listOp[Int](_ - _))
          result === expected
        }
      }
    }
  }
}
