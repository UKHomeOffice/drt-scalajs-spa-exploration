package services.crunch

import drt.shared.CrunchApi.{CrunchMinute, MillisSinceEpoch, StaffMinute}
import drt.shared._
import org.specs2.mutable.Specification

class CrunchSummarySpec extends Specification {
  "Given a port state with crunch minutes for 2 queues over 30 minutes " +
    "When I ask for a 4 period summary of 15 minutes each period " +
    "Then I should see crunch minutes for all periods" >> {
    val terminal = "T1"
    val queues = List(Queues.EeaDesk, Queues.EGate)
    val cmsList = for {
      queue <- queues
      minute <- 0 to 29
    } yield {
      CrunchMinute(terminal, queue, minute.toLong, minute.toDouble, minute.toDouble, minute, minute, Option(minute), Option(minute), Option(minute), Option(minute))
    }

    val cmsMap = cmsList.map(cm => (TQM(cm), cm)).toMap
    val portState = CrunchApi.PortState(Map[Int, ApiFlightWithSplits](), cmsMap, Map[TM, StaffMinute]())

    val periods = 4
    val periodSize = 15
    val summary: Map[MillisSinceEpoch, Map[String, CrunchMinute]] = portState.summary(0L, periods, periodSize, terminal, queues)

    val expected = Map(
      0L -> Map(
        Queues.EeaDesk -> CrunchMinute(terminal, Queues.EeaDesk, 0, 105, 105, 14, 14, Option(14), Option(14), Option(14), Option(14)),
        Queues.EGate -> CrunchMinute(terminal, Queues.EGate, 0, 105, 105, 14, 14, Option(14), Option(14), Option(14), Option(14))
      ),
      15L -> Map(
        Queues.EeaDesk -> CrunchMinute(terminal, Queues.EeaDesk, 15, 330, 330, 29, 29, Option(29), Option(29), Option(29), Option(29)),
        Queues.EGate -> CrunchMinute(terminal, Queues.EGate, 15, 330, 330, 29, 29, Option(29), Option(29), Option(29), Option(29))
      ),
      30L -> Map(
        Queues.EeaDesk -> CrunchMinute(terminal, Queues.EeaDesk, 30, 0, 0, 0, 0, None, None, None, None),
        Queues.EGate -> CrunchMinute(terminal, Queues.EGate, 30, 0, 0, 0, 0, None, None, None, None)
      ),
      45L -> Map(
        Queues.EeaDesk -> CrunchMinute(terminal, Queues.EeaDesk, 45, 0, 0, 0, 0, None, None, None, None),
        Queues.EGate -> CrunchMinute(terminal, Queues.EGate, 45, 0, 0, 0, 0, None, None, None, None)
      )
    )

    summary === expected
  }
}
