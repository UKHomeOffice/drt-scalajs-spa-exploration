package actors

import org.specs2.mutable.Specification
import PortStateMessageConversion._
import drt.shared.CrunchApi.{CrunchMinute, PortState, StaffMinute}
import drt.shared.Queues
import server.protobuf.messages.CrunchState.{CrunchMinuteMessage, CrunchStateSnapshotMessage, StaffMinuteMessage}

class PortStateMessageConversionSpec extends Specification {
  "Given a persisted port state message containing some valid & invalid crunch and staff minutes " +
    "When I ask for the corresponding PortState " +
    "Then I should only see the valid crunch & staff minutes" >> {
    val validMinuteMilli = 0L
    val invalidMinuteMilli = 60001L
    val crunchMinutes = Seq(
      CrunchMinuteMessage(Option("T1"), Option(Queues.EeaDesk), Option(validMinuteMilli), Option(0), Option(0), Option(0), Option(0), None, None, None, None),
      CrunchMinuteMessage(Option("T1"), Option(Queues.EeaDesk), Option(invalidMinuteMilli), Option(0), Option(0), Option(0), Option(0), None, None, None, None)
    )
    val staffMinutes = Seq(
      StaffMinuteMessage(Option("T1"), Option(validMinuteMilli), Option(0), Option(0), Option(0), None),
      StaffMinuteMessage(Option("T1"), Option(invalidMinuteMilli), Option(0), Option(0), Option(0), None)
    )
    val state = snapshotMessageToState(CrunchStateSnapshotMessage(None, None, Seq(), crunchMinutes, staffMinutes), None)

    val expectedCrunchMinutes = Seq(CrunchMinute("T1", Queues.EeaDesk, validMinuteMilli, 0, 0, 0, 0, None, None, None, None, None)).map(m => (m.key, m)).toMap
    val expectedStaffMinutes = Seq(StaffMinute("T1", validMinuteMilli, 0, 0, 0, None)).map(m => (m.key, m)).toMap

    val expected = PortState(Map(), expectedCrunchMinutes, expectedStaffMinutes)

    state === expected
  }
}