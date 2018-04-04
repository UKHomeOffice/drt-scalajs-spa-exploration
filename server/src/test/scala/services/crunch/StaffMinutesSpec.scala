package services.crunch

import java.util.UUID

import controllers.ArrivalGenerator
import drt.shared.CrunchApi.PortState
import drt.shared.FlightsApi.Flights
import drt.shared.PaxTypesAndQueues._
import drt.shared.SplitRatiosNs.{SplitRatio, SplitRatios, SplitSources}
import drt.shared.{MilliDate, Queues, StaffMovement}
import services.SDate
import services.graphstages.Crunch

import scala.collection.immutable.List
import scala.concurrent.duration._

class StaffMinutesSpec extends CrunchTestLike {
  sequential
  isolated

  "Given two consecutive shifts " +
    "When I ask for the PortState " +
    "Then I should see the staff available for the duration of the shifts" >> {
    val shiftStart = SDate("2017-01-01T00:00Z")

    val crunch = runCrunchGraph(
      airportConfig = airportConfig.copy(terminalNames = Seq("T1")),
      now = () => shiftStart,
      initialShifts =
        """shift a,T1,01/01/17,00:00,00:14,1
          |shift b,T1,01/01/17,00:15,00:29,2
        """.stripMargin
    )

    val expectedStaff = List.fill(15)(1) ::: List.fill(15)(2)
    val expectedMillis = (shiftStart.millisSinceEpoch to (shiftStart.millisSinceEpoch + 29 * Crunch.oneMinuteMillis) by Crunch.oneMinuteMillis).toList

    crunch.liveTestProbe.fishForMessage(5 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.staffMinutes.values.toList.sortBy(_.minute)
        val staff = minutesInOrder.map(_.shifts)
        val staffMillis = minutesInOrder.map(_.minute)

        (staffMillis, staff) == Tuple2(expectedMillis, expectedStaff)
    }

    true
  }

  "Given shifts of 0 and 1 staff and a -1 staff movement at the start of the shift" +
    "When I ask for the PortState " +
    "Then I should see zero staff available rather than a negative number" >> {
    val shiftStart = SDate("2017-01-01T00:00Z")
    val initialShifts =
      """shift a,T1,01/01/17,00:00,00:04,0
        |shift b,T1,01/01/17,00:05,00:09,2
      """.stripMargin
    val uuid = UUID.randomUUID()
    val initialMovements = Seq(
      StaffMovement("T1", "lunch start", MilliDate(shiftStart.millisSinceEpoch), -1, uuid),
      StaffMovement("T1", "lunch end", MilliDate(shiftStart.addMinutes(15).millisSinceEpoch), 1, uuid)
    )

    val crunch = runCrunchGraph(
      airportConfig = airportConfig.copy(terminalNames = Seq("T1")),
      now = () => shiftStart
    )

    offerAndWait(crunch.liveShiftsInput, initialShifts)
    offerAndWait(crunch.liveStaffMovementsInput, initialMovements)

    val expectedStaffAvailable = Seq(
      shiftStart.addMinutes(0).millisSinceEpoch -> 0,
      shiftStart.addMinutes(1).millisSinceEpoch -> 0,
      shiftStart.addMinutes(2).millisSinceEpoch -> 0,
      shiftStart.addMinutes(3).millisSinceEpoch -> 0,
      shiftStart.addMinutes(4).millisSinceEpoch -> 0,
      shiftStart.addMinutes(5).millisSinceEpoch -> 1,
      shiftStart.addMinutes(6).millisSinceEpoch -> 1,
      shiftStart.addMinutes(7).millisSinceEpoch -> 1,
      shiftStart.addMinutes(8).millisSinceEpoch -> 1,
      shiftStart.addMinutes(9).millisSinceEpoch -> 1
    )

    crunch.liveTestProbe.fishForMessage(2 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.staffMinutes.values.toList.sortBy(_.minute).take(10)
        val staffAvailable = minutesInOrder.map(sm => (sm.minute, sm.available))

        staffAvailable == expectedStaffAvailable
    }

    true
  }

  "Given a shift with 10 staff and passengers split to 2 queues " +
    "When I ask for the PortState " +
    "Then I should see deployed staff totalling the number on shift" >> {
    val scheduled = "2017-01-01T00:00Z"
    val shiftStart = SDate(scheduled)
    val initialShifts =
      """shift a,T1,01/01/17,00:00,00:14,10
      """.stripMargin
    val initialFixedPoints =
      """egate monitor,T1,01/01/17,00:00,00:14,2
      """.stripMargin
    val flight = ArrivalGenerator.apiFlight(iata = "BA0001", schDt = scheduled, actPax = 100)

    val crunch = runCrunchGraph(
      airportConfig = airportConfig.copy(
        terminalNames = Seq("T1"),
        defaultPaxSplits = SplitRatios(
          SplitSources.TerminalAverage,
          SplitRatio(eeaMachineReadableToDesk, 0.5),
          SplitRatio(visaNationalToDesk, 0.5)
        ),
        defaultProcessingTimes = Map(
          "T1" -> Map(
            eeaMachineReadableToDesk -> 25d / 60,
            visaNationalToDesk -> 75d / 60
          )
        )
      ),
      now = () => shiftStart
    )

    offerAndWait(crunch.liveShiftsInput, initialShifts)
    offerAndWait(crunch.liveFixedPointsInput, initialFixedPoints)
    offerAndWait(crunch.liveArrivalsInput, Flights(Seq(flight)))

    val expectedCrunchDeployments = Set(
      (Queues.EeaDesk, shiftStart.addMinutes(0), 2),
      (Queues.EeaDesk, shiftStart.addMinutes(1), 2),
      (Queues.EeaDesk, shiftStart.addMinutes(2), 2),
      (Queues.EeaDesk, shiftStart.addMinutes(3), 2),
      (Queues.EeaDesk, shiftStart.addMinutes(4), 2),
      (Queues.NonEeaDesk, shiftStart.addMinutes(0), 6),
      (Queues.NonEeaDesk, shiftStart.addMinutes(1), 6),
      (Queues.NonEeaDesk, shiftStart.addMinutes(2), 6),
      (Queues.NonEeaDesk, shiftStart.addMinutes(3), 6),
      (Queues.NonEeaDesk, shiftStart.addMinutes(4), 6))

    crunch.liveTestProbe.fishForMessage(10 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.crunchMinutes.values.toList.sortBy(cm => (cm.minute, cm.queueName)).take(10)
        val deployments = minutesInOrder.map(cm => (cm.queueName, SDate(cm.minute), cm.deployedDesks.getOrElse(0))).toSet

        deployments == expectedCrunchDeployments
    }

    true
  }

  "Given a shift with 10 staff and passengers split to Eea desk & egates " +
    "When I ask for the PortState " +
    "Then I should see deployed staff totalling the number on shift" >> {
    val scheduled = "2017-01-01T00:00Z"
    val shiftStart = SDate(scheduled)
    val initialShifts =
      """shift a,T1,01/01/17,00:00,00:14,10
      """.stripMargin
    val initialFixedPoints =
      """egate monitor,T1,01/01/17,00:00,00:14,2
      """.stripMargin
    val flight = ArrivalGenerator.apiFlight(iata = "BA0001", schDt = scheduled, actPax = 100)

    val crunch = runCrunchGraph(
      airportConfig = airportConfig.copy(
        terminalNames = Seq("T1"),
        queues = Map("T1" -> Seq(Queues.EeaDesk, Queues.EGate)),
        defaultPaxSplits = SplitRatios(
          SplitSources.TerminalAverage,
          SplitRatio(eeaMachineReadableToDesk, 0.1),
          SplitRatio(eeaMachineReadableToEGate, 0.9)
        ),
        defaultProcessingTimes = Map(
          "T1" -> Map(
            eeaMachineReadableToDesk -> 20d / 60,
            eeaMachineReadableToEGate -> 20d / 60
          )
        )
      ),
      now = () => shiftStart
    )

    offerAndWait(crunch.liveShiftsInput, initialShifts)
    offerAndWait(crunch.liveFixedPointsInput, initialFixedPoints)
    offerAndWait(crunch.liveArrivalsInput, Flights(Seq(flight)))

    val expectedCrunchDeployments = Set(
      (Queues.EeaDesk, shiftStart.addMinutes(0), 2),
      (Queues.EeaDesk, shiftStart.addMinutes(1), 2),
      (Queues.EeaDesk, shiftStart.addMinutes(2), 2),
      (Queues.EeaDesk, shiftStart.addMinutes(3), 2),
      (Queues.EeaDesk, shiftStart.addMinutes(4), 2),
      (Queues.EGate, shiftStart.addMinutes(0), 6),
      (Queues.EGate, shiftStart.addMinutes(1), 6),
      (Queues.EGate, shiftStart.addMinutes(2), 6),
      (Queues.EGate, shiftStart.addMinutes(3), 6),
      (Queues.EGate, shiftStart.addMinutes(4), 6))

    crunch.liveTestProbe.fishForMessage(5 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.crunchMinutes.values.toList.sortBy(cm => (cm.minute, cm.queueName)).take(10)
        val deployments = minutesInOrder.map(cm => (cm.queueName, SDate(cm.minute), cm.deployedDesks.getOrElse(0))).toSet

        println(s"deployments: $deployments")
        deployments == expectedCrunchDeployments
    }

    true
  }
}
