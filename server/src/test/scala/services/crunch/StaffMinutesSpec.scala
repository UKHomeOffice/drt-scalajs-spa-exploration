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

  "Given two staff movement covering the same time period" +
    "When I ask for the PortState " +
    "Then I should see the sum of the movements for the minute they cover" >> {
    val shiftStart = SDate("2017-01-01T00:00Z")

    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    val initialMovements = Seq(
      StaffMovement("T1", "lunch start", MilliDate(shiftStart.millisSinceEpoch), -1, uuid1),
      StaffMovement("T1", "lunch end", MilliDate(shiftStart.addMinutes(15).millisSinceEpoch), 1, uuid1),
      StaffMovement("T1", "lunch start", MilliDate(shiftStart.millisSinceEpoch), -5, uuid2),
      StaffMovement("T1", "lunch end", MilliDate(shiftStart.addMinutes(15).millisSinceEpoch), 5, uuid2)
    )

    val crunch = runCrunchGraph(
      airportConfig = airportConfig.copy(terminalNames = Seq("T1")),
      now = () => shiftStart
    )

    offerAndWait(crunch.liveStaffMovementsInput, initialMovements)

    val expectedStaffMovements = Seq(
      shiftStart.addMinutes(0).millisSinceEpoch -> -6,
      shiftStart.addMinutes(1).millisSinceEpoch -> -6,
      shiftStart.addMinutes(2).millisSinceEpoch -> -6,
      shiftStart.addMinutes(3).millisSinceEpoch -> -6,
      shiftStart.addMinutes(4).millisSinceEpoch -> -6,
      shiftStart.addMinutes(5).millisSinceEpoch -> -6,
      shiftStart.addMinutes(6).millisSinceEpoch -> -6,
      shiftStart.addMinutes(7).millisSinceEpoch -> -6,
      shiftStart.addMinutes(8).millisSinceEpoch -> -6,
      shiftStart.addMinutes(9).millisSinceEpoch -> -6
    )

    crunch.liveTestProbe.fishForMessage(2 seconds) {
      case ps: PortState  =>
        val minutesInOrder = ps.staffMinutes.values.toList.sortBy(_.minute).take(10)
        val staffMovements = minutesInOrder.map(sm => (sm.minute, sm.movements))

        staffMovements === expectedStaffMovements
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
    val flight = ArrivalGenerator.apiFlight(iata = "BA0001", schDt = scheduled, actPax = Option(100))

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

  "Given one initial fixed point " +
    "When I remove the fixed point " +
    "Then I should not see zero fixed points in the staff minutes" >> {
    val scheduled = "2017-01-01T00:00Z"
    val shiftStart = SDate(scheduled)

    val initialFixedPoints =
      """egate monitor,T1,01/01/17,00:00,00:14,2
      """.stripMargin

    val crunch = runCrunchGraph(
      airportConfig = airportConfig.copy(
        terminalNames = Seq("T1")
      ),
      now = () => shiftStart
    )

    offerAndWait(crunch.liveFixedPointsInput, initialFixedPoints)

    offerAndWait(crunch.liveFixedPointsInput, "")

    val expectedFixedPoints = Seq(
      shiftStart.addMinutes(0).millisSinceEpoch -> 0,
      shiftStart.addMinutes(1).millisSinceEpoch -> 0,
      shiftStart.addMinutes(2).millisSinceEpoch -> 0,
      shiftStart.addMinutes(3).millisSinceEpoch -> 0,
      shiftStart.addMinutes(4).millisSinceEpoch -> 0,
      shiftStart.addMinutes(5).millisSinceEpoch -> 0,
      shiftStart.addMinutes(6).millisSinceEpoch -> 0,
      shiftStart.addMinutes(7).millisSinceEpoch -> 0,
      shiftStart.addMinutes(8).millisSinceEpoch -> 0,
      shiftStart.addMinutes(9).millisSinceEpoch -> 0
    )

    crunch.liveTestProbe.fishForMessage(10 seconds) {
      case ps: PortState  =>
        val minutesInOrder = ps.staffMinutes.values.toList.sortBy(_.minute).take(10)
        val fixedPoints = minutesInOrder.map(sm => (sm.minute, sm.fixedPoints))

        fixedPoints == expectedFixedPoints
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
    val flight = ArrivalGenerator.apiFlight(iata = "BA0001", schDt = scheduled, actPax = Option(100))

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

  "Given a shift with 50 staff and a max of 45 split to Eea desk, egates and fastrack " +
    "When I ask for the PortState " +
    "Then I should see deployed staff maxed out on each desk" >> {
    val scheduled = "2017-01-01T00:00Z"
    val shiftStart = SDate(scheduled)
    val initialShifts =
      """shift a,T1,01/01/17,00:00,00:14,50
      """.stripMargin

    val crunch = runCrunchGraph(
      airportConfig = airportConfig.copy(
        terminalNames = Seq("T1"),
        queues = Map("T1" -> Seq(Queues.EeaDesk, Queues.FastTrack, Queues.NonEeaDesk)),
        slaByQueue = Map(Queues.EeaDesk -> 25, Queues.FastTrack -> 30, Queues.NonEeaDesk -> 20),
        defaultPaxSplits = SplitRatios(
          SplitSources.TerminalAverage,
          SplitRatio(eeaMachineReadableToDesk, 0.45),
          SplitRatio(visaNationalToDesk, 0.45),
          SplitRatio(visaNationalToFastTrack, 0.1)
        ),
        defaultProcessingTimes = Map(
          "T1" -> Map(
            eeaMachineReadableToDesk -> 20d / 60,
            visaNationalToDesk -> 20d / 60,
            visaNationalToFastTrack -> 5d / 60
          )
        ),
        minMaxDesksByTerminalQueue = Map(
          "T1" -> Map(
            Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
            Queues.NonEeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
            Queues.FastTrack -> ((List.fill[Int](24)(1), List.fill[Int](24)(5)))
        ))
    ),
      now = () => shiftStart
    )


    offerAndWait(crunch.liveShiftsInput, initialShifts)

    val flight = ArrivalGenerator.apiFlight(iata = "BA0001", schDt = scheduled, actPax = 100)
    offerAndWait(crunch.liveArrivalsInput, Flights(Seq(flight)))

    val expectedCrunchDeployments = Set(
      (Queues.EeaDesk, shiftStart.addMinutes(0), 20),
      (Queues.EeaDesk, shiftStart.addMinutes(1), 20),
      (Queues.EeaDesk, shiftStart.addMinutes(2), 20),
      (Queues.FastTrack, shiftStart.addMinutes(0), 5),
      (Queues.FastTrack, shiftStart.addMinutes(1), 5),
      (Queues.FastTrack, shiftStart.addMinutes(2), 5),
      (Queues.NonEeaDesk, shiftStart.addMinutes(0), 20),
      (Queues.NonEeaDesk, shiftStart.addMinutes(1), 20),
      (Queues.NonEeaDesk, shiftStart.addMinutes(2), 20)
    )

    crunch.liveTestProbe.fishForMessage(5 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.crunchMinutes.values.toList.sortBy(cm => (cm.minute, cm.queueName)).take(9)
        val deployments = minutesInOrder.map(cm => (cm.queueName, SDate(cm.minute), cm.deployedDesks.getOrElse(0))).toSet

        deployments == expectedCrunchDeployments
    }

    true
  }

}
