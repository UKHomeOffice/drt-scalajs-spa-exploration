package services

import akka.NotUsed
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.stream._
import akka.stream.scaladsl._
import akka.testkit.{TestKit, TestProbe}
import controllers.ArrivalGenerator
import drt.shared.FlightsApi.{QueueName, TerminalName}
import drt.shared.PaxTypes.EeaMachineReadable
import drt.shared.PaxTypesAndQueues._
import drt.shared.SplitRatiosNs.SplitSources
import drt.shared.SplitRatiosNs.SplitSources.ApiSplitsWithCsvPercentage
import drt.shared._
import org.joda.time.DateTimeZone
import org.specs2.mutable.{Specification, SpecificationLike}
import passengersplits.AkkaPersistTestConfig
import services.Crunch.{flightSplitMinutesToQueueLoadMinutes, _}
import services.workloadcalculator.PaxLoadCalculator.MillisSinceEpoch
import org.joda.time.{DateTime, DateTimeZone}

import scala.collection.immutable
import scala.collection.immutable.Map
import scala.concurrent.duration._
import scala.util.Success


//case class QueueMinute(queueName: QueueName, paxLoad: Double, workLoad: Double, crunchDesks: Int, crunchWait: Int, allocStaff: Int, allocWait: Int, minute: Long)

class StreamingSpec extends TestKit(ActorSystem("StreamingCrunchTests", AkkaPersistTestConfig.inMemoryAkkaPersistConfig)) with SpecificationLike {
  isolated
  sequential

  implicit val actorSystem = system
  implicit val materializer = ActorMaterializer()
  val oneMinute = 60000
  val validTerminals = Set("T1", "T2")
  val uniquifyArrivals = CodeShares.uniqueArrivalsWithCodeShares((f: ApiFlightWithSplits) => f.apiFlight) _

  "Given two identical sets of FlightSplitMinutes for a flight " +
    "When I ask for the differences" +
    "Then I get a an empty set of differences" >> {
    val oldSet = Set(FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 0L))
    val newSet = Set(FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 0L))

    val result = flightLoadDiff(oldSet, newSet)
    val expected = Set()

    result === expected
  }

  "Given two sets of FlightSplitMinutes for a flight offset by a minute " +
    "When I ask for the differences" +
    "Then I get a one removal and one addition representing the old & new times" >> {
    val oldSet = Set(FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 0L))
    val newSet = Set(FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 1L))

    val result = flightLoadDiff(oldSet, newSet)
    val expected = Set(
      FlightSplitDiff(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, -10, -200, 0L),
      FlightSplitDiff(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 1L)
    )

    result === expected
  }

  "Given two sets of FlightSplitMinutes for a flight where the minute is the same but the loads have increased " +
    "When I ask for the differences" +
    "Then I get a single diff with the load difference " >> {
    val oldSet = Set(FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 0L))
    val newSet = Set(FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 15, 300, 0L))

    val result = flightLoadDiff(oldSet, newSet)
    val expected = Set(
      FlightSplitDiff(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 5, 100, 0L)
    )

    result === expected
  }

  "Given two sets of single FlightSplitMinutes for the same minute but with an increased load " +
    "When I ask for the differences" +
    "Then I get a set containing one FlightSplitDiff representing the increased load" >> {
    val oldSet = Set(FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 0L))
    val newSet = Set(FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 15, 300, 0L))

    val result = flightLoadDiff(oldSet, newSet)
    val expected = Set(FlightSplitDiff(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 5, 100, 0L))

    result === expected
  }

  "Given two sets of 3 FlightSplitMinutes for 2 queues where the minute shifts and the loads" +
    "When I ask for the differences" +
    "Then I get a set containing the corresponding diffs" >> {
    val oldSet = Set(
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 0L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 10, 200, 1L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 7, 140, 2L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EGate, 15, 300, 0L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EGate, 15, 300, 1L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EGate, 11, 220, 2L)
    )
    val newSet = Set(
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 12, 240, 1L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 12, 240, 2L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EeaDesk, 5, 100, 3L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EGate, 6, 120, 1L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EGate, 6, 120, 2L),
      FlightSplitMinute(1, PaxTypes.EeaMachineReadable, "T1", Queues.EGate, 3, 60, 3L))

    val result = flightLoadDiff(oldSet, newSet)
    val expected = Set(
      FlightSplitDiff(1, EeaMachineReadable, "T1", Queues.EeaDesk, -10.0, -200.0, 0),
      FlightSplitDiff(1, EeaMachineReadable, "T1", Queues.EeaDesk, 2.0, 40.0, 1),
      FlightSplitDiff(1, EeaMachineReadable, "T1", Queues.EeaDesk, 5.0, 100.0, 2),
      FlightSplitDiff(1, EeaMachineReadable, "T1", Queues.EeaDesk, 5.0, 100.0, 3),
      FlightSplitDiff(1, EeaMachineReadable, "T1", Queues.EGate, -15.0, -300.0, 0),
      FlightSplitDiff(1, EeaMachineReadable, "T1", Queues.EGate, -9.0, -180.0, 1),
      FlightSplitDiff(1, EeaMachineReadable, "T1", Queues.EGate, -5.0, -100.0, 2),
      FlightSplitDiff(1, EeaMachineReadable, "T1", Queues.EGate, 3.0, 60.0, 3)
    )

    result === expected
  }

  "Given an SDateLike for a date outside BST" +
    "When I ask for a corresponding cunch start time " +
    "Then I should get an SDateLike representing the previous midnight UTC" >> {
    val now = SDate("2010-01-02T11:39", DateTimeZone.forID("Europe/London"))

    val result = getLocalLastMidnight(now).millisSinceEpoch
    val expected = SDate("2010-01-02T00:00").millisSinceEpoch

    result === expected
  }

  "Given an SDateLike for a date inside BST" +
    "When I ask for a corresponding cunch start time " +
    "Then I should get an SDateLike representing the previous midnight UTC" >> {
    val now = SDate("2010-07-02T11:39", DateTimeZone.forID("Europe/London"))
    val result: MillisSinceEpoch = getLocalLastMidnight(now).millisSinceEpoch
    val expected = SDate("2010-07-01T23:00").millisSinceEpoch

    result === expected
  }

  "Given a flight with one passenger and one split to eea desk " +
    "When I ask for queue loads " +
    "Then I should see a single eea desk queue load containing the passenger and their proc time" >> {
    val scheduled = "2017-01-01T00:00Z"
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled),
        List(ApiSplits(
          List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val emr2dProcTime = 20d / 60
    val emr2eProcTime = 35d / 60
    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> emr2dProcTime,
      eeaMachineReadableToEGate -> emr2eProcTime)

    val sourceUnderTest = Source.tick(0.seconds, 200.millis, flightsWithSplits)

    val probe = TestProbe()(system)
    val cancellable = sourceUnderTest.map(s => flightSplitMinutesToQueueLoadMinutes(flightsToFlightSplitMinutes(procTimes)(s)))
      .to(Sink.actorRef(probe.ref, "completed"))
      .run()

    val expected = Set(QueueLoadMinute("T1", Queues.EeaDesk, 1.0, emr2dProcTime, SDate(scheduled, DateTimeZone.UTC).millisSinceEpoch))

    probe.expectMsg(expected)
    cancellable.cancel()

    true
  }

  "Given a flight with one passenger and splits to eea desk & egates " +
    "When I ask for queue loads " +
    "Then I should see 2 queue loads, each representing their portion of the passenger and the split queue" >> {
    val scheduled = "2017-01-01T00:00Z"
    val scheduledMillis = SDate(scheduled, DateTimeZone.UTC).millisSinceEpoch
    val edPax = 0.25
    val egPax = 0.75
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled),
        List(ApiSplits(List(
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, edPax),
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EGate, egPax)
        ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val emr2dProcTime = 20d / 60
    val emr2eProcTime = 35d / 60
    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> emr2dProcTime,
      eeaMachineReadableToEGate -> emr2eProcTime)

    val sourceUnderTest = Source.tick(0.seconds, 200.millis, flightsWithSplits)

    val probe = TestProbe()

    sourceUnderTest.map(s => flightSplitMinutesToQueueLoadMinutes(flightsToFlightSplitMinutes(procTimes)(s)))
      .to(Sink.actorRef(probe.ref, "completed"))
      .run()

    val expected = Set(
      QueueLoadMinute("T1", Queues.EeaDesk, edPax, edPax * emr2dProcTime, scheduledMillis),
      QueueLoadMinute("T1", Queues.EGate, egPax, egPax * emr2eProcTime, scheduledMillis))

    probe.expectMsg(expected)
    true
  }


  "Given a flight with 21 passengers and splits to eea desk & egates " +
    "When I ask for queue loads " +
    "Then I should see 4 queue loads, 2 for the first 20 pax to each queue and 2 for the last 1 split to each queue" >> {
    val scheduled = "2017-01-01T00:00Z"
    val scheduledMillis = SDate(scheduled, DateTimeZone.UTC).millisSinceEpoch
    val totalPax = 21
    val edSplit = 0.25
    val egSplit = 0.75
    val edPax = edSplit * totalPax
    val egPax = egSplit * totalPax
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled),
        List(ApiSplits(List(
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, edPax),
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EGate, egPax)
        ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val emr2dProcTime = 20d / 60
    val emr2eProcTime = 35d / 60
    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> emr2dProcTime,
      eeaMachineReadableToEGate -> emr2eProcTime)

    val sourceUnderTest = Source.tick(0.seconds, 200.millis, flightsWithSplits)

    val probe = TestProbe()
    sourceUnderTest.map(s => flightSplitMinutesToQueueLoadMinutes(flightsToFlightSplitMinutes(procTimes)(s)))
      .to(Sink.actorRef(probe.ref, "completed"))
      .run()

    val expected = Set(
      QueueLoadMinute("T1", Queues.EeaDesk, 20 * edSplit, 20 * edSplit * emr2dProcTime, scheduledMillis),
      QueueLoadMinute("T1", Queues.EGate, 20 * egSplit, 20 * egSplit * emr2eProcTime, scheduledMillis),
      QueueLoadMinute("T1", Queues.EeaDesk, 1 * edSplit, 1 * edSplit * emr2dProcTime, scheduledMillis + oneMinute),
      QueueLoadMinute("T1", Queues.EGate, 1 * egSplit, 1 * egSplit * emr2eProcTime, scheduledMillis + oneMinute))

    probe.expectMsg(expected)
    true
  }

  "Given 2 flights with one passenger each and one split to eea desk arriving at pcp 1 minute apart" +
    "When I ask for queue loads " +
    "Then I should see two eea desk queue loads containing the 2 passengers and their proc time" >> {
    val scheduled1 = "2017-01-01T00:00Z"
    val scheduled2 = "2017-01-01T00:01Z"
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1),
        List(ApiSplits(
          List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled2),
        List(ApiSplits(
          List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val emr2dProcTime = 20d / 60
    val emr2eProcTime = 35d / 60
    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> emr2dProcTime,
      eeaMachineReadableToEGate -> emr2eProcTime)
    val sourceUnderTest = Source.tick(0.seconds, 200.millis, flightsWithSplits)

    val probe = TestProbe()

    sourceUnderTest.map(s => flightSplitMinutesToQueueLoadMinutes(flightsToFlightSplitMinutes(procTimes)(s)))
      .to(Sink.actorRef(probe.ref, "completed"))
      .run()

    val expected = Set(
      QueueLoadMinute("T1", Queues.EeaDesk, 1.0, emr2dProcTime, SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch),
      QueueLoadMinute("T1", Queues.EeaDesk, 1.0, emr2dProcTime, SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + oneMinute))

    probe.expectMsg(expected)
    true
  }

  "Given 2 flights with one passenger each and one split to eea desk arriving at pcp 1 minute apart" +
    "When I ask for queue workloads between two times " +
    "Then I should get a map of every minute in the day, with workload in minutes when we have flights" >> {
    val scheduled1 = "2017-01-01T00:00Z"
    val scheduled2 = "2017-01-01T00:01Z"
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1),
        List(ApiSplits(
          List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled2),
        List(ApiSplits(
          List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val emr2dProcTime = 20d / 60
    val emr2eProcTime = 35d / 60
    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> emr2dProcTime,
      eeaMachineReadableToEGate -> emr2eProcTime)
    val sourceUnderTest = Source.tick(0.seconds, 200.millis, flightsWithSplits)

    val probe = TestProbe()
    val startTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch
    val endTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + 120000

    sourceUnderTest
      .map(s => flightSplitMinutesToQueueLoadMinutes(flightsToFlightSplitMinutes(procTimes)(s)))
      .map(indexQueueWorkloadsByMinute)
      .map(queueMinutesForPeriod(startTime, endTime))
      .to(Sink.actorRef(probe.ref, "completed"))
      .run()

    val expected = Map(
      "T1" -> Map(
        Queues.EeaDesk -> List(
          (SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch, (1.0, emr2dProcTime)),
          (SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + oneMinute, (1.0, emr2dProcTime)),
          (SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + 120000, (0.0, 0.0)))))

    probe.expectMsg(expected)
    true
  }

  "Given 2 flights with one passenger each and one split to eea desk arriving at pcp 1 minute apart" +
    "When crunch queue workloads between two times " +
    "Then I should get a map queue to map of minute to desk rec" >> {
    val scheduled1 = "2017-01-01T00:00Z"
    val scheduled2 = "2017-01-01T00:01Z"
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1),
        List(ApiSplits(
          List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled2),
        List(ApiSplits(
          List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val emr2dProcTime = 20d / 60
    val emr2eProcTime = 35d / 60
    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> emr2dProcTime,
      eeaMachineReadableToEGate -> emr2eProcTime)
    val sourceUnderTest = Source.tick(0.seconds, 200.millis, flightsWithSplits)

    val probe = TestProbe()
    val startTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch
    val endTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + (29 * oneMinute)

    val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 20)
    val minMaxDesks = Map("T1" -> Map(
      Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
      Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20)))))
    val eGateBankSize = 5

    sourceUnderTest
      .map(s => flightSplitMinutesToQueueLoadMinutes(flightsToFlightSplitMinutes(procTimes)(s)))
      .map(indexQueueWorkloadsByMinute)
      .map(queueMinutesForPeriod(startTime, endTime))
      .map(pwl => queueWorkloadsToCrunchResults(startTime, pwl, slaByQueue, minMaxDesks, eGateBankSize))
      .to(Sink.actorRef(probe.ref, "completed"))
      .run()

    val expected = Map(
      "T1" -> Map(Queues.EeaDesk -> Success(
        OptimizerCrunchResult(
          Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
          Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        ))))

    probe.expectMsg(expected)
    true
  }

  "Given 2 flights with one passenger each and one split to eea desk arriving at pcp 1 minute apart" +
    "When crunch queue workloads between two times " +
    "Then I should emit a CrunchState" >> {
    val scheduled1 = "2017-01-01T00:00Z"
    val scheduled2 = "2017-01-01T00:01Z"
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1),
        List(ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled2),
        List(ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> 20d / 60,
      eeaMachineReadableToEGate -> 35d / 60
    )

    val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 20)
    val minMaxDesks = Map("T1" -> Map(
      Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
      Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20)))))

    val probe = TestProbe()
    val startTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch
    val endTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + (29 * oneMinute)

    val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
    publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

    val zeroMinutes = (1483228920000L to 1483230540000L by oneMinute).toList
    val zeroLoads = zeroMinutes.map(minute => (minute, (0.0, 0.0)))
    val workloads = (1483228800000L, (1.0, 0.3333333333333333)) :: (1483228860000L, (1.0, 0.3333333333333333)) :: zeroLoads

    val expected = CrunchState(
      flightsWithSplits,
      Map("T1" -> Map(Queues.EeaDesk -> workloads)),
      Map("T1" -> Map(Queues.EeaDesk -> Success(OptimizerCrunchResult(
        Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))))),
      startTime
    )

    probe.expectMsg(expected)

    true
  }

  "CSV split ratios " >> {
    "Given a flight with 20 passengers and one CSV split of 25% to eea desk" +
      "When request a crunch " +
      "Then I should see a pax load of 5 (20 * 0.25)" >> {
      val scheduled1 = "2017-01-01T00:00Z"
      val scheduled2 = "2017-01-01T00:01Z"
      val flightsWithSplits = List(
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1, actPax = 20),
          List(ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 25)), SplitSources.Historical, Percentage))))

      val procTimes: Map[PaxTypeAndQueue, Double] = Map(
        eeaMachineReadableToDesk -> 25d / 60
      )

      val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 20)
      val minMaxDesks = Map("T1" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20)))))

      val probe = TestProbe()
      val startTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch
      val endTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + (29 * oneMinute)

      val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
      publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

      val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

      val expected = Map(
        "T1" -> Map(Queues.EeaDesk -> Seq(
          5.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)))


      val resultSummary: Map[TerminalName, Map[QueueName, List[Double]]] = result match {
        case CrunchState(_, workloads, _, _) =>
          workloads.mapValues {
            case twl => twl.mapValues {
              case qwl => qwl.map(_._2._1)
            }
          }
      }

      resultSummary === expected
    }
  }

  "Split source precedence " >> {
    "Given a flight with both api & csv splits " +
      "When I crunch " +
      "I should see pax loads calculated from the api splits, ie 15 pax in first minute not 10 " >> {
      val scheduled1 = "2017-01-01T00:00Z"
      val scheduled2 = "2017-01-01T00:01Z"
      val flightsWithSplits = List(
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1, actPax = 20),
          List(
            ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 50)), SplitSources.Historical, Percentage),
            ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 15)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers)
          )))

      val procTimes: Map[PaxTypeAndQueue, Double] = Map(
        eeaMachineReadableToDesk -> 25d / 60
      )

      val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 20)
      val minMaxDesks = Map("T1" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20)))))

      val probe = TestProbe()
      val startTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch
      val endTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + (29 * oneMinute)

      val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
      publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

      val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

      val expected = Map(
        "T1" -> Map(Queues.EeaDesk -> Seq(
          15.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)))


      val resultSummary: Map[TerminalName, Map[QueueName, List[Double]]] = result match {
        case CrunchState(_, workloads, _, _) =>
          workloads.mapValues {
            case twl => twl.mapValues {
              case qwl => qwl.map(_._2._1)
            }
          }
      }

      resultSummary === expected
    }
  }

  "Given flights with one passenger and one split to eea desk" +
    "When the date falls within GMT" +
    "Then I should see desks being allocated at the time passengers start arriving at PCP" >> {
    val scheduled1 = "2017-01-01T00:00Z"
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1),
        List(ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> 20d / 60
    )

    val slaByQueue = Map(Queues.EeaDesk -> 25)
    val minMaxDesks = Map("T1" -> Map(Queues.EeaDesk -> ((List.fill[Int](24)(0), List.fill[Int](24)(20)))))

    val probe = TestProbe()
    val startTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch
    val endTime = SDate(scheduled1, DateTimeZone.UTC).millisSinceEpoch + (29 * oneMinute)

    val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
    publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

    val zeroMinutes = (startTime + (oneMinute * 1) to startTime + (oneMinute * 29) by oneMinute).toList
    val zeroLoads = zeroMinutes.map(minute => (minute, (0.0, 0.0)))
    val workloads = (startTime, (1.0, 0.3333333333333333)) :: zeroLoads
    val expected = CrunchState(
      flightsWithSplits,
      Map("T1" -> Map(Queues.EeaDesk -> workloads)),
      Map("T1" -> Map(Queues.EeaDesk -> Success(OptimizerCrunchResult(
        Vector(
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        ),
        Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))))),
      startTime
    )

    probe.expectMsg(expected)

    true
  }

  "Min / Max desks in BST " >> {
    "Given flights with one passenger and one split to eea desk " +
      "When the date falls within BST " +
      "Then I should see min desks allocated in alignment with BST" >> {
      val scheduled1amBST = "2017-06-01T00:00Z"
      val flightsWithSplits = List(
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1amBST),
          List(ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

      val procTimes: Map[PaxTypeAndQueue, Double] = Map(
        eeaMachineReadableToDesk -> 20d / 60
      )

      val slaByQueue = Map(Queues.EeaDesk -> 25)
      val minMaxDesks = Map("T1" -> Map(Queues.EeaDesk -> ((0 :: 5 :: List.fill[Int](22)(0), List.fill[Int](24)(20)))))

      val probe = TestProbe()
      val startTimeMidnightBST = SDate("2017-05-30T23:00Z").millisSinceEpoch
      val endTime = startTimeMidnightBST + (119 * oneMinute)

      val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
      publisher.publish(CrunchFlights(flightsWithSplits, startTimeMidnightBST, endTime))

      val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

      val expected = Seq(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
      )

      val resultSummary = result match {
        case CrunchState(_, _, crunchResult, _) =>
          crunchResult.flatMap {
            case (_, twl) => twl.flatMap {
              case (_, Success(OptimizerCrunchResult(recommendedDesks, _))) => recommendedDesks
              case _ => IndexedSeq()
            }
          }.toList
      }

      resultSummary === expected
    }

    "Given a list of Min or Max desks" >> {
      "When parsing a BST date then we should get BST min/max desks" >> {
        val testMaxDesks = List(0, 1, 2, 3, 4, 5)
        val startTimeMidnightBST = SDate("2017-06-01T00:00Z").addHours(-1).millisSinceEpoch

        val oneHour = oneMinute * 60
        val startTimes = startTimeMidnightBST to startTimeMidnightBST + (oneHour * 5) by oneHour

        val expected = List(0, 1, 2, 3, 4, 5)
        startTimes.map(desksForHourOfDayInUKLocalTime(_, testMaxDesks)) === expected
      }
      "When parsing a GMT date then we should get BST min/max desks" >> {
        val testMaxDesks = List(0, 1, 2, 3, 4, 5)
        val startTimeMidnightGMT = SDate("2017-01-01T00:00Z").millisSinceEpoch

        val oneHour = oneMinute * 60
        val startTimes = startTimeMidnightGMT to startTimeMidnightGMT + (oneHour * 5) by oneHour

        val expected = List(0, 1, 2, 3, 4, 5)
        startTimes.map(desksForHourOfDayInUKLocalTime(_, testMaxDesks)) === expected
      }
    }
  }

  "Given 2 flights with one passenger each and one split to eea desk arriving at pcp 1 minute apart" +
    "When crunching a whole day " +
    "Then I should emit a CrunchState containing the right flights, workload minutes and crunch minutes" >> {
    val flightsWithSplits = List(
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = "2017-01-01T00:00Z"),
        List(ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
      ApiFlightWithSplits(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = "2017-01-01T00:01Z"),
        List(ApiSplits(List(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1d)), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

    val procTimes: Map[PaxTypeAndQueue, Double] = Map(
      eeaMachineReadableToDesk -> 20d / 60,
      eeaMachineReadableToEGate -> 35d / 60
    )

    val slaByQueue = Map(Queues.EeaDesk -> 25, "nonEeaDesk" -> 45, Queues.EGate -> 20)
    val minMaxDesks = Map("T1" -> Map(
      Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
      Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20)))))

    val probe = TestProbe()
    val startTime = SDate("2017-01-01T00:00Z", DateTimeZone.UTC).millisSinceEpoch
    val endTime = SDate("2017-01-01T23:59Z", DateTimeZone.UTC).millisSinceEpoch

    val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
    publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

    val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

    val resultSummary = result match {
      case CrunchState(flights, workloads, crunchResult, _) =>
        val workloadCount = workloads.map {
          case (_, twl) => twl.map {
            case (_, qwl) => qwl.length
          }.sum
        }.sum
        val successfulCrunchCount = crunchResult.map {
          case (_, twl) => twl.map {
            case (_, Success(qwl)) => 1
            case _ => 0
          }.sum
        }.sum
        (flights, workloadCount, successfulCrunchCount)
    }

    val expected = (flightsWithSplits, 1440, 1)

    resultSummary === expected
  }

  "Egate banks handling " >> {
    "Given flights with 20 very expensive passengers and splits to eea desk & egates " +
      "When I ask for desk recs " +
      "Then I should see lower egates recs by a factor of 5 (rounded up)" >> {
      val scheduled1 = "2017-01-01T00:00Z"
      val flightsWithSplits = List(
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled1),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 10d),
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EGate, 10d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

      val fiveMinutes = 600d / 60
      val procTimes: Map[PaxTypeAndQueue, Double] = Map(
        eeaMachineReadableToDesk -> fiveMinutes,
        eeaMachineReadableToEGate -> fiveMinutes
      )

      val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 25)
      val minMaxDesks = Map("T1" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(0), List.fill[Int](24)(20))),
        Queues.EGate -> ((List.fill[Int](24)(0), List.fill[Int](24)(20)))
      ))

      val probe = TestProbe()
      val startTime = SDate(scheduled1).millisSinceEpoch
      val endTime = startTime + (29 * oneMinute)

      val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
      publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

      val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

      val expected = Seq(
        Seq(
          7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Seq(
          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

      val resultSummary: Seq[IndexedSeq[Int]] = result match {
        case CrunchState(_, _, crunchResult, _) =>
          crunchResult.flatMap {
            case (_, twl) => twl.map {
              case (_, Success(OptimizerCrunchResult(recommendedDesks, _))) => recommendedDesks
              case _ => IndexedSeq()
            }
          }.toList
      }

      resultSummary === expected
    }
  }

  "Code shares " >> {
    "Given 2 flights which are codeshares with each other " +
      "When I ask for a crunch " +
      "Then I should see workload representing only the flight with the highest passenger numbers" >> {
      val scheduled = "2017-01-01T00:00Z"
      val flightsWithSplits = List(
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled, iata = "BA0001"),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 10d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled, iata = "FR8819"),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 10d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

      val processingTime = 10d / 60
      val procTimes: Map[PaxTypeAndQueue, Double] = Map(eeaMachineReadableToDesk -> processingTime)

      val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 25)
      val minMaxDesks = Map("T1" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(0), List.fill[Int](24)(20)))))

      val probe = TestProbe()
      val startTime = SDate(scheduled).millisSinceEpoch
      val endTime = startTime + (29 * oneMinute)

      val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
      publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

      val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

      val expected = Seq(
        10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

      val resultSummary = result match {
        case CrunchState(_, workloads, _, _) =>
          workloads.values.flatMap {
            case twl => twl.values.flatMap {
              case qwl => qwl.map(_._2._1)
            }
          }
      }

      resultSummary === expected
    }

    "Given flights some of which are code shares with each other " +
      "When I ask for a crunch " +
      "Then I should see workload correctly split to the appropriate terminals, and having accounted for code shares" >> {
      val scheduled00 = "2017-01-01T00:00Z"
      val scheduled15 = "2017-01-01T00:15Z"
      val flightsWithSplits = List(
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled00, iata = "BA0001", terminal = "T1", actPax = 15),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 15d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled00, iata = "FR8819", terminal = "T1", actPax = 10),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 10d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled15, iata = "EZ1010", terminal = "T2", actPax = 12),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 12d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

      val processingTime = 10d / 60
      val procTimes: Map[PaxTypeAndQueue, Double] = Map(eeaMachineReadableToDesk -> processingTime)

      val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 25)
      val minMaxDesks = Map("T1" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(0), List.fill[Int](24)(20)))))

      val probe = TestProbe()
      val startTime = SDate(scheduled00).millisSinceEpoch
      val endTime = startTime + (29 * oneMinute)

      val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, uniquifyArrivals, validTerminals))
      publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

      val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

      val expected = Map(
        "T1" -> Map(Queues.EeaDesk -> Seq(
          15.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
        "T2" -> Map(Queues.EeaDesk -> Seq(
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          12.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)))


      val resultSummary = result match {
        case CrunchState(_, workloads, _, _) =>
          workloads.mapValues {
            case twl => twl.mapValues {
              case qwl => qwl.map(_._2._1)
            }
          }
      }

      resultSummary === expected
    }


    "Given flights some of which are code shares with each other " +
      "When I ask for a crunch " +
      "Then I should see workload correctly split to the appropriate terminals, and having accounted for code shares" >> {
      val scheduled00 = "2017-01-01T00:00Z"
      val scheduled15 = "2017-01-01T00:15Z"
      val flightsWithSplits = List(
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled00, iata = "BA0001", terminal = "T1", actPax = 15),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 15d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))),
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled15, iata = "EZ1010", terminal = "xxx", actPax = 12),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 12d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

      val processingTime = 10d / 60
      val procTimes: Map[PaxTypeAndQueue, Double] = Map(eeaMachineReadableToDesk -> processingTime)

      val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 25)
      val minMaxDesks = Map("T1" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(0), List.fill[Int](24)(20)))))

      val probe = TestProbe()
      val startTime = SDate(scheduled00).millisSinceEpoch
      val endTime = startTime + (29 * oneMinute)

      val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, CodeShares.uniqueArrivalsWithCodeShares((f: ApiFlightWithSplits) => f.apiFlight), validTerminals))
      publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

      val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

      val expected = Map(
        "T1" -> Map(Queues.EeaDesk -> Seq(
          15.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)))


      val resultSummary = result match {
        case CrunchState(_, workloads, _, _) =>
          workloads.mapValues {
            case twl => twl.mapValues {
              case qwl => qwl.map(_._2._1)
            }
          }
      }

      resultSummary === expected
    }
  }

  "Queue validation " >> {
    "Given a flight with transfers " +
      "When I ask for a crunch " +
      "Then I should see only the non-transfer queue" >> {
      val scheduled00 = "2017-01-01T00:00Z"
      val scheduled15 = "2017-01-01T00:15Z"
      val flightsWithSplits = List(
        ApiFlightWithSplits(
          ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled00, iata = "BA0001", terminal = "T1", actPax = 15),
          List(ApiSplits(List(
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 15d),
            ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.Transfer, 5d)
          ), SplitSources.ApiSplitsWithCsvPercentage, PaxNumbers))))

      val processingTime = 10d / 60
      val procTimes: Map[PaxTypeAndQueue, Double] = Map(eeaMachineReadableToDesk -> processingTime)

      val slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 25)
      val minMaxDesks = Map("T1" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(0), List.fill[Int](24)(20)))))

      val probe = TestProbe()
      val startTime = SDate(scheduled00).millisSinceEpoch
      val endTime = startTime + (29 * oneMinute)

      val publisher: Publisher = Publisher(probe.ref, new CrunchStateFlow(slaByQueue, minMaxDesks, procTimes, CodeShares.uniqueArrivalsWithCodeShares((f: ApiFlightWithSplits) => f.apiFlight), validTerminals))
      publisher.publish(CrunchFlights(flightsWithSplits, startTime, endTime))

      val result = probe.expectMsgAnyClassOf(classOf[CrunchState])

      val expected = Set(Queues.EeaDesk)

      val resultSummary: Set[QueueName] = result match {
        case CrunchState(_, workloads, _, _) =>
          workloads.values.flatMap {
            case twl => twl.keys
          }.toSet
      }

      resultSummary === expected
    }
  }

  "Given a list of QueueLoadMinutes corresponding to the same queue & minute " +
    "When I ask for them as a set " +
    "Then I should see a single QueueLoadMinute wth the loads summed up" >> {
    val qlm = List(
      QueueLoadMinute("T1", "EeaDesk", 1.0, 1.5, 1L),
      QueueLoadMinute("T1", "EeaDesk", 1.0, 1.5, 1L))

    val result = collapseQueueLoadMinutesToSet(qlm)
    val expected = Set(QueueLoadMinute("T1", "EeaDesk", 2.0, 3.0, 1L))

    result === expected
  }
}

