package services.crunch

import actors.Sizes.oneMegaByte
import actors._
import actors.acking.AckingReceiver.Ack
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.AskableActorRef
import akka.stream.QueueOfferResult.Enqueued
import akka.stream.scaladsl.{Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy, QueueOfferResult, UniqueKillSwitch}
import akka.testkit.{TestKit, TestProbe}
import drt.shared.CrunchApi._
import drt.shared.PaxTypes._
import drt.shared.PaxTypesAndQueues._
import drt.shared.Queues.Queue
import drt.shared.SplitRatiosNs.{SplitRatio, SplitRatios, SplitSources}
import drt.shared.Terminals.{T1, T2, Terminal}
import drt.shared._
import graphs.SinkToSourceBridge
import manifests.passengers.BestAvailableManifest
import org.slf4j.{Logger, LoggerFactory}
import org.specs2.mutable.SpecificationLike
import server.feeds.{ArrivalsFeedResponse, ManifestsFeedResponse}
import services._
import services.crunch.desklimits.TerminalDeskLimitsLike
import services.crunch.desklimits.flexed.FlexedPortDeskLimits
import services.crunch.deskrecs.RunnableDeskRecs
import services.crunch.deskrecs.fixed.FixedPortDeskRecsProvider
import services.crunch.deskrecs.flexed.FlexedPortDeskRecsProvider
import services.graphstages.CrunchMocks
import slickdb.Tables

import scala.collection.immutable.{Map, SortedMap}
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}


class CrunchStateMockActor extends Actor {
  override def receive: Receive = {
    case _ => sender() ! Ack
  }
}

class PortStateTestActor(liveActor: ActorRef, forecastActor: ActorRef, probe: ActorRef, now: () => SDateLike, liveDaysAhead: Int)
  extends PortStateActor(liveActor, forecastActor, now, liveDaysAhead) {
  override def splitDiffAndSend(diff: PortStateDiff): Unit = {
    super.splitDiffAndSend(diff)
    probe ! state.immutable
  }
}

object PortStateTestActor {
  def props(liveActor: ActorRef, forecastActor: ActorRef, probe: ActorRef, now: () => SDateLike, liveDaysAhead: Int) =
    Props(new PortStateTestActor(liveActor, forecastActor, probe, now, liveDaysAhead))
}

case class CrunchGraphInputsAndProbes(baseArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      forecastArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      liveArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      manifestsLiveInput: SourceQueueWithComplete[ManifestsFeedResponse],
                                      shiftsInput: SourceQueueWithComplete[ShiftAssignments],
                                      fixedPointsInput: SourceQueueWithComplete[FixedPointAssignments],
                                      liveStaffMovementsInput: SourceQueueWithComplete[Seq[StaffMovement]],
                                      forecastStaffMovementsInput: SourceQueueWithComplete[Seq[StaffMovement]],
                                      actualDesksAndQueuesInput: SourceQueueWithComplete[ActualDeskStats],
                                      portStateTestProbe: TestProbe,
                                      baseArrivalsTestProbe: TestProbe,
                                      forecastArrivalsTestProbe: TestProbe,
                                      liveArrivalsTestProbe: TestProbe,
                                      aggregatedArrivalsActor: ActorRef,
                                      portStateActor: ActorRef,
                                      deskRecsGraphKillSwitch: UniqueKillSwitch) {
  def shutdown(): Unit = {
    baseArrivalsInput.complete()
    forecastArrivalsInput.complete()
    liveArrivalsInput.complete()
    manifestsLiveInput.complete()
    shiftsInput.complete()
    fixedPointsInput.complete()
    liveStaffMovementsInput.complete()
    forecastStaffMovementsInput.complete()
    actualDesksAndQueuesInput.complete()
    deskRecsGraphKillSwitch.shutdown()
  }
}


object H2Tables extends {
  val profile = slick.jdbc.H2Profile
} with Tables

class CrunchTestLike
  extends TestKit(ActorSystem("StreamingCrunchTests"))
    with SpecificationLike {
  isolated
  sequential

  implicit val actorSystem: ActorSystem = system
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  val log: Logger = LoggerFactory.getLogger(getClass)

  val crunchStateMockActor: ActorRef = system.actorOf(Props(classOf[CrunchStateMockActor]), "crunch-state-mock")

  val oneMinuteMillis = 60000
  val uniquifyArrivals: Seq[ApiFlightWithSplits] => List[(ApiFlightWithSplits, Set[Arrival])] =
    CodeShares.uniqueArrivalsWithCodeShares((f: ApiFlightWithSplits) => f.apiFlight)

  val defaultAirportConfig = AirportConfig(
    portCode = PortCode("STN"),
    queuesByTerminal = SortedMap(T1 -> Seq(Queues.EeaDesk, Queues.NonEeaDesk), T2 -> Seq(Queues.EeaDesk, Queues.NonEeaDesk)),
    slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 20, Queues.NonEeaDesk -> 45),
    defaultWalkTimeMillis = Map(),
    terminalPaxSplits = List(T1, T2).map(t => (t, SplitRatios(
      SplitSources.TerminalAverage,
      SplitRatio(eeaMachineReadableToDesk, 1)
    ))).toMap,
    terminalProcessingTimes = Map(
      T1 -> Map(
        eeaMachineReadableToDesk -> 25d / 60,
        eeaNonMachineReadableToDesk -> 25d / 60
      ),
      T2 -> Map(
        eeaMachineReadableToDesk -> 25d / 60,
        eeaNonMachineReadableToDesk -> 25d / 60
      )
    ),
    minMaxDesksByTerminalQueue24Hrs = Map(
      T1 -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.NonEeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20)))),
      T2 -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.NonEeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))))),
    timeToChoxMillis = 120000L,
    firstPaxOffMillis = 180000L,
    role = STNAccess,
    terminalPaxTypeQueueAllocation = Map(
      T1 -> Map(
        EeaMachineReadable -> List(Queues.EGate -> 0.8, Queues.EeaDesk -> 0.2),
        EeaBelowEGateAge -> List(Queues.EeaDesk -> 1.0),
        EeaNonMachineReadable -> List(Queues.EeaDesk -> 1.0),
        NonVisaNational -> List(Queues.NonEeaDesk -> 1.0),
        VisaNational -> List(Queues.NonEeaDesk -> 1.0),
        B5JPlusNational -> List(Queues.EGate -> 0.6, Queues.EeaDesk -> 0.4),
        B5JPlusNationalBelowEGateAge -> List(Queues.EeaDesk -> 1)
      ),
      T2 -> Map(
        EeaMachineReadable -> List(Queues.EeaDesk -> 1),
        EeaBelowEGateAge -> List(Queues.EeaDesk -> 1.0),
        EeaNonMachineReadable -> List(Queues.EeaDesk -> 1.0),
        NonVisaNational -> List(Queues.NonEeaDesk -> 1.0),
        VisaNational -> List(Queues.NonEeaDesk -> 1.0),
        B5JPlusNational -> List(Queues.EeaDesk -> 1),
        B5JPlusNationalBelowEGateAge -> List(Queues.EeaDesk -> 1)
      )
    ),
    desksByTerminal = Map(T1 -> 40, T2 -> 40)
    )

  val pcpForFlightFromSch: Arrival => MilliDate = (a: Arrival) => MilliDate(SDate(a.Scheduled).millisSinceEpoch)
  val pcpForFlightFromBest: Arrival => MilliDate = (a: Arrival) => {
    if (a.ActualChox.isDefined) MilliDate(SDate(a.ActualChox.get).millisSinceEpoch)
    else if (a.EstimatedChox.isDefined) MilliDate(SDate(a.EstimatedChox.get).millisSinceEpoch)
    else if (a.Actual.isDefined) MilliDate(SDate(a.Actual.get).millisSinceEpoch)
    else if (a.Estimated.isDefined) MilliDate(SDate(a.Estimated.get).millisSinceEpoch)
    else MilliDate(SDate(a.Scheduled).millisSinceEpoch)
  }

  def createPortStateActor(testProbe: TestProbe, now: () => SDateLike): ActorRef = {
    system.actorOf(PortStateTestActor.props(crunchStateMockActor, crunchStateMockActor, testProbe.ref, now, 100), name = "port-state-actor")
  }

  def testProbe(name: String) = TestProbe(name = name)

  def runCrunchGraph(initialForecastBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap(),
                     initialForecastArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap(),
                     initialLiveBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap(),
                     initialLiveArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap(),
                     initialPortState: Option[PortState] = None,
                     airportConfig: AirportConfig = defaultAirportConfig,
                     csvSplitsProvider: SplitsProvider.SplitProvider = (_, _) => None,
                     pcpArrivalTime: Arrival => MilliDate = pcpForFlightFromSch,
                     expireAfterMillis: Int = DrtStaticParameters.expireAfterMillis,
                     now: () => SDateLike,
                     initialShifts: ShiftAssignments = ShiftAssignments.empty,
                     initialFixedPoints: FixedPointAssignments = FixedPointAssignments.empty,
                     initialStaffMovements: Seq[StaffMovement] = Seq(),
                     logLabel: String = "",
                     cruncher: TryCrunch = CrunchMocks.mockCrunch,
                     simulator: Simulator = CrunchMocks.mockSimulator,
                     aggregatedArrivalsActor: ActorRef = testProbe("aggregated-arrivals").ref,
                     useLegacyManifests: Boolean = false,
                     maxDaysToCrunch: Int = 2,
                     checkRequiredStaffUpdatesOnStartup: Boolean = false,
                     refreshArrivalsOnStart: Boolean = false,
                     recrunchOnStart: Boolean = false,
                     flexDesks: Boolean = false,
                     useLegacyDeployments: Boolean = false
                    ): CrunchGraphInputsAndProbes = {

    airportConfig.assertValid()

    val portStateProbe = testProbe("portstate")
    val forecastBaseArrivalsProbe = testProbe("forecast-base-arrivals")
    val forecastArrivalsProbe = testProbe("forecast-arrivals")
    val liveBaseArrivalsProbe = testProbe("live-base-arrivals")
    val liveArrivalsProbe = testProbe("live-arrivals")

    val shiftsActor: ActorRef = system.actorOf(Props(classOf[ShiftsActor], now, DrtStaticParameters.timeBeforeThisMonth(now)))
    val fixedPointsActor: ActorRef = system.actorOf(Props(classOf[FixedPointsActor], now))
    val staffMovementsActor: ActorRef = system.actorOf(Props(classOf[StaffMovementsActor], now, DrtStaticParameters.time48HoursAgo(now)))
    val snapshotInterval = 1
    val manifestsActor: ActorRef = system.actorOf(Props(classOf[VoyageManifestsActor], oneMegaByte, now, DrtStaticParameters.expireAfterMillis, Option(snapshotInterval)))

    val portStateActor = createPortStateActor(portStateProbe, now)
    initialPortState.foreach(ps => portStateActor ! ps)

    val portDescRecs = if (flexDesks)
      FlexedPortDeskRecsProvider(airportConfig, cruncher)
    else
      FixedPortDeskRecsProvider(airportConfig, cruncher)

    val maxDesksProvider: Map[Terminal, TerminalDeskLimitsLike] = FlexedPortDeskLimits(airportConfig, 1440).maxDesksByTerminal

    val (millisToCrunchActor: ActorRef, deskRecsKillSwitch: UniqueKillSwitch) = RunnableDeskRecs.start(portStateActor, portDescRecs, now, recrunchOnStart, maxDaysToCrunch, maxDesksProvider)
    portStateActor ! SetCrunchActor(millisToCrunchActor)

    val manifestsSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]] = Source.queue[ManifestsFeedResponse](0, OverflowStrategy.backpressure)
    val liveArrivals: Source[ArrivalsFeedResponse, SourceQueueWithComplete[ArrivalsFeedResponse]] = Source.queue[ArrivalsFeedResponse](0, OverflowStrategy.backpressure)
    val liveBaseArrivals: Source[ArrivalsFeedResponse, SourceQueueWithComplete[ArrivalsFeedResponse]] = Source.queue[ArrivalsFeedResponse](0, OverflowStrategy.backpressure)
    val forecastArrivals: Source[ArrivalsFeedResponse, SourceQueueWithComplete[ArrivalsFeedResponse]] = Source.queue[ArrivalsFeedResponse](0, OverflowStrategy.backpressure)
    val forecastBaseArrivals: Source[ArrivalsFeedResponse, SourceQueueWithComplete[ArrivalsFeedResponse]] = Source.queue[ArrivalsFeedResponse](0, OverflowStrategy.backpressure)

    val (_, _, manifestRequestsSink) = SinkToSourceBridge[List[Arrival]]
    val (manifestResponsesSource, _, _) = SinkToSourceBridge[List[BestAvailableManifest]]

    val crunchInputs = CrunchSystem(CrunchProps(logLabel = logLabel, airportConfig = airportConfig, pcpArrival = pcpArrivalTime, historicalSplitsProvider = csvSplitsProvider, portStateActor = portStateActor, maxDaysToCrunch = maxDaysToCrunch, expireAfterMillis = expireAfterMillis, actors = Map[String, AskableActorRef](
            "shifts" -> shiftsActor,
            "fixed-points" -> fixedPointsActor,
            "staff-movements" -> staffMovementsActor,
            "forecast-base-arrivals" -> forecastBaseArrivalsProbe.ref,
            "forecast-arrivals" -> forecastArrivalsProbe.ref,
            "live-base-arrivals" -> liveBaseArrivalsProbe.ref,
            "live-arrivals" -> liveArrivalsProbe.ref,
            "aggregated-arrivals" -> aggregatedArrivalsActor
          ), useNationalityBasedProcessingTimes = false, useLegacyManifests = useLegacyManifests, now = now, manifestsLiveSource = manifestsSource, manifestResponsesSource = manifestResponsesSource, voyageManifestsActor = manifestsActor, manifestRequestsSink = manifestRequestsSink, simulator = simulator, initialPortState = initialPortState, initialForecastBaseArrivals = initialForecastBaseArrivals, initialForecastArrivals = initialForecastArrivals, initialLiveBaseArrivals = initialLiveBaseArrivals, initialLiveArrivals = initialLiveArrivals, arrivalsForecastBaseSource = forecastBaseArrivals, arrivalsForecastSource = forecastArrivals, arrivalsLiveBaseSource = liveBaseArrivals, arrivalsLiveSource = liveArrivals, initialShifts = initialShifts, initialFixedPoints = initialFixedPoints, initialStaffMovements = initialStaffMovements, refreshArrivalsOnStart = refreshArrivalsOnStart, checkRequiredStaffUpdatesOnStartup = checkRequiredStaffUpdatesOnStartup, stageThrottlePer = 50 milliseconds, useApiPaxNos = true, adjustEGateUseByUnder12s = false, optimiser = cruncher, useLegacyDeployments = useLegacyDeployments))

    portStateActor ! SetSimulationActor(crunchInputs.loadsToSimulate)

    CrunchGraphInputsAndProbes(
      baseArrivalsInput = crunchInputs.forecastBaseArrivalsResponse,
      forecastArrivalsInput = crunchInputs.forecastArrivalsResponse,
      liveArrivalsInput = crunchInputs.liveArrivalsResponse,
      manifestsLiveInput = crunchInputs.manifestsLiveResponse,
      shiftsInput = crunchInputs.shifts,
      fixedPointsInput = crunchInputs.fixedPoints,
      liveStaffMovementsInput = crunchInputs.staffMovements,
      forecastStaffMovementsInput = crunchInputs.staffMovements,
      actualDesksAndQueuesInput = crunchInputs.actualDeskStats,
      portStateTestProbe = portStateProbe,
      baseArrivalsTestProbe = forecastBaseArrivalsProbe,
      forecastArrivalsTestProbe = forecastArrivalsProbe,
      liveArrivalsTestProbe = liveArrivalsProbe,
      aggregatedArrivalsActor = aggregatedArrivalsActor,
      portStateActor = portStateActor,
      deskRecsGraphKillSwitch = deskRecsKillSwitch
    )
  }

  def paxLoadsFromPortState(portState: PortState, minsToTake: Int, startFromMinuteIdx: Int = 0): Map[Terminal, Map[Queue, List[Double]]] = portState
    .crunchMinutes
    .values
    .groupBy(_.terminal)
    .map {
      case (tn, tms) =>
        val terminalLoads = tms
          .groupBy(_.queue)
          .map {
            case (qn, qms) =>
              val paxLoad = qms
                .toList
                .sortBy(_.minute)
                .map(_.paxLoad)
                .slice(startFromMinuteIdx, startFromMinuteIdx + minsToTake)
              (qn, paxLoad)
          }
        (tn, terminalLoads)
    }

  def paxLoadsFromPortState(portState: PortState, minsToTake: Int, startFromMinute: SDateLike): Map[Terminal, Map[Queue, List[Double]]] = {
    val startFromMillis = startFromMinute.millisSinceEpoch

    portState
      .crunchMinutes
      .values
      .groupBy(_.terminal)
      .map {
        case (tn, tms) =>
          val terminalLoads = tms
            .groupBy(_.queue)
            .map {
              case (qn, qms) =>
                val startIdx = qms
                  .toList
                  .sortBy(_.minute)
                  .indexWhere(_.minute == startFromMillis)
                val paxLoad = qms
                  .toList
                  .sortBy(_.minute)
                  .map(_.paxLoad)
                  .slice(startIdx, startIdx + minsToTake)
                (qn, paxLoad)
            }
          (tn, terminalLoads)
      }
  }

  def allWorkLoadsFromPortState(portState: PortState): Map[Terminal, Map[Queue, List[Double]]] = portState
    .crunchMinutes
    .values
    .groupBy(_.terminal)
    .map {
      case (tn, tms) =>
        val terminalLoads = tms
          .groupBy(_.queue)
          .map {
            case (qn, qms) =>
              val sortedCms = qms.toList.sortBy(_.minute)
              val workLoad = sortedCms.map(_.workLoad)
              (qn, workLoad)
          }
        (tn, terminalLoads)
    }

  def workLoadsFromPortState(portState: PortState, minsToTake: Int): Map[Terminal, Map[Queue, List[Double]]] = portState
    .crunchMinutes
    .values
    .groupBy(_.terminal)
    .map {
      case (tn, tms) =>
        val terminalLoads = tms
          .groupBy(_.queue)
          .map {
            case (qn, qms) =>
              val sortedCms = qms.toList.sortBy(_.minute)
              val workLoad = sortedCms.map(_.workLoad).take(minsToTake)
              (qn, workLoad)
          }
        (tn, terminalLoads)
    }

  def deskRecsFromPortState(portState: PortState, minsToTake: Int): Map[Terminal, Map[Queue, List[Int]]] = portState
    .crunchMinutes
    .values
    .groupBy(_.terminal)
    .map {
      case (tn, tms) =>
        val terminalLoads = tms
          .groupBy(_.queue)
          .map {
            case (qn, qms) =>
              val sortedCms = qms.toList.sortBy(_.minute)
              val deskRecs = sortedCms.map(_.deskRec).take(minsToTake)
              (qn, deskRecs)
          }
        (tn, terminalLoads)
    }

  def offerAndWait[T](sourceQueue: SourceQueueWithComplete[T], offering: T): QueueOfferResult = {
    Await.result(sourceQueue.offer(offering), 3 seconds) match {
      case offerResult if offerResult != Enqueued =>
        throw new Exception(s"Queue offering (${offering.getClass}) was not enqueued: ${offerResult.getClass}")
      case offerResult =>
        offerResult
    }
  }
}

