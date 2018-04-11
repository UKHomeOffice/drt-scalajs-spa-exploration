package services.crunch

import actors.{GetState, StaffMovements}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.AskableActorRef
import akka.stream.scaladsl.{Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.Timeout
import drt.shared.CrunchApi.{CrunchMinutes, PortState, StaffMinutes}
import drt.shared.FlightsApi.{Flights, FlightsWithSplits}
import drt.shared.{SDateLike, _}
import org.slf4j.{Logger, LoggerFactory}
import passengersplits.core.SplitsCalculator
import services._
import services.graphstages.Crunch._
import services.graphstages._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}


case class CrunchSystem[MS](shifts: SourceQueueWithComplete[String],
                            fixedPoints: SourceQueueWithComplete[String],
                            staffMovements: SourceQueueWithComplete[Seq[StaffMovement]],
                            baseArrivals: SourceQueueWithComplete[Flights],
                            forecastArrivals: SourceQueueWithComplete[Flights],
                            liveArrivals: SourceQueueWithComplete[Flights],
                            manifests: MS,
                            actualDeskStats: SourceQueueWithComplete[ActualDeskStats]
                           )

case class CrunchProps[MS](logLabel: String = "",
                           system: ActorSystem,
                           airportConfig: AirportConfig,
                           pcpArrival: Arrival => MilliDate,
                           historicalSplitsProvider: SplitsProvider.SplitProvider,
                           liveCrunchStateActor: ActorRef,
                           forecastCrunchStateActor: ActorRef,
                           maxDaysToCrunch: Int,
                           expireAfterMillis: Long,
                           minutesToCrunch: Int = 1440,
                           crunchOffsetMillis: Long = 0,
                           actors: Map[String, AskableActorRef],
                           useNationalityBasedProcessingTimes: Boolean,
                           now: () => SDateLike = () => SDate.now(),
                           initialFlightsWithSplits: Option[FlightsWithSplits] = None,
                           splitsPredictorStage: SplitsPredictorBase,
                           manifestsSource: Source[DqManifests, MS],
                           voyageManifestsActor: ActorRef,
                           cruncher: TryCrunch,
                           simulator: Simulator,
                           initialPortState: Option[PortState] = None
                          )

object CrunchSystem {

  val log: Logger = LoggerFactory.getLogger(getClass)

  def apply[MS](props: CrunchProps[MS]): CrunchSystem[MS] = {
    val baseArrivalsActor: ActorRef = props.system.actorOf(Props(classOf[ForecastBaseArrivalsActor], props.now, props.expireAfterMillis), name = "base-arrivals-actor")
    val forecastArrivalsActor: ActorRef = props.system.actorOf(Props(classOf[ForecastPortArrivalsActor], props.now, props.expireAfterMillis), name = "forecast-arrivals-actor")
    val liveArrivalsActor: ActorRef = props.system.actorOf(Props(classOf[LiveArrivalsActor], props.now, props.expireAfterMillis), name = "live-arrivals-actor")

    val initialShifts = initialShiftsLikeState(props.actors("shifts"))
    val initialFixedPoints = initialShiftsLikeState(props.actors("fixed-points"))
    val initialStaffMovements = initialStaffMovementsState(props.actors("staff-movements"))

    val baseArrivals: Source[Flights, SourceQueueWithComplete[Flights]] = Source.queue[Flights](1, OverflowStrategy.backpressure)
    val forecastArrivals: Source[Flights, SourceQueueWithComplete[Flights]] = Source.queue[Flights](1, OverflowStrategy.backpressure)
    val liveArrivals: Source[Flights, SourceQueueWithComplete[Flights]] = Source.queue[Flights](1, OverflowStrategy.backpressure)
    val manifests = props.manifestsSource
    val shiftsSource: Source[String, SourceQueueWithComplete[String]] = Source.queue[String](1, OverflowStrategy.backpressure)
    val fixedPointsSource: Source[String, SourceQueueWithComplete[String]] = Source.queue[String](1, OverflowStrategy.backpressure)
    val staffMovementsSource: Source[Seq[StaffMovement], SourceQueueWithComplete[Seq[StaffMovement]]] = Source.queue[Seq[StaffMovement]](1, OverflowStrategy.backpressure)
    val actualDesksAndQueuesSource: Source[ActualDeskStats, SourceQueueWithComplete[ActualDeskStats]] = Source.queue[ActualDeskStats](1, OverflowStrategy.backpressure)

    val splitsCalculator = SplitsCalculator(props.airportConfig.portCode, props.historicalSplitsProvider, props.airportConfig.defaultPaxSplits.splits.toSet)
    val groupFlightsByCodeShares = CodeShares.uniqueArrivalsWithCodeShares((f: ApiFlightWithSplits) => f.apiFlight) _
    val crunchStartDateProvider: (SDateLike) => SDateLike = s => Crunch.getLocalLastMidnight(s).addMinutes(props.airportConfig.crunchOffsetMinutes)

    val maybeStaffMinutes = initialStaffMinutesFromPortState(props.initialPortState)
    val maybeCrunchMinutes = initialCrunchMinutesFromPortState(props.initialPortState)

    val arrivalsStage = new ArrivalsGraphStage(
      name = props.logLabel,
      initialBaseArrivals = initialArrivals(baseArrivalsActor),
      initialForecastArrivals = initialArrivals(forecastArrivalsActor),
      initialLiveArrivals = initialArrivals(liveArrivalsActor),
      pcpArrivalTime = props.pcpArrival,
      validPortTerminals = props.airportConfig.terminalNames.toSet,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now)

    val arrivalSplitsGraphStage = new ArrivalSplitsGraphStage(
      name = props.logLabel,
      optionalInitialFlights = initialFlightsFromPortState(props.initialPortState),
      splitsCalculator = splitsCalculator,
      groupFlightsByCodeShares = groupFlightsByCodeShares,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now,
      maxDaysToCrunch = props.maxDaysToCrunch)

    val splitsPredictorStage = props.splitsPredictorStage

    val staffGraphStage = new StaffGraphStage(
      name = props.logLabel,
      optionalInitialShifts = Option(initialShifts),
      optionalInitialFixedPoints = Option(initialFixedPoints),
      optionalInitialMovements = Option(initialStaffMovements),
      now = props.now,
      expireAfterMillis = props.expireAfterMillis,
      airportConfig = props.airportConfig,
      numberOfDays = props.maxDaysToCrunch)

    val staffBatcher = new StaffBatchUpdateGraphStage(props.now, props.expireAfterMillis)
    val loadBatcher = new LoadBatchUpdateGraphStage(props.now, props.expireAfterMillis, crunchStartDateProvider)

    val workloadGraphStage = new WorkloadGraphStage(
      name = props.logLabel,
      optionalInitialLoads = initialLoadsFromPortState(props.initialPortState),
      optionalInitialFlightsWithSplits = initialFlightsFromPortState(props.initialPortState),
      airportConfig = props.airportConfig,
      natProcTimes = AirportConfigs.nationalityProcessingTimes,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now,
      useNationalityBasedProcessingTimes = props.useNationalityBasedProcessingTimes)

    val crunchLoadGraphStage = new CrunchLoadGraphStage(
      name = props.logLabel,
      optionalInitialCrunchMinutes = maybeCrunchMinutes,
      airportConfig = props.airportConfig,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now,
      crunch = props.cruncher,
      crunchPeriodStartMillis = crunchStartDateProvider,
      minutesToCrunch = props.minutesToCrunch)

    val simulationGraphStage = new SimulationGraphStage(
      name = props.logLabel,
      optionalInitialCrunchMinutes = maybeCrunchMinutes,
      optionalInitialStaffMinutes = maybeStaffMinutes,
      airportConfig = props.airportConfig,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now,
      simulate = props.simulator,
      crunchPeriodStartMillis = crunchStartDateProvider,
      minutesToCrunch = props.minutesToCrunch)

    val portStateGraphStage = new PortStateGraphStage(
      name = props.logLabel,
      optionalInitialPortState = props.initialPortState,
      airportConfig = props.airportConfig,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now)

    val crunchSystem = RunnableCrunch(
      baseArrivals, forecastArrivals, liveArrivals, manifests, shiftsSource, fixedPointsSource, staffMovementsSource, actualDesksAndQueuesSource,
      arrivalsStage, arrivalSplitsGraphStage, splitsPredictorStage, workloadGraphStage, loadBatcher, crunchLoadGraphStage, staffGraphStage, staffBatcher, simulationGraphStage, portStateGraphStage,
      baseArrivalsActor, forecastArrivalsActor, liveArrivalsActor,
      props.voyageManifestsActor,
      props.liveCrunchStateActor, props.forecastCrunchStateActor,
      crunchStartDateProvider, props.now
    )

    implicit val actorSystem: ActorSystem = props.system
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val (baseIn, fcstIn, liveIn, manifestsIn, shiftsIn, fixedPointsIn, movementsIn, actDesksIn) = crunchSystem.run

    CrunchSystem(
      shifts = shiftsIn,
      fixedPoints = fixedPointsIn,
      staffMovements = movementsIn,
      baseArrivals = baseIn,
      forecastArrivals = fcstIn,
      liveArrivals = liveIn,
      manifests = manifestsIn,
      actualDeskStats = actDesksIn
    )
  }

  def initialStaffMinutesFromPortState(initialPortState: Option[PortState]): Option[StaffMinutes] = initialPortState.map(ps => StaffMinutes(ps.staffMinutes))

  def initialCrunchMinutesFromPortState(initialPortState: Option[PortState]): Option[CrunchMinutes] = initialPortState.map(ps => CrunchMinutes(ps.crunchMinutes.values.toSet))

  def initialLoadsFromPortState(initialPortState: Option[PortState]): Option[Loads] = initialPortState.map(ps => Loads(ps.crunchMinutes.values.toSeq))

  def initialFlightsFromPortState(initialPortState: Option[PortState]): Option[FlightsWithSplits] = initialPortState.map(ps => FlightsWithSplits(ps.flights.values.toSeq))

  def initialShiftsLikeState(askableShiftsLikeActor: AskableActorRef): String = {
    Await.result(askableShiftsLikeActor.ask(GetState)(new Timeout(5 minutes)).map {
      case shifts: String if shifts.nonEmpty =>
        log.info(s"Got initial state from ${askableShiftsLikeActor.toString}")
        shifts
      case _ =>
        log.info(s"Got no initial state from ${askableShiftsLikeActor.toString}")
        ""
    }, 5 minutes)
  }

  def initialStaffMovementsState(askableStaffMovementsActor: AskableActorRef): Seq[StaffMovement] = {
    Await.result(askableStaffMovementsActor.ask(GetState)(new Timeout(5 minutes)).map {
      case StaffMovements(mms) if mms.nonEmpty =>
        log.info(s"Got initial state from ${askableStaffMovementsActor.toString}")
        mms
      case _ =>
        log.info(s"Got no initial state from ${askableStaffMovementsActor.toString}")
        Seq()
    }, 5 minutes)
  }

  def initialArrivals(arrivalsActor: AskableActorRef): Set[Arrival] = {
    val canWaitMinutes = 5
    val arrivalsFuture: Future[Set[Arrival]] = arrivalsActor.ask(GetState)(new Timeout(canWaitMinutes minutes)).map {
      case ArrivalsState(arrivals) => arrivals.values.toSet
      case _ => Set[Arrival]()
    }
    arrivalsFuture.onComplete {
      case Success(arrivals) => arrivals
      case Failure(t) =>
        log.warn(s"Failed to get an initial ArrivalsState: $t")
        Set[Arrival]()
    }
    Await.result(arrivalsFuture, canWaitMinutes minutes)
  }
}
