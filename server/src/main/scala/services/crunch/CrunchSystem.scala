package services.crunch

import actors.daily.PaxDeltas
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.AskableActorRef
import akka.stream._
import akka.stream.scaladsl.{Sink, Source, SourceQueueWithComplete}
import drt.chroma.ArrivalsDiffingStage
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared.{SDateLike, _}
import manifests.passengers.BestAvailableManifest
import org.slf4j.{Logger, LoggerFactory}
import queueus._
import server.feeds.{ArrivalsFeedResponse, ManifestsFeedResponse}
import services.SplitsProvider.SplitProvider
import services._
import services.arrivals.ArrivalDataSanitiser
import services.crunch.deskrecs.DesksAndWaitsPortProvider
import services.graphstages.Crunch._
import services.graphstages._

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._


case class CrunchSystem[FR](shifts: SourceQueueWithComplete[ShiftAssignments],
                            fixedPoints: SourceQueueWithComplete[FixedPointAssignments],
                            staffMovements: SourceQueueWithComplete[Seq[StaffMovement]],
                            loadsToSimulate: ActorRef,
                            forecastBaseArrivalsResponse: FR,
                            forecastArrivalsResponse: FR,
                            liveBaseArrivalsResponse: FR,
                            liveArrivalsResponse: FR,
                            manifestsLiveResponse: SourceQueueWithComplete[ManifestsFeedResponse],
                            actualDeskStats: SourceQueueWithComplete[ActualDeskStats],
                            killSwitches: List[UniqueKillSwitch]
                           )

case class CrunchProps[FR](logLabel: String = "",
                           airportConfig: AirportConfig,
                           pcpArrival: Arrival => MilliDate,
                           historicalSplitsProvider: SplitProvider,
                           portStateActor: ActorRef,
                           maxDaysToCrunch: Int,
                           expireAfterMillis: Int,
                           crunchOffsetMillis: MillisSinceEpoch = 0,
                           actors: Map[String, AskableActorRef],
                           useNationalityBasedProcessingTimes: Boolean,
                           useLegacyManifests: Boolean = false,
                           now: () => SDateLike = () => SDate.now(),
                           initialFlightsWithSplits: Option[FlightsWithSplits] = None,
                           manifestsLiveSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]],
                           manifestResponsesSource: Source[List[BestAvailableManifest], NotUsed],
                           voyageManifestsActor: ActorRef,
                           manifestRequestsSink: Sink[List[Arrival], NotUsed],
                           simulator: Simulator,
                           initialPortState: Option[PortState] = None,
                           initialForecastBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap[UniqueArrival, Arrival](),
                           initialForecastArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap[UniqueArrival, Arrival](),
                           initialLiveBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap[UniqueArrival, Arrival](),
                           initialLiveArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap[UniqueArrival, Arrival](),
                           arrivalsForecastBaseSource: Source[ArrivalsFeedResponse, FR],
                           arrivalsForecastSource: Source[ArrivalsFeedResponse, FR],
                           arrivalsLiveBaseSource: Source[ArrivalsFeedResponse, FR],
                           arrivalsLiveSource: Source[ArrivalsFeedResponse, FR],
                           passengersActorProvider: () => AskableActorRef,
                           initialShifts: ShiftAssignments = ShiftAssignments(Seq()),
                           initialFixedPoints: FixedPointAssignments = FixedPointAssignments(Seq()),
                           initialStaffMovements: Seq[StaffMovement] = Seq(),
                           refreshArrivalsOnStart: Boolean,
                           checkRequiredStaffUpdatesOnStartup: Boolean,
                           stageThrottlePer: FiniteDuration,
                           useApiPaxNos: Boolean,
                           adjustEGateUseByUnder12s: Boolean,
                           optimiser: TryCrunch,
                           useLegacyDeployments: Boolean,
                           aclPaxAdjustmentDays: Int)

object CrunchSystem {

  val log: Logger = LoggerFactory.getLogger(getClass)

  def apply[FR](props: CrunchProps[FR])
               (implicit materializer: Materializer, system: ActorSystem, ec: ExecutionContext): CrunchSystem[FR] = {

    val shiftsSource: Source[ShiftAssignments, SourceQueueWithComplete[ShiftAssignments]] = Source.queue[ShiftAssignments](10, OverflowStrategy.backpressure)
    val fixedPointsSource: Source[FixedPointAssignments, SourceQueueWithComplete[FixedPointAssignments]] = Source.queue[FixedPointAssignments](10, OverflowStrategy.backpressure)
    val staffMovementsSource: Source[Seq[StaffMovement], SourceQueueWithComplete[Seq[StaffMovement]]] = Source.queue[Seq[StaffMovement]](10, OverflowStrategy.backpressure)
    val actualDesksAndQueuesSource: Source[ActualDeskStats, SourceQueueWithComplete[ActualDeskStats]] = Source.queue[ActualDeskStats](10, OverflowStrategy.backpressure)

    val crunchStartDateProvider: SDateLike => SDateLike = crunchStartWithOffset(props.airportConfig.crunchOffsetMinutes)

    val maybeStaffMinutes = initialStaffMinutesFromPortState(props.initialPortState)
    val maybeCrunchMinutes = initialCrunchMinutesFromPortState(props.initialPortState)

    val initialFlightsWithSplits = initialFlightsFromPortState(props.initialPortState)

    val forecastMaxMillis: () => MillisSinceEpoch = () => props.now().addDays(props.maxDaysToCrunch).millisSinceEpoch

    val arrivalsStage = new ArrivalsGraphStage(
      name = props.logLabel,
      initialForecastBaseArrivals = if (props.refreshArrivalsOnStart) mutable.SortedMap[UniqueArrival, Arrival]() else props.initialForecastBaseArrivals,
      initialForecastArrivals = if (props.refreshArrivalsOnStart) mutable.SortedMap[UniqueArrival, Arrival]() else props.initialForecastArrivals,
      initialLiveBaseArrivals = if (props.refreshArrivalsOnStart) mutable.SortedMap[UniqueArrival, Arrival]() else props.initialLiveBaseArrivals,
      initialLiveArrivals = props.initialLiveArrivals,
      initialMergedArrivals = if (props.refreshArrivalsOnStart) mutable.SortedMap[UniqueArrival, Arrival]() else mutable.SortedMap[UniqueArrival, Arrival]() ++ initialFlightsWithSplits.map(_.flightsToUpdate.map(fws => (fws.apiFlight.unique, fws.apiFlight))).getOrElse(List()),
      pcpArrivalTime = props.pcpArrival,
      validPortTerminals = props.airportConfig.terminals.toSet,
      ArrivalDataSanitiser(
        props.airportConfig.maybeCiriumEstThresholdHours,
        props.airportConfig.maybeCiriumTaxiThresholdMinutes
        ),
      expireAfterMillis = props.expireAfterMillis,
      now = props.now)

    val forecastArrivalsDiffingStage = new ArrivalsDiffingStage(if (props.refreshArrivalsOnStart) mutable.SortedMap[UniqueArrival, Arrival]() else props.initialForecastArrivals, forecastMaxMillis)
    val liveBaseArrivalsDiffingStage = new ArrivalsDiffingStage(if (props.refreshArrivalsOnStart) mutable.SortedMap[UniqueArrival, Arrival]() else props.initialLiveBaseArrivals, forecastMaxMillis)
    val liveArrivalsDiffingStage = new ArrivalsDiffingStage(if (props.refreshArrivalsOnStart) mutable.SortedMap[UniqueArrival, Arrival]() else props.initialLiveArrivals, forecastMaxMillis)

    val ptqa = if (props.airportConfig.hasTransfer)
      PaxTypeQueueAllocation(
        B5JPlusWithTransitTypeAllocator(),
        TerminalQueueAllocator(props.airportConfig.terminalPaxTypeQueueAllocation))
    else
      PaxTypeQueueAllocation(
        B5JPlusTypeAllocator(),
        TerminalQueueAllocator(props.airportConfig.terminalPaxTypeQueueAllocation))

    val splitAdjustments = if (props.adjustEGateUseByUnder12s)
      ChildEGateAdjustments(props.airportConfig.assumedAdultsPerChild)
    else
      AdjustmentsNoop()

    val arrivalSplitsGraphStage = new ArrivalSplitsGraphStage(
      name = props.logLabel,
      optionalInitialFlights = initialFlightsWithSplits,
      splitsCalculator = manifests.queues.SplitsCalculator(ptqa, props.airportConfig.terminalPaxSplits, splitAdjustments),
      expireAfterMillis = props.expireAfterMillis,
      now = props.now,
      useApiPaxNos = props.useApiPaxNos
      )

    val staffGraphStage = new StaffGraphStage(
      name = props.logLabel,
      initialShifts = props.initialShifts,
      initialFixedPoints = props.initialFixedPoints,
      optionalInitialMovements = Option(props.initialStaffMovements),
      initialStaffMinutes = maybeStaffMinutes.getOrElse(StaffMinutes(Seq())),
      now = props.now,
      expireAfterMillis = props.expireAfterMillis,
      airportConfig = props.airportConfig,
      numberOfDays = props.maxDaysToCrunch,
      checkRequiredUpdatesOnStartup = props.checkRequiredStaffUpdatesOnStartup)

    val staffBatcher = new StaffBatchUpdateGraphStage(props.now, props.expireAfterMillis, props.airportConfig.crunchOffsetMinutes)

    val deploymentGraphStage = if (props.useLegacyDeployments)
      new LegacyDeploymentGraphStage(
        name = props.logLabel,
        optionalInitialCrunchMinutes = maybeCrunchMinutes,
        optionalInitialStaffMinutes = maybeStaffMinutes,
        airportConfig = props.airportConfig,
        expireAfterMillis = props.expireAfterMillis,
        now = props.now,
        simulate = props.simulator,
        crunchPeriodStartMillis = crunchStartDateProvider)
    else
      new DeploymentGraphStage(
        name = props.logLabel,
        optionalInitialCrunchMinutes = maybeCrunchMinutes,
        optionalInitialStaffMinutes = maybeStaffMinutes,
        airportConfig = props.airportConfig,
        expireAfterMillis = props.expireAfterMillis,
        now = props.now,
        crunchPeriodStartMillis = crunchStartDateProvider,
        DesksAndWaitsPortProvider(props.airportConfig, props.optimiser))

    val crunchSystem = RunnableCrunch(
      props.arrivalsForecastBaseSource, props.arrivalsForecastSource, props.arrivalsLiveBaseSource, props.arrivalsLiveSource,
      props.manifestsLiveSource, props.manifestResponsesSource,
      shiftsSource, fixedPointsSource, staffMovementsSource,
      actualDesksAndQueuesSource,
      arrivalsStage, arrivalSplitsGraphStage,
      staffGraphStage, staffBatcher, deploymentGraphStage,
      forecastArrivalsDiffingStage, liveBaseArrivalsDiffingStage, liveArrivalsDiffingStage,
      props.actors("forecast-base-arrivals").actorRef, props.actors("forecast-arrivals").actorRef, props.actors("live-base-arrivals").actorRef, props.actors("live-arrivals").actorRef,
      PaxDeltas.applyAdjustmentsToArrivals(props.passengersActorProvider, props.aclPaxAdjustmentDays),
      props.voyageManifestsActor, props.manifestRequestsSink,
      props.portStateActor,
      props.actors("aggregated-arrivals").actorRef,
      forecastMaxMillis, props.stageThrottlePer
      )

    val (forecastBaseIn, forecastIn, liveBaseIn, liveIn, manifestsLiveIn, shiftsIn, fixedPointsIn, movementsIn, simloadsIn, actDesksIn, arrivalsKillSwitch, manifestsKillSwitch, shiftsKS, fixedPKS, movementsKS) = crunchSystem.run

    CrunchSystem(
      shifts = shiftsIn,
      fixedPoints = fixedPointsIn,
      staffMovements = movementsIn,
      loadsToSimulate = simloadsIn,
      forecastBaseArrivalsResponse = forecastBaseIn,
      forecastArrivalsResponse = forecastIn,
      liveBaseArrivalsResponse = liveBaseIn,
      liveArrivalsResponse = liveIn,
      manifestsLiveResponse = manifestsLiveIn,
      actualDeskStats = actDesksIn,
      List(arrivalsKillSwitch, manifestsKillSwitch, shiftsKS, fixedPKS, movementsKS)
      )
  }

  def initialStaffMinutesFromPortState(initialPortState: Option[PortState]): Option[StaffMinutes] = initialPortState.map(
    ps => StaffMinutes(ps.staffMinutes))

  def initialCrunchMinutesFromPortState(initialPortState: Option[PortState]): Option[CrunchMinutes] = initialPortState.map(
    ps => CrunchMinutes(ps.crunchMinutes.values.toSet))

  def initialLoadsFromPortState(initialPortState: Option[PortState]): Option[Loads] = initialPortState.map(ps => Loads.fromCrunchMinutes(ps.crunchMinutes))

  def initialFlightsFromPortState(initialPortState: Option[PortState]): Option[FlightsWithSplits] = initialPortState.map { ps =>
    FlightsWithSplits(ps.flights.values.toList, List())
  }
}
