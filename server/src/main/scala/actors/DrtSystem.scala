package actors

import actors.Sizes.oneMegaByte
import actors.daily.PassengersActor
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Cancellable, Props, Scheduler}
import akka.pattern.AskableActorRef
import akka.stream.scaladsl.{Sink, Source, SourceQueueWithComplete}
import akka.stream.{Materializer, OverflowStrategy, UniqueKillSwitch}
import akka.util.Timeout
import com.amazonaws.auth.AWSCredentials
import com.typesafe.config.ConfigFactory
import controllers.{Deskstats, PaxFlow, UserRoleProviderLike}
import drt.auth.{Role, Roles}
import drt.chroma._
import drt.chroma.chromafetcher.ChromaFetcher.{ChromaForecastFlight, ChromaLiveFlight}
import drt.chroma.chromafetcher.{ChromaFetcher, ChromaFlightMarshallers}
import drt.http.ProdSendAndReceive
import drt.server.feeds.acl.AclFeed
import drt.server.feeds.api.S3ApiProvider
import drt.server.feeds.bhx.{BHXClient, BHXFeed}
import drt.server.feeds.chroma.{ChromaForecastFeed, ChromaLiveFeed}
import drt.server.feeds.cirium.CiriumFeed
import drt.server.feeds.gla.{GlaFeed, ProdGlaFeedRequester}
import drt.server.feeds.legacy.bhx.{BHXForecastFeedLegacy, BHXLiveFeedLegacy}
import drt.server.feeds.lgw.{LGWAzureClient, LGWFeed, LGWForecastFeed}
import drt.server.feeds.lhr.sftp.LhrSftpLiveContentProvider
import drt.server.feeds.lhr.{LHRFlightFeed, LHRForecastFeed}
import drt.server.feeds.ltn.{LtnFeedRequester, LtnLiveFeed}
import drt.server.feeds.mag.{MagFeed, ProdFeedRequester}
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.FlightsApi.Flights
import drt.shared.MilliTimes._
import drt.shared.Terminals._
import drt.shared._
import graphs.SinkToSourceBridge
import manifests.ManifestLookup
import manifests.actors.{RegisteredArrivals, RegisteredArrivalsActor}
import manifests.graph.{BatchStage, ManifestsGraph}
import manifests.passengers.{BestAvailableManifest, S3ManifestPoller}
import org.apache.spark.sql.SparkSession
import org.joda.time.DateTimeZone
import org.slf4j.{Logger, LoggerFactory}
import play.api.Configuration
import play.api.mvc.{Headers, Session}
import server.feeds.{ArrivalsFeedResponse, ArrivalsFeedSuccess, ManifestsFeedResponse}
import services.PcpArrival.{GateOrStandWalkTime, gateOrStandWalkTimeCalculator, walkTimeMillisProviderFromCsv}
import services.SplitsProvider.SplitProvider
import services._
import services.crunch.desklimits.{PortDeskLimits, TerminalDeskLimitsLike}
import services.crunch.deskrecs.{DesksAndWaitsPortProvider, DesksAndWaitsPortProviderLike, RunnableDeskRecs}
import services.crunch.{CrunchProps, CrunchSystem}
import services.graphstages._
import slickdb.{ArrivalTable, Tables, VoyageManifestPassengerInfoTable}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

trait DrtSystemInterface extends UserRoleProviderLike {
  val now: () => SDateLike = () => SDate.now()

  val portStateActor: ActorRef
  val shiftsActor: ActorRef
  val fixedPointsActor: ActorRef
  val staffMovementsActor: ActorRef
  val alertsActor: ActorRef
  val arrivalsImportActor: ActorRef
  val params: DrtConfigParameters

  val portDeskRecs: DesksAndWaitsPortProviderLike

  val maxDesksProviders: Map[Terminal, TerminalDeskLimitsLike]

  val aclFeed: AclFeed

  def run(): Unit

  val feedActors: Seq[AskableActorRef]

  def isValidFeedSource(fs: FeedSource): Boolean

  def getFeedStatus: Future[Seq[FeedSourceStatuses]]
}

object DrtStaticParameters {
  val expireAfterMillis: Int = 2 * oneDayMillis

  val liveDaysAhead: Int = 2

  def time48HoursAgo(now: () => SDateLike): () => SDateLike = () => now().addDays(-2)

  def timeBeforeThisMonth(now: () => SDateLike): () => SDateLike = () => now().startOfTheMonth()
}

object PostgresTables extends {
  val profile = slick.jdbc.PostgresProfile
} with Tables

case class DrtConfigParameters(config: Configuration) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  val forecastMaxDays: Int = config.get[Int]("crunch.forecast.max_days")
  val aclPollMinutes: Int = config.get[Int]("crunch.forecast.poll_minutes")
  val snapshotIntervalVm: Int = config.getOptional[Int]("persistence.snapshot-interval.voyage-manifest").getOrElse(1000)
  val snapshotMegaBytesBaseArrivals: Int = (config.getOptional[Double]("persistence.snapshot-megabytes.base-arrivals").getOrElse(1d) * oneMegaByte).toInt
  val snapshotMegaBytesFcstArrivals: Int = (config.getOptional[Double]("persistence.snapshot-megabytes.forecast-arrivals").getOrElse(5d) * oneMegaByte).toInt
  val snapshotMegaBytesLiveArrivals: Int = (config.getOptional[Double]("persistence.snapshot-megabytes.live-arrivals").getOrElse(2d) * oneMegaByte).toInt
  val snapshotMegaBytesFcstPortState: Int = (config.getOptional[Double]("persistence.snapshot-megabytes.forecast-portstate").getOrElse(10d) * oneMegaByte).toInt
  val snapshotMegaBytesLivePortState: Int = (config.getOptional[Double]("persistence.snapshot-megabytes.live-portstate").getOrElse(25d) * oneMegaByte).toInt
  val snapshotMegaBytesVoyageManifests: Int = (config.getOptional[Double]("persistence.snapshot-megabytes.voyage-manifest").getOrElse(100d) * oneMegaByte).toInt
  val awSCredentials: AWSCredentials = new AWSCredentials {
    override def getAWSAccessKeyId: String = config.getOptional[String]("aws.credentials.access_key_id").getOrElse("")

    override def getAWSSecretKey: String = config.getOptional[String]("aws.credentials.secret_key").getOrElse("")
  }
  val ftpServer: String = ConfigFactory.load.getString("acl.host")
  val username: String = ConfigFactory.load.getString("acl.username")
  val path: String = ConfigFactory.load.getString("acl.keypath")
  val aclMinFileSizeInBytes: Long = config.getOptional[Long]("acl.min-file-size-in-bytes").getOrElse(10000L)
  val refreshArrivalsOnStart: Boolean = config.getOptional[Boolean]("crunch.refresh-arrivals-on-start").getOrElse(false)
  val recrunchOnStart: Boolean = config.getOptional[Boolean]("crunch.recrunch-on-start").getOrElse(false)
  val resetRegisteredArrivalOnStart: Boolean = if (refreshArrivalsOnStart) {
    log.warn("Refresh arrivals flag is active. Turning on historic manifest refresh")
    true
  } else config.getOptional[Boolean]("crunch.manifests.reset-registered-arrivals-on-start").getOrElse(false)

  val useNationalityBasedProcessingTimes: Boolean = config.getOptional[String]("feature-flags.nationality-based-processing-times").isDefined

  val manifestLookupBatchSize: Int = config.getOptional[Int]("crunch.manifests.lookup-batch-size").getOrElse(10)

  val useLegacyManifests: Boolean = config.getOptional[Boolean]("feature-flags.use-legacy-manifests").getOrElse(false)

  val rawSplitsUrl: String = config.getOptional[String]("crunch.splits.raw-data-path").getOrElse("/dev/null")
  val dqZipBucketName: String = config.getOptional[String]("dq.s3.bucket").getOrElse(throw new Exception("You must set DQ_S3_BUCKET for us to poll for AdvPaxInfo"))
  val apiS3PollFrequencyMillis: MillisSinceEpoch = config.getOptional[Int]("dq.s3.poll_frequency_seconds").getOrElse(60) * 1000L
  val isSuperUserMode: Boolean = config.getOptional[String]("feature-flags.super-user-mode").isDefined
  val maybeBlackJackUrl: Option[String] = config.getOptional[String]("feeds.lhr.blackjack_url")

  val bhxSoapEndPointUrl: String = config.get[String]("feeds.bhx.soap.endPointUrl")
  val bhxIataEndPointUrl: String = config.get[String]("feeds.bhx.iata.endPointUrl")
  val bhxIataUsername: String = config.get[String]("feeds.bhx.iata.username")

  val newLhrFeedApiUrl: String = config.getOptional[String]("feeds.lhr.live.api_url").getOrElse("")
  val newLhrFeedApiToken: String = config.getOptional[String]("feeds.lhr.live.token").getOrElse("")

  val maybeBhxSoapEndPointUrl: Option[String] = config.getOptional[String]("feeds.bhx.soap.endPointUrl")

  val maybeB5JStartDate: Option[String] = config.getOptional[String]("feature-flags.b5jplus-start-date")

  val maybeLtnLiveFeedUrl: Option[String] = config.getOptional[String]("feeds.ltn.live.url")
  val maybeLtnLiveFeedUsername: Option[String] = config.getOptional[String]("feeds.ltn.live.username")
  val maybeLtnLiveFeedPassword: Option[String] = config.getOptional[String]("feeds.ltn.live.password")
  val maybeLtnLiveFeedToken: Option[String] = config.getOptional[String]("feeds.ltn.live.token")
  val maybeLtnLiveFeedTimeZone: Option[String] = config.getOptional[String]("feeds.ltn.live.timezone")

  val maybeLGWNamespace: Option[String] = config.getOptional[String]("feeds.lgw.live.azure.namespace")
  val maybeLGWSASToKey: Option[String] = config.getOptional[String]("feeds.lgw.live.azure.sas_to_Key")
  val maybeLGWServiceBusUri: Option[String] = config.getOptional[String]("feeds.lgw.live.azure.service_bus_uri")

  val maybeGlaLiveUrl: Option[String] = config.getOptional[String]("feeds.gla.url")
  val maybeGlaLiveToken: Option[String] = config.getOptional[String]("feeds.gla.token")
  val maybeGlaLivePassword: Option[String] = config.getOptional[String]("feeds.gla.password")
  val maybeGlaLiveUsername: Option[String] = config.getOptional[String]("feeds.gla.username")

  val snapshotStaffOnStart: Boolean = config.get[Boolean]("feature-flags.snapshot-staffing-on-start")

  val useApiPaxNos: Boolean = config.getOptional[Boolean]("feature-flags.use-api-pax-nos").getOrElse(false)

  val enableToggleDisplayWaitTimes: Boolean = config
    .getOptional[Boolean]("feature-flags.enable-toggle-display-wait-times").getOrElse(false)
  val adjustEGateUseByUnder12s: Boolean = config.getOptional[Boolean]("feature-flags.adjust-egates-use-by-u12s").getOrElse(false)

}

case class SubscribeRequestQueue(subscriber: ActorRef)

case class SubscribeResponseQueue(subscriber: SourceQueueWithComplete[ManifestsFeedResponse])

case class DrtSystem(actorSystem: ActorSystem, config: Configuration, airportConfig: AirportConfig)
                    (implicit materializer: Materializer, ec: ExecutionContext) extends DrtSystemInterface {

  implicit val system: ActorSystem = actorSystem

  val params = DrtConfigParameters(config)

  import DrtStaticParameters._

  val aclFeed = AclFeed(params.ftpServer, params.username, params.path, airportConfig.feedPortCode, AclFeed.aclToPortMapping(airportConfig.portCode), params.aclMinFileSizeInBytes)

  system.log.info(s"Path to splits file ${ConfigFactory.load.getString("passenger_splits_csv_url")}")

  val gateWalkTimesProvider: GateOrStandWalkTime = walkTimeMillisProviderFromCsv(ConfigFactory.load.getString("walk_times.gates_csv_url"))
  val standWalkTimesProvider: GateOrStandWalkTime = walkTimeMillisProviderFromCsv(ConfigFactory.load.getString("walk_times.stands_csv_url"))

  val maxBufferSize: Int = config.get[Int]("crunch.manifests.max-buffer-size")
  val minSecondsBetweenBatches: Int = config.get[Int]("crunch.manifests.min-seconds-between-batches")

  val aggregateArrivalsDbConfigKey = "aggregated-db"

  val purgeOldLiveSnapshots = false
  val purgeOldForecastSnapshots = true

  val forecastMaxMillis: () => MillisSinceEpoch = () => now().addDays(params.forecastMaxDays).millisSinceEpoch
  val liveCrunchStateProps: Props = CrunchStateActor.props(Option(airportConfig.portStateSnapshotInterval), params.snapshotMegaBytesLivePortState, "crunch-state", airportConfig.queuesByTerminal, now, expireAfterMillis, purgeOldLiveSnapshots, forecastMaxMillis)
  val forecastCrunchStateProps: Props = CrunchStateActor.props(Option(100), params.snapshotMegaBytesFcstPortState, "forecast-crunch-state", airportConfig.queuesByTerminal, now, expireAfterMillis, purgeOldForecastSnapshots, forecastMaxMillis)

  lazy val baseArrivalsActor: ActorRef = system.actorOf(Props(classOf[ForecastBaseArrivalsActor], params.snapshotMegaBytesBaseArrivals, now, expireAfterMillis), name = "base-arrivals-actor")
  lazy val forecastArrivalsActor: ActorRef = system.actorOf(Props(classOf[ForecastPortArrivalsActor], params.snapshotMegaBytesFcstArrivals, now, expireAfterMillis), name = "forecast-arrivals-actor")
  lazy val liveBaseArrivalsActor: ActorRef = system.actorOf(Props(classOf[LiveBaseArrivalsActor], params.snapshotMegaBytesLiveArrivals, now, expireAfterMillis), name = "live-base-arrivals-actor")
  lazy val liveArrivalsActor: ActorRef = system.actorOf(Props(classOf[LiveArrivalsActor], params.snapshotMegaBytesLiveArrivals, now, expireAfterMillis), name = "live-arrivals-actor")

  lazy val aclPaxAdjustmentDays: Int = config.get[Int]("acl.adjustment.number-of-days-in-average")
  lazy val passengersActorProvider: () => AskableActorRef = () => system.actorOf(Props(new PassengersActor(aclPaxAdjustmentDays)))

  lazy val arrivalsImportActor: ActorRef = system.actorOf(Props(classOf[ArrivalsImportActor]), name = "arrivals-import-actor")

  lazy val aggregatedArrivalsActor: ActorRef = system.actorOf(Props(classOf[AggregatedArrivalsActor], ArrivalTable(airportConfig.portCode, PostgresTables)), name = "aggregated-arrivals-actor")
  lazy val registeredArrivalsActor: ActorRef = system.actorOf(Props(classOf[RegisteredArrivalsActor], oneMegaByte, Option(500), airportConfig.portCode, now, expireAfterMillis), name = "registered-arrivals-actor")

  lazy val liveCrunchStateActor: AskableActorRef = system.actorOf(liveCrunchStateProps, name = "crunch-live-state-actor")
  lazy val forecastCrunchStateActor: AskableActorRef = system.actorOf(forecastCrunchStateProps, name = "crunch-forecast-state-actor")
  lazy val portStateActor: ActorRef = system.actorOf(PortStateActor.props(liveCrunchStateActor, forecastCrunchStateActor, now, liveDaysAhead), name = "port-state-actor")

  lazy val voyageManifestsActor: ActorRef = system.actorOf(Props(classOf[VoyageManifestsActor], params.snapshotMegaBytesVoyageManifests, now, expireAfterMillis, Option(params.snapshotIntervalVm)), name = "voyage-manifests-actor")
  lazy val lookup = ManifestLookup(VoyageManifestPassengerInfoTable(PostgresTables))

  lazy val manifestsArrivalRequestSource: Source[List[Arrival], SourceQueueWithComplete[List[Arrival]]] = Source.queue[List[Arrival]](100, OverflowStrategy.backpressure)

  lazy val shiftsActor: ActorRef = system.actorOf(Props(classOf[ShiftsActor], now, timeBeforeThisMonth(now)))
  lazy val fixedPointsActor: ActorRef = system.actorOf(Props(classOf[FixedPointsActor], now))
  lazy val staffMovementsActor: ActorRef = system.actorOf(Props(classOf[StaffMovementsActor], now, time48HoursAgo(now)))

  lazy val alertsActor: ActorRef = system.actorOf(Props(classOf[AlertsActor], now))
  val historicalSplitsProvider: SplitProvider = SplitsProvider.csvProvider

  val s3ApiProvider = S3ApiProvider(params.awSCredentials, params.dqZipBucketName)
  val initialManifestsState: Option[VoyageManifestState] = initialState[VoyageManifestState](voyageManifestsActor)
  val latestZipFileName: String = S3ApiProvider.latestUnexpiredDqZipFilename(initialManifestsState.map(_.latestZipFilename), now, expireAfterMillis)

  lazy val voyageManifestsLiveSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]] = Source.queue[ManifestsFeedResponse](1, OverflowStrategy.backpressure)
  lazy val voyageManifestsHistoricSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]] = Source.queue[ManifestsFeedResponse](1, OverflowStrategy.backpressure)
  lazy val useLegacyDeployments: Boolean = config.get[Boolean]("crunch.use-legacy-deployments")

  system.log.info(s"useNationalityBasedProcessingTimes: ${params.useNationalityBasedProcessingTimes}")

  def getRoles(config: Configuration, headers: Headers, session: Session): Set[Role] =
    if (params.isSuperUserMode) {
      system.log.debug(s"Using Super User Roles")
      Roles.availableRoles
    } else userRolesFromHeader(headers)

  lazy val optimiser: TryCrunch = if (config.get[Boolean]("crunch.use-legacy-optimiser")) TryRenjin.crunch else Optimiser.crunch

  lazy val portDeskRecs: DesksAndWaitsPortProviderLike = DesksAndWaitsPortProvider(airportConfig, optimiser)

  lazy val maxDesksProviders: Map[Terminal, TerminalDeskLimitsLike] = if (config.get[Boolean]("crunch.flex-desks"))
    PortDeskLimits.flexed(airportConfig)
  else
    PortDeskLimits.fixed(airportConfig)

  def run(): Unit = {
    val futurePortStates: Future[(Option[PortState], Option[PortState], Option[mutable.SortedMap[UniqueArrival, Arrival]], Option[mutable.SortedMap[UniqueArrival, Arrival]], Option[mutable.SortedMap[UniqueArrival, Arrival]], Option[RegisteredArrivals])] = {
      val maybeLivePortState = initialStateFuture[PortState](liveCrunchStateActor)
      val maybeForecastPortState = initialStateFuture[PortState](forecastCrunchStateActor)
      val maybeInitialBaseArrivals = initialStateFuture[ArrivalsState](baseArrivalsActor).map(_.map(_.arrivals))
      val maybeInitialFcstArrivals = initialStateFuture[ArrivalsState](forecastArrivalsActor).map(_.map(_.arrivals))
      val maybeInitialLiveArrivals = initialStateFuture[ArrivalsState](liveArrivalsActor).map(_.map(_.arrivals))
      val maybeInitialRegisteredArrivals = initialStateFuture[RegisteredArrivals](registeredArrivalsActor)
      for {
        lps <- maybeLivePortState
        fps <- maybeForecastPortState
        ba <- maybeInitialBaseArrivals
        fa <- maybeInitialFcstArrivals
        la <- maybeInitialLiveArrivals
        ra <- maybeInitialRegisteredArrivals
      } yield (lps, fps, ba, fa, la, ra)
    }

    futurePortStates.onComplete {
      case Success((maybeLiveState, maybeForecastState, maybeBaseArrivals, maybeForecastArrivals, maybeLiveArrivals, maybeRegisteredArrivals)) =>
        system.log.info(s"Successfully restored initial state for App")
        val initialPortState: Option[PortState] = mergePortStates(maybeForecastState, maybeLiveState)
        initialPortState.foreach(ps => portStateActor ! ps)

        val (crunchSourceActor: ActorRef, _) = RunnableDeskRecs.start(portStateActor, portDeskRecs, now, params.recrunchOnStart, params.forecastMaxDays, maxDesksProviders)
        portStateActor ! SetCrunchActor(crunchSourceActor)

        val (manifestRequestsSource, _, manifestRequestsSink) = SinkToSourceBridge[List[Arrival]]

        val (manifestResponsesSource, _, manifestResponsesSink) = SinkToSourceBridge[List[BestAvailableManifest]]

        val crunchInputs: CrunchSystem[Cancellable] = startCrunchSystem(
          initialPortState,
          maybeBaseArrivals,
          maybeForecastArrivals,
          Option(mutable.SortedMap[UniqueArrival, Arrival]()),
          maybeLiveArrivals,
          manifestRequestsSink,
          manifestResponsesSource,
          params.refreshArrivalsOnStart,
          checkRequiredStaffUpdatesOnStartup = true,
          useLegacyDeployments)

        portStateActor ! SetSimulationActor(crunchInputs.loadsToSimulate)

        if (maybeRegisteredArrivals.isDefined) log.info(s"sending ${maybeRegisteredArrivals.get.arrivals.size} initial registered arrivals to batch stage")
        else log.info(s"sending no registered arrivals to batch stage")

        new S3ManifestPoller(crunchInputs.manifestsLiveResponse, airportConfig.portCode, latestZipFileName, s3ApiProvider).startPollingForManifests()

        if (!params.useLegacyManifests) {
          val initialRegisteredArrivals = if (params.resetRegisteredArrivalOnStart) {
            log.info(s"Resetting registered arrivals for manifest lookups")
            val maybeAllArrivals: Option[mutable.SortedMap[ArrivalKey, Option[Long]]] = initialPortState
              .map { state =>
                val arrivalsByKeySorted = mutable.SortedMap[ArrivalKey, Option[MillisSinceEpoch]]()
                state.flights.values.foreach(fws => arrivalsByKeySorted += (ArrivalKey(fws.apiFlight) -> None))
                log.info(s"Sending ${arrivalsByKeySorted.size} arrivals by key from ${state.flights.size} port state arrivals")
                arrivalsByKeySorted
              }
            Option(RegisteredArrivals(maybeAllArrivals.getOrElse(mutable.SortedMap())))
          } else maybeRegisteredArrivals
          val lookupRefreshDue: MillisSinceEpoch => Boolean = (lastLookupMillis: MillisSinceEpoch) => now().millisSinceEpoch - lastLookupMillis > 15 * oneMinuteMillis
          startManifestsGraph(initialRegisteredArrivals, manifestResponsesSink, manifestRequestsSource, lookupRefreshDue)
        }

        subscribeStaffingActors(crunchInputs)
        startScheduledFeedImports(crunchInputs)

      case Failure(error) =>
        system.log.error(error, s"Failed to restore initial state for App")
        System.exit(1)
    }

    val staffingStates: Future[NotUsed] = {
      val maybeShifts = initialStateFuture[ShiftAssignments](shiftsActor)
      val maybeFixedPoints = initialStateFuture[FixedPointAssignments](fixedPointsActor)
      val maybeMovements = initialStateFuture[StaffMovements](staffMovementsActor)
      for {
        _ <- maybeShifts
        _ <- maybeFixedPoints
        _ <- maybeMovements
      } yield NotUsed
    }

    staffingStates.onComplete {
      case Success(NotUsed) =>
        system.log.info(s"Successfully restored initial staffing states for App")
        if (params.snapshotStaffOnStart) {
          log.info(s"Snapshotting staff as requested by feature flag")
          shiftsActor ! SaveSnapshot
          fixedPointsActor ! SaveSnapshot
          staffMovementsActor ! SaveSnapshot
        }

      case Failure(error) =>
        system.log.error(error, s"Failed to restore initial staffing state for App")
        System.exit(1)
    }
  }

  override val feedActors: Seq[AskableActorRef] = Seq(liveArrivalsActor, liveBaseArrivalsActor, forecastArrivalsActor, baseArrivalsActor, voyageManifestsActor)

  override def getFeedStatus: Future[Seq[FeedSourceStatuses]] = {
    val futureMaybeStatuses = feedActors.map(a => queryActorWithRetry[FeedSourceStatuses](a, GetFeedStatuses))

    Future
      .sequence(futureMaybeStatuses)
      .map(
        maybeStatuses => maybeStatuses
          .collect { case Some(fs) => fs }
          .filter(fss => isValidFeedSource(fss.feedSource))
      )
  }

  override def isValidFeedSource(fs: FeedSource): Boolean = airportConfig.feedSources.contains(fs)

  def startScheduledFeedImports(crunchInputs: CrunchSystem[Cancellable]): Unit = {
    if (airportConfig.feedPortCode == PortCode("LHR")) params.maybeBlackJackUrl.map(csvUrl => {
      val requestIntervalMillis = 5 * oneMinuteMillis
      Deskstats.startBlackjack(csvUrl, crunchInputs.actualDeskStats, requestIntervalMillis milliseconds, () => SDate.now().addDays(-1))
    })
  }

  def subscribeStaffingActors(crunchInputs: CrunchSystem[Cancellable]): Unit = {
    shiftsActor ! AddShiftSubscribers(List(crunchInputs.shifts))
    fixedPointsActor ! AddFixedPointSubscribers(List(crunchInputs.fixedPoints))
    staffMovementsActor ! AddStaffMovementsSubscribers(List(crunchInputs.staffMovements))
  }

  def startManifestsGraph(maybeRegisteredArrivals: Option[RegisteredArrivals],
                          manifestResponsesSink: Sink[List[BestAvailableManifest], NotUsed],
                          manifestRequestsSource: Source[List[Arrival], NotUsed],
                          lookupRefreshDue: MillisSinceEpoch => Boolean): UniqueKillSwitch = {
    val batchSize = config.get[Int]("crunch.manifests.lookup-batch-size")
    lazy val batchStage: BatchStage = new BatchStage(now, Crunch.isDueLookup, batchSize, expireAfterMillis, maybeRegisteredArrivals, 1000, lookupRefreshDue)

    ManifestsGraph(manifestRequestsSource, batchStage, manifestResponsesSink, registeredArrivalsActor, airportConfig.portCode, lookup).run
  }

  def startCrunchSystem(initialPortState: Option[PortState],
                        initialForecastBaseArrivals: Option[mutable.SortedMap[UniqueArrival, Arrival]],
                        initialForecastArrivals: Option[mutable.SortedMap[UniqueArrival, Arrival]],
                        initialLiveBaseArrivals: Option[mutable.SortedMap[UniqueArrival, Arrival]],
                        initialLiveArrivals: Option[mutable.SortedMap[UniqueArrival, Arrival]],
                        manifestRequestsSink: Sink[List[Arrival], NotUsed],
                        manifestResponsesSource: Source[List[BestAvailableManifest], NotUsed],
                        refreshArrivalsOnStart: Boolean,
                        checkRequiredStaffUpdatesOnStartup: Boolean,
                        useLegacyDeployments: Boolean): CrunchSystem[Cancellable] = {

    val crunchInputs = CrunchSystem(CrunchProps(
      airportConfig = airportConfig,
      pcpArrival = pcpArrivalTimeCalculator,
      historicalSplitsProvider = historicalSplitsProvider,
      portStateActor = portStateActor,
      maxDaysToCrunch = params.forecastMaxDays,
      expireAfterMillis = expireAfterMillis,
      actors = Map(
        "shifts" -> shiftsActor,
        "fixed-points" -> fixedPointsActor,
        "staff-movements" -> staffMovementsActor,
        "forecast-base-arrivals" -> baseArrivalsActor,
        "forecast-arrivals" -> forecastArrivalsActor,
        "live-base-arrivals" -> liveBaseArrivalsActor,
        "live-arrivals" -> liveArrivalsActor,
        "aggregated-arrivals" -> aggregatedArrivalsActor
      ),
      useNationalityBasedProcessingTimes = params.useNationalityBasedProcessingTimes,
      useLegacyManifests = params.useLegacyManifests,
      manifestsLiveSource = voyageManifestsLiveSource,
      manifestResponsesSource = manifestResponsesSource,
      voyageManifestsActor = voyageManifestsActor,
      manifestRequestsSink = manifestRequestsSink,
      simulator = Optimiser.runSimulationOfWork,
      initialPortState = initialPortState,
      initialForecastBaseArrivals = initialForecastBaseArrivals.getOrElse(mutable.SortedMap()),
      initialForecastArrivals = initialForecastArrivals.getOrElse(mutable.SortedMap()),
      initialLiveBaseArrivals = initialLiveBaseArrivals.getOrElse(mutable.SortedMap()),
      initialLiveArrivals = initialLiveArrivals.getOrElse(mutable.SortedMap()),
      arrivalsForecastBaseSource = baseArrivalsSource(),
      arrivalsForecastSource = forecastArrivalsSource(airportConfig.feedPortCode),
      arrivalsLiveBaseSource = liveBaseArrivalsSource(airportConfig.feedPortCode),
      arrivalsLiveSource = liveArrivalsSource(airportConfig.feedPortCode),
      initialShifts = initialState[ShiftAssignments](shiftsActor).getOrElse(ShiftAssignments(Seq())),
      initialFixedPoints = initialState[FixedPointAssignments](fixedPointsActor).getOrElse(FixedPointAssignments(Seq())),
      initialStaffMovements = initialState[StaffMovements](staffMovementsActor).map(_.movements).getOrElse(Seq[StaffMovement]()),
      refreshArrivalsOnStart = refreshArrivalsOnStart,
      checkRequiredStaffUpdatesOnStartup = checkRequiredStaffUpdatesOnStartup,
      stageThrottlePer = config.get[Int]("crunch.stage-throttle-millis") millisecond,
      useApiPaxNos = params.useApiPaxNos,
      adjustEGateUseByUnder12s = params.adjustEGateUseByUnder12s,
      optimiser = optimiser,
      useLegacyDeployments = useLegacyDeployments,
      passengersActorProvider = passengersActorProvider,
      aclPaxAdjustmentDays = aclPaxAdjustmentDays))
    crunchInputs
  }

  def initialState[A](askableActor: AskableActorRef): Option[A] = Await.result(initialStateFuture[A](askableActor), 2 minutes)

  def initialStateFuture[A](askableActor: AskableActorRef): Future[Option[A]] = {
    val actorPath = askableActor.actorRef.path
    queryActorWithRetry[A](askableActor, GetState)
      .map {
        case Some(state: A) if state.isInstanceOf[A] =>
          log.debug(s"Got initial state (Some(${state.getClass})) from $actorPath")
          Option(state)
        case None =>
          log.warn(s"Got no state (None) from $actorPath")
          None
      }
      .recoverWith {
        case t =>
          log.error(s"Failed to get response from $askableActor", t)
          Future(None)
      }
  }

  def queryActorWithRetry[A](askableActor: AskableActorRef, toAsk: Any): Future[Option[A]] = {
    val future = askableActor.ask(toAsk)(new Timeout(2 minutes)).map {
      case Some(state: A) if state.isInstanceOf[A] => Option(state)
      case state: A if !state.isInstanceOf[Option[A]] => Option(state)
      case _ => None
    }

    implicit val scheduler: Scheduler = actorSystem.scheduler
    Retry.retry(future, RetryDelays.fibonacci, 3, 5 seconds)
  }

  def mergePortStates(maybeForecastPs: Option[PortState],
                      maybeLivePs: Option[PortState]): Option[PortState] = (maybeForecastPs, maybeLivePs) match {
    case (None, None) => None
    case (Some(fps), None) =>
      log.info(s"We only have initial forecast port state")
      Option(fps)
    case (None, Some(lps)) =>
      log.info(s"We only have initial live port state")
      Option(lps)
    case (Some(fps), Some(lps)) =>
      log.info(s"Merging initial live & forecast port states. ${lps.flights.size} live flights, ${fps.flights.size} forecast flights")
      Option(PortState(
        fps.flights ++ lps.flights,
        fps.crunchMinutes ++ lps.crunchMinutes,
        fps.staffMinutes ++ lps.staffMinutes))
  }

  def createSparkSession(): SparkSession = {
    SparkSession
      .builder
      .appName("DRT Predictor")
      .config("spark.master", "local")
      .getOrCreate()
  }

  def liveBaseArrivalsSource(portCode: PortCode): Source[ArrivalsFeedResponse, Cancellable] = {
    if (config.get[Boolean]("feature-flags.use-cirium-feed")) {
      log.info(s"Using Cirium Live Base Feed")
      CiriumFeed(config.get[String]("feeds.cirium.host"), portCode).tickingSource(30 seconds)
    }
    else {
      log.info(s"Using Noop Base Live Feed")
      arrivalsNoOp
    }
  }

  def liveArrivalsSource(portCode: PortCode): Source[ArrivalsFeedResponse, Cancellable] =
    if (config.get[Boolean]("feeds.random-generator")) randomArrivals()
    else portCode.iata match {
      case "LHR" =>
        val host = config.get[String]("feeds.lhr.sftp.live.host")
        val username = config.get[String]("feeds.lhr.sftp.live.username")
        val password = config.get[String]("feeds.lhr.sftp.live.password")
        val contentProvider = () => LhrSftpLiveContentProvider(host, username, password).latestContent
        LHRFlightFeed(contentProvider)
      case "EDI" =>
        createLiveChromaFlightFeed(ChromaLive).chromaEdiFlights()
      case "LGW" =>
        val lgwNamespace = params.maybeLGWNamespace.getOrElse(throw new Exception("Missing LGW Azure Namespace parameter"))
        val lgwSasToKey = params.maybeLGWSASToKey.getOrElse(throw new Exception("Missing LGW SAS Key for To Queue"))
        val lgwServiceBusUri = params.maybeLGWServiceBusUri.getOrElse(throw new Exception("Missing LGW Service Bus Uri"))
        val azureClient = LGWAzureClient(LGWFeed.serviceBusClient(lgwNamespace, lgwSasToKey, lgwServiceBusUri))
        LGWFeed(azureClient)(system).source()
      case "BHX" if !params.bhxIataEndPointUrl.isEmpty =>
        BHXFeed(BHXClient(params.bhxIataUsername, params.bhxIataEndPointUrl), 80 seconds, 1 milliseconds)(system)
      case "BHX" =>
        BHXLiveFeedLegacy(params.maybeBhxSoapEndPointUrl.getOrElse(throw new Exception("Missing BHX live feed URL")))
      case "LTN" =>
        val url = params.maybeLtnLiveFeedUrl.getOrElse(throw new Exception("Missing live feed url"))
        val username = params.maybeLtnLiveFeedUsername.getOrElse(throw new Exception("Missing live feed username"))
        val password = params.maybeLtnLiveFeedPassword.getOrElse(throw new Exception("Missing live feed password"))
        val token = params.maybeLtnLiveFeedToken.getOrElse(throw new Exception("Missing live feed token"))
        val timeZone = params.maybeLtnLiveFeedTimeZone match {
          case Some(tz) => DateTimeZone.forID(tz)
          case None => DateTimeZone.UTC
        }
        val requester = LtnFeedRequester(url, token, username, password)
        LtnLiveFeed(requester, timeZone).tickingSource
      case "MAN" | "STN" | "EMA" =>
        if (config.get[Boolean]("feeds.mag.use-legacy")) {
          log.info(s"Using legacy MAG live feed")
          createLiveChromaFlightFeed(ChromaLive).chromaVanillaFlights(30 seconds)
        } else {
          log.info(s"Using new MAG live feed")
          val privateKey: String = config.get[String]("feeds.mag.private-key")
          val claimIss: String = config.get[String]("feeds.mag.claim.iss")
          val claimRole: String = config.get[String]("feeds.mag.claim.role")
          val claimSub: String = config.get[String]("feeds.mag.claim.sub")
          MagFeed(privateKey, claimIss, claimRole, claimSub, now, airportConfig.portCode, ProdFeedRequester).tickingSource
        }
      case "GLA" =>
        val liveUrl = params.maybeGlaLiveUrl.getOrElse(throw new Exception("Missing GLA Live Feed Url"))
        val livePassword = params.maybeGlaLivePassword.getOrElse(throw new Exception("Missing GLA Live Feed Password"))
        val liveToken = params.maybeGlaLiveToken.getOrElse(throw new Exception("Missing GLA Live Feed Token"))
        val liveUsername = params.maybeGlaLiveUsername.getOrElse(throw new Exception("Missing GLA Live Feed Username"))
        GlaFeed(liveUrl, liveToken, livePassword, liveUsername, ProdGlaFeedRequester).tickingSource
      case _ => arrivalsNoOp
    }

  private def randomArrivals(): Source[ArrivalsFeedResponse, Cancellable] = {
    val arrivals = ArrivalGenerator.arrivals(now, airportConfig.terminals)
    Source.tick(1 millisecond, 1 minute, NotUsed).map { _ =>
      ArrivalsFeedSuccess(Flights(arrivals.toSeq))
    }
  }

  def arrivalsNoOp: Source[ArrivalsFeedResponse, Cancellable] = Source.tick[ArrivalsFeedResponse](100 days, 100 days, ArrivalsFeedSuccess(Flights(Seq()), SDate.now()))

  def forecastArrivalsSource(portCode: PortCode): Source[ArrivalsFeedResponse, Cancellable] = {
    val feed = portCode match {
      case PortCode("LHR") => createForecastLHRFeed()
      case PortCode("BHX") => BHXForecastFeedLegacy(params.maybeBhxSoapEndPointUrl.getOrElse(throw new Exception("Missing BHX feed URL")))
      case PortCode("LGW") => LGWForecastFeed()
      case _ =>
        system.log.info(s"No Forecast Feed defined.")
        arrivalsNoOp
    }
    feed
  }

  def baseArrivalsSource(): Source[ArrivalsFeedResponse, Cancellable] = if (isTestEnvironment)
    arrivalsNoOp
  else
    Source.tick(1 second, 10 minutes, NotUsed).map(_ => {
      system.log.info(s"Requesting ACL feed")
      aclFeed.requestArrivals
    })

  private def isTestEnvironment: Boolean = config.getOptional[String]("env").getOrElse("live") == "test"

  def walkTimeProvider(flight: Arrival): MillisSinceEpoch =
    gateOrStandWalkTimeCalculator(gateWalkTimesProvider, standWalkTimesProvider, airportConfig.defaultWalkTimeMillis.getOrElse(flight.Terminal, 300000L))(flight)

  def pcpArrivalTimeCalculator: Arrival => MilliDate =
    PaxFlow.pcpArrivalTimeForFlight(airportConfig.timeToChoxMillis, airportConfig.firstPaxOffMillis)(walkTimeProvider)

  def createLiveChromaFlightFeed(feedType: ChromaFeedType): ChromaLiveFeed = {
    ChromaLiveFeed(new ChromaFetcher[ChromaLiveFlight](feedType, ChromaFlightMarshallers.live) with ProdSendAndReceive)
  }

  def createForecastChromaFlightFeed(feedType: ChromaFeedType): ChromaForecastFeed = {
    ChromaForecastFeed(new ChromaFetcher[ChromaForecastFlight](feedType, ChromaFlightMarshallers.forecast) with ProdSendAndReceive)
  }

  def createForecastLHRFeed(): Source[ArrivalsFeedResponse, Cancellable] = {
    val lhrForecastFeed = LHRForecastFeed(arrivalsImportActor)
    Source
      .tick(10 seconds, 60 seconds, NotUsed)
      .map(_ => lhrForecastFeed.requestFeed)
  }
}

case class SetSimulationActor(loadsToSimulate: AskableActorRef)

case class SetCrunchActor(millisToCrunchActor: AskableActorRef)

object ArrivalGenerator {
  def arrival(iata: String = "",
              icao: String = "",
              schDt: String = "",
              actPax: Option[Int] = None,
              maxPax: Option[Int] = None,
              terminal: Terminal = T1,
              origin: PortCode = PortCode(""),
              operator: Option[Operator] = None,
              status: String = "",
              estDt: String = "",
              actDt: String = "",
              estChoxDt: String = "",
              actChoxDt: String = "",
              pcpDt: String = "",
              gate: Option[String] = None,
              stand: Option[String] = None,
              tranPax: Option[Int] = None,
              runwayId: Option[String] = None,
              baggageReclaimId: Option[String] = None,
              airportId: PortCode = PortCode(""),
              feedSources: Set[FeedSource] = Set()
             ): Arrival = {
    val pcpTime = if (pcpDt.nonEmpty) Option(SDate(pcpDt).millisSinceEpoch) else if (schDt.nonEmpty) Option(SDate(schDt).millisSinceEpoch) else None

    Arrival(
      rawICAO = icao,
      rawIATA = iata,
      ActPax = actPax,
      Terminal = terminal,
      Origin = origin,
      Operator = operator,
      Status = ArrivalStatus(status),
      Estimated = if (estDt.nonEmpty) Option(SDate.parseString(estDt).millisSinceEpoch) else None,
      Actual = if (actDt.nonEmpty) Option(SDate.parseString(actDt).millisSinceEpoch) else None,
      EstimatedChox = if (estChoxDt.nonEmpty) Option(SDate.parseString(estChoxDt).millisSinceEpoch) else None,
      ActualChox = if (actChoxDt.nonEmpty) Option(SDate.parseString(actChoxDt).millisSinceEpoch) else None,
      Gate = gate,
      Stand = stand,
      MaxPax = maxPax,
      TranPax = tranPax,
      RunwayID = runwayId,
      BaggageReclaimId = baggageReclaimId,
      AirportID = airportId,
      PcpTime = pcpTime,
      Scheduled = if (schDt.nonEmpty) SDate(schDt).millisSinceEpoch else 0,
      FeedSources = feedSources
      )
  }

  def arrivals(now: () => SDateLike, terminalNames: Iterable[Terminal]): Iterable[Arrival] = {
    val today = now().toISODateOnly
    val arrivals = for {
      terminal <- terminalNames
    } yield {
      (1 to 100).map { _ =>
        def rand(max: Int): Int = (Math.random() * max).floor.toInt

        ArrivalGenerator.arrival(iata = s"BA${rand(1000)}", terminal = terminal, schDt = s"${today}T${rand(24)}:${rand(60)}", actPax = Option(rand(450)))
      }
    }

    arrivals.flatten
  }
}
