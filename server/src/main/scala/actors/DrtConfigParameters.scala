package actors

import actors.persistent.Sizes.oneMegaByte
import com.amazonaws.auth.AWSCredentials
import com.typesafe.config.ConfigFactory
import drt.shared.CrunchApi.MillisSinceEpoch
import org.slf4j.{Logger, LoggerFactory}
import play.api.Configuration
import scala.concurrent.duration._
case class DrtConfigParameters(config: Configuration) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  val gateWalkTimesFilePath: String = config.get[String]("walk_times.gates_csv_url")
  val standWalkTimesFilePath: String = config.get[String]("walk_times.stands_csv_url")

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
  val refreshManifestsOnStart: Boolean = if (refreshArrivalsOnStart) {
    log.warn("Refresh arrivals flag is active. Turning on historic manifest refresh")
    true
  } else config.getOptional[Boolean]("crunch.manifests.reset-registered-arrivals-on-start").getOrElse(false)

  val useNationalityBasedProcessingTimes: Boolean = config.getOptional[String]("feature-flags.nationality-based-processing-times").isDefined

  val manifestLookupBatchSize: Int = config.getOptional[Int]("crunch.manifests.lookup-batch-size").getOrElse(10)

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

  val useApiPaxNos: Boolean = config.get[Boolean]("feature-flags.use-api-pax-nos")
  val displayRedListInfo: Boolean = config.get[Boolean]("feature-flags.display-red-list-info")

  val enableToggleDisplayWaitTimes: Boolean = config.get[Boolean]("feature-flags.enable-toggle-display-wait-times")
  val adjustEGateUseByUnder12s: Boolean = config.get[Boolean]("feature-flags.adjust-egates-use-by-u12s")

  val maybeLcySoapEndPointUrl: Option[String] = config.getOptional[String]("feeds.lcy.soap.endPointUrl")
  val lcyLiveEndPointUrl: String = config.get[String]("feeds.lcy.live.endPointUrl")
  val lcyLiveUsername: String = config.get[String]("feeds.lcy.live.username")
  val lcyLivePassword: String = config.get[String]("feeds.lcy.live.password")

  val maybeEdiTerminalMapCsvUrl: Option[String] = config.getOptional[String]("feeds.edi.terminal-map-csv-url")

  //ignore ACL flight removals X seconds after the end of the day.
  val maybeRemovalCutOffSeconds: Option[FiniteDuration] = config.getOptional[Int]("acl.removal-cutoff-seconds").map(s => s seconds)

}
