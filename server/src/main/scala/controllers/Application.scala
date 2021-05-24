package controllers

import actors.PartitionedPortStateActor.GetStateForTerminalDateRange
import actors._
import actors.persistent.staffing.{GetState, UpdateShifts}
import akka.actor._
import akka.event.{Logging, LoggingAdapter}
import akka.pattern._
import akka.stream._
import akka.util.Timeout
import api._
import boopickle.Default._
import buildinfo.BuildInfo
import com.typesafe.config.ConfigFactory
import controllers.application._
import drt.http.ProdSendAndReceive
import drt.shared.CrunchApi._
import drt.shared.KeyCloakApi.{KeyCloakGroup, KeyCloakUser}
import drt.shared.Terminals.Terminal
import drt.shared.api.Arrival
import drt.shared.{AirportConfig, _}
import drt.users.KeyCloakClient
import org.joda.time.chrono.ISOChronology
import org.slf4j.{Logger, LoggerFactory}
import play.api.mvc.{Action, _}
import play.api.{Configuration, Environment}
import services.PcpArrival.{pcpFrom, _}
import services._
import services.graphstages.Crunch
import services.metrics.Metrics
import services.staffing.StaffTimeSlots
import test.TestDrtSystem
import uk.gov.homeoffice.drt.auth.Roles.{BorderForceStaff, ManageUsers, Role, StaffEdit}
import uk.gov.homeoffice.drt.auth._

import java.nio.ByteBuffer
import java.util.{Calendar, TimeZone, UUID}
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.postfixOps

object Router extends autowire.Server[ByteBuffer, Pickler, Pickler] {

  import scala.language.experimental.macros

  override def read[R: Pickler](p: ByteBuffer): R = Unpickle[R].fromBytes(p)

  def myroute[Trait](target: Trait): Router = macro MyMacros.routeMacro[Trait, ByteBuffer]

  override def write[R: Pickler](r: R): ByteBuffer = Pickle.intoBytes(r)
}

object PaxFlow {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def pcpArrivalTimeForFlight(timeToChoxMillis: MillisSinceEpoch, firstPaxOffMillis: MillisSinceEpoch)
                             (walkTimeProvider: FlightWalkTime)
                             (flight: Arrival): MilliDate = pcpFrom(timeToChoxMillis, firstPaxOffMillis, walkTimeProvider)(flight)
}

trait AirportConfiguration {
  def airportConfig: AirportConfig
}

trait AirportConfProvider extends AirportConfiguration {
  val portCode: PortCode = PortCode(ConfigFactory.load().getString("portcode").toUpperCase)
  val config: Configuration

  def contactEmail: Option[String] = config.getOptional[String]("contact-email")

  def oohPhone: Option[String] = config.getOptional[String]("ooh-phone")

  def getPortConfFromEnvVar: AirportConfig = AirportConfigs.confByPort(portCode)

  lazy val airportConfig: AirportConfig = {
    val configForPort = getPortConfFromEnvVar.copy(
      contactEmail = contactEmail,
      outOfHoursContactPhone = oohPhone
    )

    configForPort.assertValid()

    configForPort
  }
}

trait UserRoleProviderLike {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def userRolesFromHeader(headers: Headers): Set[Role] = headers.get("X-Auth-Roles").map(_.split(",").flatMap(Roles.parse).toSet).getOrElse(Set.empty[Role])

  def getRoles(config: Configuration, headers: Headers, session: Session): Set[Role]

  def getLoggedInUser(config: Configuration, headers: Headers, session: Session): LoggedInUser = {
    val baseRoles = Set()
    val roles: Set[Role] =
      getRoles(config, headers, session) ++ baseRoles
    LoggedInUser(
      userName = headers.get("X-Auth-Username").getOrElse("Unknown"),
      id = headers.get("X-Auth-Userid").getOrElse("Unknown"),
      email = headers.get("X-Auth-Email").getOrElse("Unknown"),
      roles = roles
    )
  }
}

object DrtActorSystem extends AirportConfProvider {
  implicit val actorSystem: ActorSystem = ActorSystem("DRT")
  implicit val mat: Materializer = ActorMaterializer.create(actorSystem)
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global
  val config: Configuration = new Configuration(ConfigFactory.load)
  val isTestEnvironment: Boolean = config.getOptional[String]("env").getOrElse("live") == "test"

  val drtSystem: DrtSystemInterface =
    if (isTestEnvironment) drtTestSystem
    else drtProdSystem

  lazy val drtTestSystem: TestDrtSystem = TestDrtSystem(getPortConfFromEnvVar)
  lazy val drtProdSystem: ProdDrtSystem = ProdDrtSystem(getPortConfFromEnvVar)

}

@Singleton
class Application @Inject()(implicit val config: Configuration, env: Environment)
  extends InjectedController
    with AirportConfProvider
    with WithAirportConfig
    with WithAirportInfo
    with WithAlerts
    with WithAuth
    with WithContactDetails
    with WithFeatureFlags
    with WithExports
    with WithFeeds
    with WithImports
    with WithPortState
    with WithStaffing
    with WithApplicationInfo
    with WithSimulations
    with WithPassengerInfo
    with WithWalkTimes
    with WithDebug
    with WithEmailFeedback {

  implicit val system: ActorSystem = DrtActorSystem.actorSystem
  implicit val mat: Materializer = DrtActorSystem.mat
  implicit val ec: ExecutionContext = DrtActorSystem.ec

  implicit val timeout: Timeout = new Timeout(30 seconds)

  val googleTrackingCode: String = config.get[String]("googleTrackingCode")

  val ctrl: DrtSystemInterface = DrtActorSystem.drtSystem

  ctrl.run()

  val now: () => SDateLike = () => SDate.now()

  lazy val govNotifyApiKey = config.get[String]("notifications.gov-notify-api-key")

  lazy val negativeFeedbackTemplateId = config.get[String]("notifications.negative-feedback-templateId")

  lazy val positiveFeedbackTemplateId = config.get[String]("notifications.positive-feedback-templateId")

  lazy val govNotifyReference = config.get[String]("notifications.reference")

  val virusScannerUrl: String = config.get[String]("virus-scanner-url")

  val virusScanner: VirusScanner = VirusScanner(VirusScanService(virusScannerUrl))

  val log: LoggingAdapter = system.log

  log.info(s"Starting DRTv2 build ${BuildInfo.version}")

  log.info(s"ISOChronology.getInstance: ${ISOChronology.getInstance}")

  def defaultTimeZone: String = TimeZone.getDefault.getID

  def systemTimeZone: String = System.getProperty("user.timezone")

  log.info(s"System.getProperty(user.timezone): $systemTimeZone")
  log.info(s"TimeZone.getDefault: $defaultTimeZone")
  assert(systemTimeZone == "UTC", "System Timezone is not set to UTC")
  assert(defaultTimeZone == "UTC", "Default Timezone is not set to UTC")

  log.info(s"timezone: ${Calendar.getInstance().getTimeZone}")

  def previousDay(date: MilliDate): SDateLike = {
    val oneDayInMillis = 60 * 60 * 24 * 1000L
    SDate(date.millisSinceEpoch - oneDayInMillis)
  }

  val permissionDeniedMessage = "You do not have permission manage users"

  object ApiService {
    def apply(
               airportConfig: AirportConfig,
               shiftsActor: ActorRef,
               fixedPointsActor: ActorRef,
               staffMovementsActor: ActorRef,
               headers: Headers,
               session: Session
             ): ApiService = new ApiService(airportConfig, shiftsActor, fixedPointsActor, staffMovementsActor, headers, session) {

      def actorSystem: ActorSystem = system

      def getLoggedInUser(): LoggedInUser = ctrl.getLoggedInUser(config, headers, session)

      def forecastWeekSummary(startDay: MillisSinceEpoch,
                              terminal: Terminal): Future[Option[ForecastPeriodWithHeadlines]] = {
        val (startOfForecast, endOfForecast) = startAndEndForDay(startDay, 7)

        val portStateFuture = portStateActor.ask(
          GetStateForTerminalDateRange(startOfForecast.millisSinceEpoch, endOfForecast.millisSinceEpoch, terminal)
        )(new Timeout(30 seconds))

        portStateFuture
          .map {
            case portState: PortState =>
              log.info(s"Sent forecast for week beginning ${SDate(startDay).toISOString()} on $terminal")
              val fp = services.exports.Forecast.forecastPeriod(airportConfig, terminal, startOfForecast, endOfForecast, portState)
              val hf = services.exports.Forecast.headlineFigures(startOfForecast, endOfForecast, terminal, portState, airportConfig.queuesByTerminal(terminal).toList)
              Option(ForecastPeriodWithHeadlines(fp, hf))
          }
          .recover {
            case t =>
              log.error(s"Failed to get PortState", t)
              None
          }
      }

      def updateShifts(shiftsToUpdate: Seq[StaffAssignment]): Unit = {
        if (getLoggedInUser().roles.contains(StaffEdit)) {
          log.info(s"Saving ${shiftsToUpdate.length} shift staff assignments")
          shiftsActor ! UpdateShifts(shiftsToUpdate)
        } else throw new Exception("You do not have permission to edit staffing.")
      }

      def getShiftsForMonth(month: MillisSinceEpoch, terminal: Terminal): Future[ShiftAssignments] = {
        val shiftsFuture = shiftsActor ? GetState

        shiftsFuture.collect {
          case shifts: ShiftAssignments =>
            log.info(s"Shifts: Retrieved shifts from actor for month starting: ${SDate(month).toISOString()}")
            val monthInLocalTime = SDate(month, Crunch.europeLondonTimeZone)
            StaffTimeSlots.getShiftsForMonth(shifts, monthInLocalTime)
        }
      }

      def keyCloakClient: KeyCloakClient with ProdSendAndReceive = {
        val token = headers.get("X-Auth-Token").getOrElse(throw new Exception("X-Auth-Token missing from headers, we need this to query the Key Cloak API."))
        val keyCloakUrl = config.getOptional[String]("key-cloak.url").getOrElse(throw new Exception("Missing key-cloak.url config value, we need this to query the Key Cloak API"))
        new KeyCloakClient(token, keyCloakUrl) with ProdSendAndReceive
      }

      def getKeyCloakUsers(): Future[List[KeyCloakUser]] = {
        log.info(s"Got these roles: ${getLoggedInUser().roles}")
        if (getLoggedInUser().roles.contains(ManageUsers)) {
          Future(keyCloakClient.getAllUsers().toList)
        } else throw new Exception(permissionDeniedMessage)
      }

      def getKeyCloakGroups(): Future[List[KeyCloakGroup]] = {
        if (getLoggedInUser().roles.contains(ManageUsers)) {
          keyCloakClient.getGroups
        } else throw new Exception(permissionDeniedMessage)
      }

      def getKeyCloakUserGroups(userId: UUID): Future[Set[KeyCloakGroup]] = {
        if (getLoggedInUser().roles.contains(ManageUsers)) {
          keyCloakClient.getUserGroups(userId).map(_.toSet)
        } else throw new Exception(permissionDeniedMessage)
      }

      case class KeyCloakGroups(groups: List[KeyCloakGroup])


      def addUserToGroups(userId: UUID, groups: Set[String]): Future[Unit] =
        if (getLoggedInUser().roles.contains(ManageUsers)) {
          val futureGroupIds: Future[KeyCloakGroups] = keyCloakClient
            .getGroups
            .map(kcGroups => KeyCloakGroups(kcGroups.filter(g => groups.contains(g.name))))


          futureGroupIds.map {
            case KeyCloakGroups(gps) if gps.nonEmpty =>
              log.info(s"Adding ${gps.map(_.name)} to $userId")
              gps.foreach(group => {
                val response = keyCloakClient.addUserToGroup(userId, group.id)
                response.map(res => log.info(s"Added group and got: ${res.status}  $res")
                )
              })
            case _ => log.error(s"Unable to add $userId to $groups")
          }
        } else throw new Exception(permissionDeniedMessage)

      def removeUserFromGroups(userId: UUID, groups: Set[String]): Future[Unit] =
        keyCloakClient
          .getGroups
          .map(kcGroups => kcGroups.filter(g => groups.contains(g.name))
            .foreach(g => keyCloakClient.removeUserFromGroup(userId, g.id)))

      override def portStateActor: ActorRef = ctrl.portStateActor

      def getShowAlertModalDialog(): Boolean = config
        .getOptional[Boolean]("feature-flags.display-modal-alert")
        .getOrElse(false)

    }
  }

  def autowireApi(path: String): Action[RawBuffer] = authByRole(BorderForceStaff) {
    Action.async(parse.raw) {
      implicit request =>
        log.debug(s"Request path: $path")

        val b = request.body.asBytes(parse.UNLIMITED).get

        val router = Router.route[Api](ApiService(airportConfig, ctrl.shiftsActor, ctrl.fixedPointsActor, ctrl.staffMovementsActor, request.headers, request.session))

        router(
          autowire.Core.Request(path.split("/"), Unpickle[Map[String, ByteBuffer]].fromBytes(b.asByteBuffer))
        ).map(buffer => {
          val data = Array.ofDim[Byte](buffer.remaining())
          buffer.get(data)
          Ok(data)
        })
    }
  }

  def timedEndPoint[A](name: String, maybeParams: Option[String] = None)(eventualThing: Future[A]): Future[A] = {
    val startMillis = SDate.now().millisSinceEpoch
    eventualThing.foreach { _ =>
      val endMillis = SDate.now().millisSinceEpoch
      val millisTaken = endMillis - startMillis
      Metrics.timer(s"$name", millisTaken)
      log.info(s"$name${maybeParams.map(p => s" - $p").getOrElse("")} took ${millisTaken}ms")
    }
    eventualThing
  }

  def index: Action[AnyContent] = Action { request =>
    val user = ctrl.getLoggedInUser(config, request.headers, request.session)
    if (user.hasRole(airportConfig.role))
      Ok(views.html.index("DRT - BorderForce", portCode.toString, googleTrackingCode, user.id))
    else {
      val baseDomain = config.get[String]("drt.domain")
      val isSecure = config.get[Boolean]("drt.use-https")
      val protocol = if (isSecure) "https://" else "http://"
      val fromPort = "?fromPort=" + airportConfig.portCode.toString.toLowerCase
      val redirectUrl = protocol + baseDomain + fromPort
      log.info(s"Redirecting to $redirectUrl")
      Redirect(Call("get", redirectUrl))
    }
  }

  lazy val healthChecker: HealthChecker = if (!config.get[Boolean]("health-check.disable-feed-monitoring")) {
    val healthyResponseTimeSeconds = config.get[Int]("health-check.max-response-time-seconds")
    val lastFeedCheckThresholdMinutes = config.get[Int]("health-check.max-last-feed-check-minutes")

    val feedsToMonitor = ctrl.feedActorsForPort
      .filterKeys(!airportConfig.feedSourceMonitorExemptions.contains(_))
      .values.toList

    HealthChecker(Seq(
      FeedsHealthCheck(feedsToMonitor, lastFeedCheckThresholdMinutes, now),
      ActorResponseTimeHealthCheck(ctrl.portStateActor, healthyResponseTimeSeconds * MilliTimes.oneSecondMillis))
    )
  } else HealthChecker(Seq())

  def healthCheck: Action[AnyContent] = Action.async { _ =>
    healthChecker.checksPassing.map {
      case true => Ok("health check ok")
      case _ => InternalServerError("health check failed")
    }
  }

  def apiLogin(): Action[Map[String, Seq[String]]] = Action.async(parse.tolerantFormUrlEncoded) { request =>

    def postStringValOrElse(key: String): Option[String] = {
      request.body.get(key).map(_.head)
    }

    val tokenUrlOption = config.getOptional[String]("key-cloak.token_url")
    val clientIdOption = config.getOptional[String]("key-cloak.client_id")
    val clientSecretOption = config.getOptional[String]("key-cloak.client_secret")
    val usernameOption = postStringValOrElse("username")
    val passwordOption = postStringValOrElse("password")
    import KeyCloakAuthTokenParserProtocol._
    import spray.json._

    def tokenToHttpResponse(username: String)(token: KeyCloakAuthResponse) = token match {
      case t: KeyCloakAuthToken =>
        log.info(s"Successful login to API via keycloak for $username")
        Ok(t.toJson.toString)
      case e: KeyCloakAuthError =>
        log.info(s"Failed login to API via keycloak for $username")
        BadRequest(e.toJson.toString)
    }

    def missingPostFieldsResponse = Future(
      BadRequest(KeyCloakAuthError("invalid_form_data", "You must provide a username and password").toJson.toString)
    )

    val result: Option[Future[Result]] = for {
      tokenUrl <- tokenUrlOption
      clientId <- clientIdOption
      clientSecret <- clientSecretOption
    } yield (usernameOption, passwordOption) match {
      case (Some(username), Some(password)) =>
        val authClient = new KeyCloakAuth(tokenUrl, clientId, clientSecret) with ProdSendAndReceive
        authClient.getToken(username, password).map(tokenToHttpResponse(username))
      case _ =>
        log.info(s"Invalid post fields for api login.")
        missingPostFieldsResponse
    }

    def disabledFeatureResponse = Future(NotImplemented(
      KeyCloakAuthError(
        "feature_not_implemented",
        "This feature is not currently available for this port on DRT"
      ).toJson.toString
    ))

    result match {
      case Some(f) => f.map(t => t)
      case None =>
        disabledFeatureResponse
    }
  }

  def keyCloakClient(headers: Headers): KeyCloakClient with ProdSendAndReceive = {
    val token = headers.get("X-Auth-Token").getOrElse(throw new Exception("X-Auth-Token missing from headers, we need this to query the Key Cloak API."))
    val keyCloakUrl = config.getOptional[String]("key-cloak.url").getOrElse(throw new Exception("Missing key-cloak.url config value, we need this to query the Key Cloak API"))
    new KeyCloakClient(token, keyCloakUrl) with ProdSendAndReceive
  }

  def logging: Action[Map[String, Seq[String]]] = auth {
    Action(parse.tolerantFormUrlEncoded) {
      implicit request =>

        def postStringValOrElse(key: String, default: String) = {
          request.body.get(key).map(_.head).getOrElse(default)
        }

        val logLevel = postStringValOrElse("level", "ERROR")

        val millis = request.body.get("timestamp")
          .map(_.head.toLong)
          .getOrElse(SDate.now(Crunch.europeLondonTimeZone).millisSinceEpoch)

        val logMessage = Map(
          "logger" -> ("CLIENT - " + postStringValOrElse("logger", "log")),
          "message" -> postStringValOrElse("message", "no log message"),
          "logTime" -> SDate(millis).toISOString(),
          "url" -> postStringValOrElse("url", request.headers.get("referrer").getOrElse("unknown url")),
          "logLevel" -> logLevel
        )

        log.log(Logging.levelFor(logLevel).getOrElse(Logging.ErrorLevel), s"Client Error: ${
          logMessage.map {
            case (value, key) => s"$key: $value"
          }.mkString(", ")
        }")

        Ok("logged successfully")
    }
  }
}

case class GetTerminalCrunch(terminalName: Terminal)
