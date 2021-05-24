package drt.client.services

import diode._
import diode.data._
import diode.react.ReactConnector
import drt.client.components.{FileUploadState, StaffAdjustmentDialogueState}
import drt.client.services.JSDateConversions.SDate
import drt.client.services.handlers._
import drt.shared.CrunchApi._
import drt.shared.KeyCloakApi.{KeyCloakGroup, KeyCloakUser}
import drt.shared._
import drt.shared.api.{PassengerInfoSummary, WalkTimes}
import drt.shared.dates.UtcDate
import uk.gov.homeoffice.drt.auth.LoggedInUser

import java.util.UUID
import scala.collection.immutable.Map
import scala.concurrent.duration._
import scala.language.postfixOps

sealed trait ViewMode {
  val uUID: UUID = UUID.randomUUID()

  def isDifferentTo(viewMode: ViewMode): Boolean = viewMode.uUID != uUID

  def millis: MillisSinceEpoch = time.millisSinceEpoch

  def dayStart: SDateLike = SDate.midnightOf(time)

  def dayEnd: SDateLike = dayStart.addDays(1).addMinutes(-1)

  def time: SDateLike

  def isLive: Boolean

  def isHistoric(now: SDateLike): Boolean
}

case object ViewLive extends ViewMode {
  def time: SDateLike = SDate.now()

  override val isLive: Boolean = true

  override def isHistoric(now: SDateLike): Boolean = false
}

case class NoViewMode() extends ViewMode {
  def time: SDateLike = SDate(0L)

  override val isLive: Boolean = false

  override def isHistoric(now: SDateLike): Boolean = false
}

case class ViewPointInTime(time: SDateLike) extends ViewMode {
  override val isLive: Boolean = true

  override def isHistoric(now: SDateLike): Boolean = true
}

case class ViewDay(time: SDateLike) extends ViewMode {
  override val isLive: Boolean = false

  override def isHistoric(now: SDateLike): Boolean = time.isHistoricDate(now)
}

sealed trait ExportType {
  def toUrlString: String
}

object ExportDeskRecs extends ExportType {
  override def toString = "Recommendations"

  override def toUrlString: String = "desk-recs"
}

object ExportDeployments extends ExportType {
  override def toString = "Deployments"

  override def toUrlString: String = "desk-deps"
}

object ExportArrivals extends ExportType {
  override def toString = "Arrivals"

  override def toUrlString: String = toString.toLowerCase
}

object ExportStaffMovements extends ExportType {
  override def toString = "Movements"

  override def toUrlString: String = "staff-movements"
}

case class LoadingState(isLoading: Boolean = false)

case class ClientServerVersions(client: String, server: String)

case class RootModel(applicationVersion: Pot[ClientServerVersions] = Empty,
                     latestUpdateMillis: MillisSinceEpoch = 0L,
                     portStatePot: Pot[PortState] = Empty,
                     forecastPeriodPot: Pot[ForecastPeriodWithHeadlines] = Empty,
                     airportInfos: Map[PortCode, Pot[AirportInfo]] = Map(),
                     walkTimes: Pot[WalkTimes] = Empty,
                     airportConfig: Pot[AirportConfig] = Empty,
                     arrivalSources: Option[(UniqueArrival, Pot[List[Option[FeedSourceArrival]]])] = None,
                     contactDetails: Pot[ContactDetails] = Empty,
                     shifts: Pot[ShiftAssignments] = Empty,
                     monthOfShifts: Pot[MonthOfShifts] = Empty,
                     fixedPoints: Pot[FixedPointAssignments] = Empty,
                     staffMovements: Pot[StaffMovements] = Empty,
                     viewMode: ViewMode = NoViewMode(),
                     loadingState: LoadingState = LoadingState(),
                     showActualIfAvailable: Boolean = false,
                     loggedInUserPot: Pot[LoggedInUser] = Empty,
                     userHasPortAccess: Pot[Boolean] = Empty,
                     minuteTicker: Int = 0,
                     keyCloakUsers: Pot[List[KeyCloakUser]] = Empty,
                     selectedUserGroups: Pot[Set[KeyCloakGroup]] = Empty,
                     feedStatuses: Pot[Seq[FeedSourceStatuses]] = Empty,
                     alerts: Pot[List[Alert]] = Empty,
                     maybeStaffDeploymentAdjustmentPopoverState: Option[StaffAdjustmentDialogueState] = None,
                     displayAlertDialog: Pot[Boolean] = Empty,
                     oohStatus: Pot[OutOfHoursStatus] = Empty,
                     featureFlags: Pot[FeatureFlags] = Empty,
                     fileUploadState: Pot[FileUploadState] = Empty,
                     simulationResult: Pot[SimulationResult] = Empty,
                     passengerInfoSummariesByDayPot: Pot[Map[UtcDate, Map[ArrivalKey, PassengerInfoSummary]]] = Ready(Map())
                    )

object PollDelay {
  val recoveryDelay: FiniteDuration = 10 seconds
  val loginCheckDelay: FiniteDuration = 30 seconds
  val minuteUpdateDelay: FiniteDuration = 10 seconds
  val oohSupportUpdateDelay: FiniteDuration = 1 minute
  val checkFeatureFlagsDelay: FiniteDuration = 10 minutes
  val passengerInfoDelay: FiniteDuration = 1 minutes
  val passengerInfoDelayWaitingForFlights: FiniteDuration = 1 second

}

trait DrtCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  val blockWidth = 15

  override protected def initialModel = RootModel()


  def currentViewMode: () => ViewMode = () => zoom(_.viewMode).value

  def airportConfigPot(): Pot[AirportConfig] = zoomTo(_.airportConfig).value

  def pointInTimeMillis: MillisSinceEpoch = zoom(_.viewMode).value.millis

  override val actionHandler: HandlerFunction = {
    val composedHandlers: HandlerFunction = composeHandlers(
      new InitialPortStateHandler(currentViewMode, zoomRW(m => (m.portStatePot, m.latestUpdateMillis))((m, v) => m.copy(portStatePot = v._1, latestUpdateMillis = v._2))),
      new PortStateUpdatesHandler(currentViewMode, zoomRW(m => (m.portStatePot, m.latestUpdateMillis))((m, v) => m.copy(portStatePot = v._1, latestUpdateMillis = v._2))),
      new ForecastHandler(zoomRW(_.forecastPeriodPot)((m, v) => m.copy(forecastPeriodPot = v))),
      new AirportCountryHandler(zoomRW(_.airportInfos)((m, v) => m.copy(airportInfos = v))),
      new PassengerInfoSummaryHandler(zoom(_.portStatePot), zoomRW(_.passengerInfoSummariesByDayPot)((m, v) => m.copy(passengerInfoSummariesByDayPot = v))),
      new ArrivalSourcesHandler(zoomRW(_.arrivalSources)((m, v) => m.copy(arrivalSources = v))),
      new AirportConfigHandler(zoomRW(_.airportConfig)((m, v) => m.copy(airportConfig = v))),
      new ContactDetailsHandler(zoomRW(_.contactDetails)((m, v) => m.copy(contactDetails = v))),
      new OohForSupportHandler(zoomRW(_.oohStatus)((m, v) => m.copy(oohStatus = v))),
      new FeatureFlagHandler(zoomRW(_.featureFlags)((m, v) => m.copy(featureFlags = v))),
      new ApplicationVersionHandler(zoomRW(_.applicationVersion)((m, v) => m.copy(applicationVersion = v))),
      new ShiftsHandler(currentViewMode, zoomRW(_.shifts)((m, v) => m.copy(shifts = v))),
      new ShiftsForMonthHandler(zoomRW(_.monthOfShifts)((m, v) => m.copy(monthOfShifts = v))),
      new FixedPointsHandler(currentViewMode, zoomRW(_.fixedPoints)((m, v) => m.copy(fixedPoints = v))),
      new StaffMovementsHandler(currentViewMode, zoomRW(_.staffMovements)((m, v) => m.copy(staffMovements = v))),
      new ViewModeHandler(zoomRW(m => (m.viewMode, m.portStatePot, m.latestUpdateMillis))((m, v) => m.copy(viewMode = v._1, portStatePot = v._2, latestUpdateMillis = v._3))),
      new LoaderHandler(zoomRW(_.loadingState)((m, v) => m.copy(loadingState = v))),
      new ShowActualDesksAndQueuesHandler(zoomRW(_.showActualIfAvailable)((m, v) => m.copy(showActualIfAvailable = v))),
      new ShowAlertModalDialogHandler(zoomRW(_.displayAlertDialog)((m, v) => m.copy(displayAlertDialog = v))),
      new RetryHandler(zoomRW(identity)((m, _) => m)),
      new LoggedInStatusHandler(zoomRW(identity)((m, _) => m)),
      new NoopHandler(zoomRW(identity)((m, _) => m)),
      new LoggedInUserHandler(zoomRW(_.loggedInUserPot)((m, v) => m.copy(loggedInUserPot = v))),
      new UserDashboardHandler(zoomRW(_.loggedInUserPot)((m, v) => m.copy(loggedInUserPot = v))),
      new UserHasPortAccessHandler(zoomRW(_.userHasPortAccess)((m, v) => m.copy(userHasPortAccess = v))),
      new UsersHandler(zoomRW(_.keyCloakUsers)((m, v) => m.copy(keyCloakUsers = v))),
      new EditUserHandler(zoomRW(_.selectedUserGroups)((m, v) => m.copy(selectedUserGroups = v))),
      new MinuteTickerHandler(zoomRW(_.minuteTicker)((m, v) => m.copy(minuteTicker = v))),
      new FeedsStatusHandler(zoomRW(_.feedStatuses)((m, v) => m.copy(feedStatuses = v))),
      new AlertsHandler(zoomRW(_.alerts)((m, v) => m.copy(alerts = v))),
      new StaffAdjustmentDialogueStateHandler(zoomRW(_.maybeStaffDeploymentAdjustmentPopoverState)((m, v) => m.copy(maybeStaffDeploymentAdjustmentPopoverState = v))),
      new ForecastFileUploadHandler(zoomRW(_.fileUploadState)((m, v) => m.copy(fileUploadState = v))),
      new WalkTimeHandler(zoomRW(_.walkTimes)((m, v) => m.copy(walkTimes = v))),
      new SimulationHandler(zoomRW(_.simulationResult)((m, v) => m.copy(simulationResult = v))),
    )

    composedHandlers
  }
}

object SPACircuit extends DrtCircuit
