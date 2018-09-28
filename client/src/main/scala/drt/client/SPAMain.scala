package drt.client

import java.util.UUID
import diode.Action
import drt.client.actions.Actions._
import drt.client.components.TerminalDesksAndQueues.{ViewDeps, ViewRecs, ViewType}
import drt.client.components.{AlertsPage, EditKeyCloakUserPage, GlobalStyles, KeyCloakUsersPage, Layout, StatusPage, TerminalComponent, TerminalPlanningComponent, TerminalsDashboardPage}
import drt.client.logger._
import drt.client.services.JSDateConversions.SDate
import drt.client.services._
import drt.client.services.handlers.GetFeedStatuses
import drt.shared.SDateLike
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router._
import org.scalajs.dom
import scalacss.ProdDefaults._
import scala.collection.immutable.Seq
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object SPAMain {

  sealed trait Loc

  sealed trait UrlParameter {
    val name: String
    val value: Option[String]
  }

  case class UrlDateParameter(value: Option[String]) extends UrlParameter {
    val name = "date"
  }

  case class UrlTimeRangeStart(value: Option[String]) extends UrlParameter {
    val name = "timeRangeStart"
  }

  case class UrlTimeRangeEnd(value: Option[String]) extends UrlParameter {
    val name = "timeRangeEnd"
  }

  case class UrlViewType(viewType: Option[ViewType]) extends UrlParameter {
    val name = "viewType"
    override val value: Option[String] = viewType.map(_.toString)
  }

  case class TerminalPageTabLoc(
                                  terminal: String,
                                  mode: String = "current",
                                  subMode: String = "desksAndQueues",
                                  queryParams: Map[String, String] = Map.empty[String, String]
                                ) extends Loc {
    val date: Option[String] = queryParams.get(UrlDateParameter(None).name).filter(_.matches(".+"))
    val timeRangeStartString: Option[String] = queryParams.get(UrlTimeRangeStart(None).name).filter(_.matches("[0-9]+"))
    val timeRangeEndString: Option[String] = queryParams.get(UrlTimeRangeEnd(None).name).filter(_.matches("[0-9]+"))
    val viewType: ViewType = queryParams.get(UrlViewType(None).name).map(vt => if (ViewRecs.toString.contains(vt)) ViewRecs else ViewDeps).getOrElse(ViewDeps)

    def viewMode: ViewMode = {
      (mode, date) match {
        case ("current", Some(dateString)) => ViewDay(parseDateString(dateString))
        case ("snapshot", dateStringOption) => ViewPointInTime(dateStringOption.map(parseDateString)
          .getOrElse(SDate.midnightThisMorning()))
        case _ => ViewLive()
      }
    }

    def withUrlParameters(urlParameters: Array[UrlParameter]) : TerminalPageTabLoc = {
      def unset(allParams: Map[String, String], key:  String) = allParams.filterNot(kv=> kv._1.contains(key))
      def set(allParams: Map[String, String], key: String, value: String) = allParams + (key -> value)
      val newQueryParams = urlParameters.foldLeft(queryParams)((allParams, current) => current.value.map(value => set(allParams, current.name, value)).getOrElse(unset(allParams, current.name)))
      copy(queryParams= newQueryParams)
    }

    def parseDateString(s: String): SDateLike = SDate(s.replace("%20", " ").split(" ").mkString("T"))

    def timeRangeStart: Option[Int] = timeRangeStartString.map(_.toInt)

    def timeRangeEnd: Option[Int] = timeRangeEndString.map(_.toInt)

    def dateFromUrlOrNow: SDateLike = {
      date.map(parseDateString).getOrElse(SDate.now())
    }

    def updateRequired(p: TerminalPageTabLoc): Boolean = (terminal != p.terminal) || (date != p.date) || (mode != p.mode)

    def loadAction: Action = mode match {
      case "planning" =>
        GetForecastWeek(TerminalPlanningComponent.defaultStartDate(dateFromUrlOrNow), terminal)
      case "staffing" =>
        log.info(s"dispatching get shifts for month on staffing page")
        GetShiftsForMonth(dateFromUrlOrNow, terminal)
      case _ => SetViewMode(viewMode)
    }
  }

  def serverLogEndpoint: String = BaseUrl.until_#(Path("/logging")).value

  case class TerminalsDashboardLoc(period: Option[Int]) extends Loc

  case object StatusLoc extends Loc

  case object KeyCloakUsersLoc extends Loc

  case class KeyCloakUserEditLoc(userId: UUID) extends Loc

  case object AlertLoc extends Loc

  def requestInitialActions(): Unit = {
    val initActions = Seq(
      GetApplicationVersion,
      GetLoggedInUser,
      GetUserHasPortAccess,
      GetLoggedInStatus,
      GetAirportConfig(),
      GetFixedPoints(),
      UpdateMinuteTicker,
      GetFeedStatuses(),
      GetAlerts(0L)
    )

    initActions.foreach(SPACircuit.dispatch(_))
  }

  val routerConfig: RouterConfig[Loc] = RouterConfigDsl[Loc]
    .buildConfig { dsl =>
      import dsl._

      val rule = homeRoute(dsl) | dashboardRoute(dsl) | terminalRoute(dsl) | statusRoute(dsl) | keyCloakUsersRoute(dsl) | keyCloakUserEditRoute(dsl) | alertRoute(dsl)

      rule.notFound(redirectToPage(TerminalsDashboardLoc(None))(Redirect.Replace))
    }
    .renderWith(layout)
    .onPostRender((maybePrevLoc, currentLoc) => {
      Callback(
        (maybePrevLoc, currentLoc) match {
          case (Some(p: TerminalPageTabLoc), c: TerminalPageTabLoc) =>
            if (c.updateRequired(p)) SPACircuit.dispatch(c.loadAction)
          case (_, c: TerminalPageTabLoc) =>
            log.info(s"Triggering post load action for $c")
            SPACircuit.dispatch(c.loadAction)
          case (_, _: TerminalsDashboardLoc) =>
            SPACircuit.dispatch(SetViewMode(ViewLive()))
          case (_, KeyCloakUsersLoc) =>
            SPACircuit.dispatch(GetKeyCloakUsers)
          case (_, KeyCloakUserEditLoc(userId)) =>
            SPACircuit.dispatch(GetUserGroups(userId))
          case _ =>
        }
      )
    })

  def homeRoute(dsl: RouterConfigDsl[Loc]): dsl.Rule = {
    import dsl._

    staticRoute(root, TerminalsDashboardLoc(None)) ~> renderR((router: RouterCtl[Loc]) => TerminalsDashboardPage(None, router))
  }

  def statusRoute(dsl: RouterConfigDsl[Loc]): dsl.Rule = {
    import dsl._

    staticRoute("#status", StatusLoc) ~> renderR((router: RouterCtl[Loc]) => StatusPage())
  }

  def keyCloakUsersRoute(dsl: RouterConfigDsl[Loc]): dsl.Rule = {
    import dsl._

    staticRoute("#users", KeyCloakUsersLoc) ~> renderR((router: RouterCtl[Loc]) => KeyCloakUsersPage(router))
  }

  def keyCloakUserEditRoute(dsl: RouterConfigDsl[Loc]): dsl.Rule = {
    import dsl._

    dynamicRouteCT(("#editUser" / uuid ).caseClass[KeyCloakUserEditLoc]) ~>
      dynRenderR((page: KeyCloakUserEditLoc, router) => {
        EditKeyCloakUserPage(page.userId)
      })
  }

  def alertRoute(dsl: RouterConfigDsl[Loc]): dsl.Rule = {
    import dsl._

    staticRoute("#alerts", AlertLoc) ~> renderR((router: RouterCtl[Loc]) => AlertsPage())
  }

  def dashboardRoute(dsl: RouterConfigDsl[Loc]): dsl.Rule = {
    import dsl._

    dynamicRouteCT(("#terminalsDashboard" / int.option).caseClass[TerminalsDashboardLoc]) ~>
      dynRenderR((page: TerminalsDashboardLoc, router) => {
        TerminalsDashboardPage(None, router, page)
      })
  }

  def terminalRoute(dsl: RouterConfigDsl[Loc]): dsl.Rule = {
    import dsl._
    import modules.AdditionalDsl._

    val requiredTerminalName = string("[a-zA-Z0-9]+")
    val requiredTopLevelTab = string("[a-zA-Z0-9]+")
    val requiredSecondLevelTab = string("[a-zA-Z0-9]+")

    dynamicRouteCT(
      ("#terminal" / requiredTerminalName / requiredTopLevelTab / requiredSecondLevelTab / "" ~ queryToMap).caseClass[TerminalPageTabLoc]) ~>
      dynRenderR((page: TerminalPageTabLoc, router) => {
        val props = TerminalComponent.Props(terminalPageTab = page, router)
        TerminalComponent(props)
      })
  }

  def layout(c: RouterCtl[Loc], r: Resolution[Loc]) = Layout(c, r)

  def pathToThisApp: String = dom.document.location.pathname

  @JSExportTopLevel("SPAMain")
  protected def getInstance(): this.type = this

  @JSExport
  def main(): Unit = {
    log.debug("Application starting")

    ErrorHandler.registerGlobalErrorHandler()

    import scalacss.ScalaCssReact._

    GlobalStyles.addToDocument()

    requestInitialActions()

    val router = Router(BaseUrl.until_#, routerConfig.logToConsole)
    router().renderIntoDOM(dom.document.getElementById("root"))
  }
}
