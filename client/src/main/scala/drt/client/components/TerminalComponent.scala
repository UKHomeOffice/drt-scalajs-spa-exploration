package drt.client.components

import diode.UseValueEq
import diode.data.Pot
import drt.client.SPAMain.{Loc, TerminalPageTabLoc, UrlDateParameter}
import drt.client.components.ToolTips._
import drt.client.logger.{Logger, LoggerFactory}
import drt.client.modules.GoogleEventTracker
import drt.client.services.JSDateConversions.SDate
import drt.client.services._
import drt.shared.CrunchApi.ForecastPeriodWithHeadlines
import drt.shared._
import drt.shared.api.{PassengerInfoSummary, WalkTimes}
import drt.shared.dates.UtcDate
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CtorType, ScalaComponent}
import uk.gov.homeoffice.drt.auth.LoggedInUser
import uk.gov.homeoffice.drt.auth.Roles.StaffEdit

import scala.collection.immutable.Map

object TerminalComponent {

  val log: Logger = LoggerFactory.getLogger("TerminalComponent")

  case class Props(terminalPageTab: TerminalPageTabLoc, router: RouterCtl[Loc])

  case class TerminalModel(
                            portStatePot: Pot[PortState],
                            passengerInfoByDayPot: Pot[Map[UtcDate, Map[ArrivalKey, PassengerInfoSummary]]],
                            forecastPeriodPot: Pot[ForecastPeriodWithHeadlines],
                            potShifts: Pot[ShiftAssignments],
                            potMonthOfShifts: Pot[MonthOfShifts],
                            potFixedPoints: Pot[FixedPointAssignments],
                            potStaffMovements: Pot[StaffMovements],
                            airportConfig: Pot[AirportConfig],
                            loadingState: LoadingState,
                            showActuals: Boolean,
                            loggedInUserPot: Pot[LoggedInUser],
                            viewMode: ViewMode,
                            minuteTicker: Int,
                            maybeStaffAdjustmentsPopoverState: Option[StaffAdjustmentDialogueState],
                            featureFlags: Pot[FeatureFlags],
                            arrivalSources: Option[(UniqueArrival, Pot[List[Option[FeedSourceArrival]]])],
                            potWalkTimes: Pot[WalkTimes]
                          ) extends UseValueEq

  val component: Component[Props, Unit, Unit, CtorType.Props] = ScalaComponent.builder[Props]("Terminal")
    .render_P(props => {
      val modelRCP = SPACircuit.connect(model => TerminalModel(
        model.portStatePot,
        model.passengerInfoSummariesByDayPot,
        model.forecastPeriodPot,
        model.shifts,
        model.monthOfShifts,
        model.fixedPoints,
        model.staffMovements,
        model.airportConfig,
        model.loadingState,
        model.showActualIfAvailable,
        model.loggedInUserPot,
        model.viewMode,
        model.minuteTicker,
        model.maybeStaffDeploymentAdjustmentPopoverState,
        model.featureFlags,
        model.arrivalSources,
        model.walkTimes
      ))

      val dialogueStateRCP = SPACircuit.connect(_.maybeStaffDeploymentAdjustmentPopoverState)


      <.div(
        dialogueStateRCP(dialogueStateMP => <.div(dialogueStateMP().map(dialogueState => StaffAdjustmentDialogue(dialogueState)()).whenDefined)),
        modelRCP(modelMP => {
          val model = modelMP()
          <.div(model.airportConfig.render(airportConfig => {

            val timeRangeHours = if (model.viewMode == ViewLive) CurrentWindow() else WholeDayWindow()

            val currentClass = if (props.terminalPageTab.mode == "current") "active" else ""
            val snapshotDataClass = if (props.terminalPageTab.mode == "snapshot") "active" else ""
            val planningClass = if (props.terminalPageTab.mode == "planning") "active" else ""
            val staffingClass = if (props.terminalPageTab.mode == "staffing") "active" else ""
            val terminalDashboardClass = if (props.terminalPageTab.mode == "dashboard") "active" else ""

            val currentContentClass = if (props.terminalPageTab.mode == "current") "fade in active" else "fade out"
            val snapshotContentClass = if (props.terminalPageTab.mode == "snapshot") "fade in active" else "fade out"
            val planningContentClass = if (props.terminalPageTab.mode == "planning") "fade in active" else "fade out"
            val staffingContentClass = if (props.terminalPageTab.mode == "staffing") "fade in active" else "fade out"
            val dashboardContentClass = if (props.terminalPageTab.mode == "dashboard") "fade in active" else "fade out"

            val subMode = if (props.terminalPageTab.mode != "current" && props.terminalPageTab.mode != "snapshot")
              "desksAndQueues"
            else
              props.terminalPageTab.subMode

            val terminalName = props.terminalPageTab.terminal.toString

            model.loggedInUserPot.render { loggedInUser =>

              val terminalContentProps = TerminalContentComponent.Props(
                model.portStatePot,
                model.passengerInfoByDayPot,
                model.potShifts,
                model.potFixedPoints,
                model.potStaffMovements,
                airportConfig,
                props.terminalPageTab,
                timeRangeHours,
                props.router,
                model.showActuals,
                model.viewMode,
                loggedInUser,
                model.minuteTicker,
                model.featureFlags,
                model.arrivalSources,
                model.potWalkTimes,
              )
              <.div(
                <.ul(^.className := "nav nav-tabs",
                  <.li(^.className := terminalDashboardClass,
                    <.a(^.id := "terminalDashboardTab", VdomAttr("data-toggle") := "tab", "Terminal Dashboard"), ^.onClick --> {
                      GoogleEventTracker.sendEvent(terminalName, "click", "Terminal Dashboard")
                      props.router.set(
                        props.terminalPageTab.copy(
                          mode = "dashboard",
                          subMode = "summary",
                          queryParams = props.terminalPageTab.withUrlParameters(UrlDateParameter(None)).queryParams)
                      )
                    }
                  ),
                  <.li(^.className := currentClass,
                    <.a(^.id := "currentTab", VdomAttr("data-toggle") := "tab", "Current", " ", currentTooltip), ^.onClick --> {
                      GoogleEventTracker.sendEvent(terminalName, "click", "Current")
                      props.router.set(props.terminalPageTab.copy(
                        mode = "current",
                        subMode = subMode,
                        queryParams = props.terminalPageTab.withUrlParameters(UrlDateParameter(None)).queryParams
                      ))
                    }),
                  <.li(^.className := snapshotDataClass,
                    <.a(^.id := "snapshotTab", VdomAttr("data-toggle") := "tab", "Snapshot", " ", snapshotTooltip),
                    ^.onClick --> {
                      GoogleEventTracker.sendEvent(terminalName, "click", "Snapshot")
                      props.router.set(props.terminalPageTab.copy(
                        mode = "snapshot",
                        subMode = subMode,
                        queryParams = props.terminalPageTab.withUrlParameters(UrlDateParameter(None)).queryParams
                      ))
                    }
                  ),
                  <.li(^.className := planningClass,
                    <.a(^.id := "planningTab", VdomAttr("data-toggle") := "tab", "Planning"),
                    ^.onClick --> {
                      GoogleEventTracker.sendEvent(terminalName, "click", "Planning")
                      props.router.set(props.terminalPageTab.copy(mode = "planning", subMode = subMode, queryParams = props.terminalPageTab.withUrlParameters(UrlDateParameter(None)).queryParams))
                    }
                  ),
                  if (loggedInUser.roles.contains(StaffEdit))
                    <.li(^.className := staffingClass,
                      <.a(^.id := "monthlyStaffingTab", VdomAttr("data-toggle") := "tab", "Monthly Staffing", " ", monthlyStaffingTooltip),
                      ^.onClick --> {
                        GoogleEventTracker.sendEvent(terminalName, "click", "Monthly Staffing")
                        props.router.set(props.terminalPageTab.copy(mode = "staffing", subMode = "15", queryParams = props.terminalPageTab.withUrlParameters(UrlDateParameter(None)).queryParams))
                      }
                    ) else ""
                ),
                <.div(^.className := "tab-content",
                  <.div(^.id := "dashboard", ^.className := s"tab-pane terminal-dashboard-container $dashboardContentClass",
                    if (props.terminalPageTab.mode == "dashboard") {
                      terminalContentProps.portStatePot.renderReady(ps =>
                        terminalContentProps.passengerInfoByDayPot.renderReady(paxInfo => {
                          TerminalDashboardComponent(
                            props.terminalPageTab,
                            terminalContentProps.airportConfig,
                            ps,
                            paxInfo,
                            props.router,
                            model.featureFlags,
                            model.potWalkTimes,
                            loggedInUser
                          )
                        }))
                    } else ""
                  ),
                  <.div(^.id := "current", ^.className := s"tab-pane $currentContentClass", {
                    if (props.terminalPageTab.mode == "current") <.div(
                      <.h2(props.terminalPageTab.dateFromUrlOrNow match {
                        case date: SDateLike if date.ddMMyyString == SDate.now().ddMMyyString => "Live View"
                        case date: SDateLike if date.millisSinceEpoch < SDate.now().millisSinceEpoch => "Historic View"
                        case date: SDateLike if date.millisSinceEpoch > SDate.now().millisSinceEpoch => "Forecast View"
                        case _ => "Live View"
                      }),
                      <.div(^.className := "content-head",
                        PcpPaxSummariesComponent(terminalContentProps.portStatePot, terminalContentProps.viewMode, props.terminalPageTab.terminal, model.minuteTicker),
                        DaySelectorComponent(DaySelectorComponent.Props(props.router,
                          props.terminalPageTab,
                          model.loadingState,
                          model.minuteTicker
                        ))
                      ),
                      TerminalContentComponent(terminalContentProps)
                    ) else ""
                  }),
                  <.div(^.id := "snapshot", ^.className := s"tab-pane $snapshotContentClass", {
                    if (props.terminalPageTab.mode == "snapshot") <.div(
                      <.h2("Snapshot View"),
                      SnapshotSelector(props.router, props.terminalPageTab, model.loadingState),
                      TerminalContentComponent(terminalContentProps)
                    ) else ""
                  }),
                  <.div(^.id := "planning", ^.className := s"tab-pane $planningContentClass", {
                    if (props.terminalPageTab.mode == "planning") {
                      <.div(
                        <.div(model.forecastPeriodPot.render(fp => {
                          TerminalPlanningComponent(TerminalPlanningComponent.Props(fp, props.terminalPageTab, props.router))
                        }))
                      )
                    } else ""
                  }),
                  if (loggedInUser.roles.contains(StaffEdit))
                    <.div(^.id := "staffing", ^.className := s"tab-pane terminal-staffing-container $staffingContentClass",
                      if (props.terminalPageTab.mode == "staffing") {
                        model.potMonthOfShifts.render(ms => {
                          MonthlyStaffing(ms.shifts, props.terminalPageTab, props.router)
                        })
                      } else ""
                    ) else ""
                )
              )
            }
          }))
        })
      )
    })
    .build

  def apply(props: Props): VdomElement = component(props)
}
