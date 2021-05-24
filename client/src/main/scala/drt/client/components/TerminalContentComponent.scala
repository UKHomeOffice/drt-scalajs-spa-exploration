package drt.client.components

import diode.data.{Pending, Pot}
import diode.react.{ModelProxy, ReactConnectProxy}
import drt.client.SPAMain
import drt.client.SPAMain.{Loc, TerminalPageTabLoc}
import drt.client.components.FlightComponents.SplitsGraph.splitsGraphComponentColoured
import drt.client.components.ToolTips._
import drt.client.components.scenarios.ScenarioSimulationComponent
import drt.client.logger.log
import drt.client.modules.GoogleEventTracker
import drt.client.services.JSDateConversions.SDate
import drt.client.services._
import drt.shared.Queues.Queue
import drt.shared._
import drt.shared.api.{PassengerInfoSummary, WalkTimes}
import drt.shared.dates.UtcDate
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.{<, VdomAttr, VdomElement, ^, _}
import japgolly.scalajs.react.{Callback, CtorType, ScalaComponent}
import org.scalajs.dom.html.{Anchor, Div}
import uk.gov.homeoffice.drt.auth.Roles.{ArrivalSimulationUpload, Role, StaffMovementsExport}
import uk.gov.homeoffice.drt.auth._

import scala.collection.immutable.Map

object TerminalContentComponent {

  case class Props(portStatePot: Pot[PortState],
                   passengerInfoByDayPot: Pot[Map[UtcDate, Map[ArrivalKey, PassengerInfoSummary]]],
                   potShifts: Pot[ShiftAssignments],
                   potFixedPoints: Pot[FixedPointAssignments],
                   potStaffMovements: Pot[StaffMovements],
                   airportConfig: AirportConfig,
                   terminalPageTab: TerminalPageTabLoc,
                   defaultTimeRangeHours: TimeRangeHours,
                   router: RouterCtl[Loc],
                   showActuals: Boolean,
                   viewMode: ViewMode,
                   loggedInUser: LoggedInUser,
                   minuteTicker: Int,
                   featureFlags: Pot[FeatureFlags],
                   arrivalSources: Option[(UniqueArrival, Pot[List[Option[FeedSourceArrival]]])],
                   potWalkTimes: Pot[WalkTimes]
                  )

  case class State(activeTab: String, showExportDialogue: Boolean = false)

  def viewStartAndEnd(day: SDateLike, range: TimeRangeHours): (SDateLike, SDateLike) = {
    val startOfDay = SDate(day.getFullYear(), day.getMonth(), day.getDate())
    val startOfView = startOfDay.addHours(range.start)
    val endOfView = startOfDay.addHours(range.end)
    (startOfView, endOfView)
  }

  def airportWrapper(portCode: PortCode): ReactConnectProxy[Pot[AirportInfo]] = SPACircuit.connect(_.airportInfos.getOrElse(portCode, Pending()))

  def originMapper(portCode: PortCode): VdomElement = airportWrapper(portCode) {
    proxy: ModelProxy[Pot[AirportInfo]] =>
      <.span(^.className := "flight-origin",
        proxy().render(ai => Tippy.describe(<.span(s"${ai.airportName}, ${ai.city}, ${ai.country}"), portCode.toString)),
        proxy().renderEmpty(<.span(portCode.toString))
      )
  }

  class Backend() {

    val arrivalsTableComponent: Component[FlightsWithSplitsTable.Props, Unit, Unit, CtorType.Props] = FlightsWithSplitsTable.ArrivalsTable(
      None,
      originMapper,
      splitsGraphComponentColoured)

    def render(props: Props, state: State): TagOf[Div] = {
      val terminal = props.terminalPageTab.terminal
      val queueOrder: Seq[Queue] = props.airportConfig.queueTypeSplitOrder(terminal)

      val desksAndQueuesActive = if (state.activeTab == "desksAndQueues") "active" else ""
      val arrivalsActive = if (state.activeTab == "arrivals") "active" else ""
      val staffingActive = if (state.activeTab == "staffing") "active" else ""
      val simulationsActive = if (state.activeTab == "simulations") "active" else ""

      val desksAndQueuesPanelActive = if (state.activeTab == "desksAndQueues") "active" else "fade"
      val arrivalsPanelActive = if (state.activeTab == "arrivals") "active" else "fade"
      val staffingPanelActive = if (state.activeTab == "staffing") "active" else "fade"
      val viewModeStr = props.terminalPageTab.viewMode.getClass.getSimpleName.toLowerCase

      val timeRangeHours: CustomWindow = timeRange(props)

      <.div(
        props.portStatePot.renderPending(_ => if (props.portStatePot.isEmpty) <.div(^.id := "terminal-spinner", Icon.spinner) else ""),
        props.portStatePot.renderEmpty(if (!props.portStatePot.isPending) {
          <.div(^.id := "terminal-data", "Nothing to show for this time period")
        } else ""),
        props.portStatePot.render((portState: PortState) => {
          props.passengerInfoByDayPot.renderReady(passengerInfoByDay => {
            props.potWalkTimes.renderReady(walkTimes => {

              val queues = props.airportConfig.queuesByTerminal.filterKeys(_ == terminal)
              val (viewStart, viewEnd) = viewStartAndEnd(props.terminalPageTab.viewMode.time, timeRangeHours)
              val filteredPortState = portState.windowWithTerminalFilter(viewStart, viewEnd, queues)
              val terminalName = terminal.toString
              <.div(^.className := s"view-mode-content $viewModeStr",
                <.div(^.className := "tabs-with-export",
                  <.ul(^.className := "nav nav-tabs",
                    <.li(^.className := desksAndQueuesActive,
                      <.a(^.id := "desksAndQueuesTab", VdomAttr("data-toggle") := "tab", "Desks & Queues"), ^.onClick --> {
                        GoogleEventTracker.sendEvent(terminalName, "Desks & Queues", props.terminalPageTab.dateFromUrlOrNow.toISODateOnly)
                        props.router.set(props.terminalPageTab.copy(subMode = "desksAndQueues"))
                      }),
                    <.li(^.className := arrivalsActive,
                      <.a(^.id := "arrivalsTab", VdomAttr("data-toggle") := "tab", "Arrivals"), ^.onClick --> {
                        GoogleEventTracker.sendEvent(terminalName, "Arrivals", props.terminalPageTab.dateFromUrlOrNow.toISODateOnly)
                        props.router.set(props.terminalPageTab.copy(subMode = "arrivals"))
                      }),
                    <.li(^.className := staffingActive,
                      <.a(^.id := "staffMovementsTab", VdomAttr("data-toggle") := "tab", "Staff Movements", " ", staffMovementsTabTooltip), ^.onClick --> {
                        GoogleEventTracker.sendEvent(terminalName, "Staff Movements", props.terminalPageTab.dateFromUrlOrNow.toISODateOnly)
                        props.router.set(props.terminalPageTab.copy(subMode = "staffing"))
                      }),
                    displayForRole(
                      <.li(^.className := simulationsActive,
                        <.a(^.id := "simulationDayTab", VdomAttr("data-toggle") := "tab", "Simulate Day"), ^.onClick --> {
                          GoogleEventTracker.sendEvent(terminalName, "Simulate Day", props.terminalPageTab.dateFromUrlOrNow.toISODateOnly)
                          props.router.set(props.terminalPageTab.copy(subMode = "simulations"))
                        }),
                      ArrivalSimulationUpload, props.loggedInUser
                    )
                  ),
                  <.div(^.className := "exports",
                    exportLink(
                      props.terminalPageTab.dateFromUrlOrNow,
                      terminalName,
                      ExportArrivals,
                      SPAMain.exportArrivalViewUrl(props.terminalPageTab.viewMode, terminal)
                    ),
                    exportLink(
                      props.terminalPageTab.dateFromUrlOrNow,
                      terminalName,
                      ExportDeskRecs,
                      SPAMain.exportDesksUrl(ExportDeskRecs, props.terminalPageTab.viewMode, terminal)
                    ),
                    exportLink(
                      props.terminalPageTab.dateFromUrlOrNow,
                      terminalName,
                      ExportDeployments,
                      SPAMain.exportDesksUrl(ExportDeployments, props.terminalPageTab.viewMode, terminal)
                    ),
                    displayForRole(
                      exportLink(
                        props.terminalPageTab.dateFromUrlOrNow,
                        terminalName,
                        ExportStaffMovements,
                        SPAMain.absoluteUrl(
                          s"export/staff-movements/${props.terminalPageTab.viewMode.millis}/$terminal?pointInTime=${props.viewMode.millis}"
                        )
                      ),
                      StaffMovementsExport,
                      props.loggedInUser
                    ),
                    MultiDayExportComponent(terminal, props.terminalPageTab.dateFromUrlOrNow, props.loggedInUser))),
                <.div(^.className := "tab-content",
                  <.div(^.id := "desksAndQueues", ^.className := s"tab-pane terminal-desk-recs-container $desksAndQueuesPanelActive",
                    if (state.activeTab == "desksAndQueues") {
                      val (viewStart, _) = viewStartAndEnd(props.terminalPageTab.viewMode.time, timeRangeHours)
                      props.featureFlags.render(features =>
                        TerminalDesksAndQueues(
                          TerminalDesksAndQueues.Props(
                            props.router,
                            filteredPortState,
                            viewStart,
                            timeRangeHours.end - timeRangeHours.start,
                            props.airportConfig,
                            props.terminalPageTab,
                            props.showActuals,
                            props.viewMode,
                            props.loggedInUser,
                            features
                          )
                        ))
                    } else ""
                  ),
                  <.div(^.id := "arrivals", ^.className := s"tab-pane in $arrivalsPanelActive", {
                    if (state.activeTab == "arrivals") {
                      val flightsForTerminal = filteredPortState.flights.values.toList
                      props.featureFlags.render(features =>
                        arrivalsTableComponent(
                          FlightsWithSplitsTable.Props(
                            flightsForTerminal,
                            passengerInfoByDay,
                            queueOrder,
                            props.airportConfig.hasEstChox,
                            props.arrivalSources,
                            props.loggedInUser,
                            props.viewMode,
                            walkTimes,
                            props.airportConfig.defaultWalkTimeMillis(props.terminalPageTab.terminal),
                            props.airportConfig.hasTransfer,
                            features.displayRedListInfo
                          )
                        )
                      )
                    } else ""
                  }),
                  displayForRole(
                    <.div(^.id := "simluations", ^.className := s"tab-pane in $simulationsActive", {
                      if (state.activeTab == "simulations") {

                        props.portStatePot.renderReady(ps =>
                          ScenarioSimulationComponent(
                            props.viewMode.dayStart.toLocalDate,
                            props.terminalPageTab.terminal,
                            props.airportConfig,
                            ps.window(props.viewMode.dayStart, props.viewMode.dayStart.getLocalNextMidnight)
                          )
                        )
                      } else "not rendering"
                    }),
                    ArrivalSimulationUpload,
                    props.loggedInUser
                  ),
                  <.div(^.id := "available-staff", ^.className := s"tab-pane terminal-staffing-container $staffingPanelActive",
                    if (state.activeTab == "staffing") {
                      TerminalStaffing(TerminalStaffing.Props(
                        terminal,
                        props.potShifts,
                        props.potFixedPoints,
                        props.potStaffMovements,
                        props.airportConfig,
                        props.loggedInUser,
                        props.viewMode
                      ))
                    } else ""
                  ))
              )
            })
          })
        }))
    }
  }

  def exportLink(exportDay: SDateLike,
                 terminalName: String,
                 exportType: ExportType,
                 exportUrl: String
                ): VdomTagOf[Anchor] =
    <.a(Icon.download, s" $exportType",
      ^.className := "btn btn-default",
      ^.href := exportUrl,
      ^.target := "_blank",
      ^.id := s"export-day-${exportType.toUrlString}",
      ^.onClick --> {
        Callback(GoogleEventTracker.sendEvent(terminalName, s"Export $exportType", exportDay.toISODateOnly))
      })

  def displayForRole(node: VdomNode, role: Role, loggedInUser: LoggedInUser): TagMod =
    if (loggedInUser.hasRole(role))
      node
    else
      EmptyVdom

  def timeRange(props: Props): CustomWindow = {
    TimeRangeHours(
      props.terminalPageTab.timeRangeStart.getOrElse(props.defaultTimeRangeHours.start),
      props.terminalPageTab.timeRangeEnd.getOrElse(props.defaultTimeRangeHours.end)
    )
  }

  val component: Component[Props, State, Backend, CtorType.Props] = ScalaComponent.builder[Props]("TerminalContentComponent")
    .initialStateFromProps(p => State(p.terminalPageTab.subMode))
    .renderBackend[TerminalContentComponent.Backend]
    .componentDidMount(p =>
      Callback {
        val page = s"${p.props.terminalPageTab.terminal}/${p.props.terminalPageTab.mode}/${p.props.terminalPageTab.subMode}"
        val pageWithTime = s"$page/${timeRange(p.props).start}/${timeRange(p.props).end}"
        val pageWithDate = p.props.terminalPageTab.date.map(s => s"$page/${p.props.terminalPageTab.parseDateString(s)}/${timeRange(p.props).start}/${timeRange(p.props).end}").getOrElse(pageWithTime)
        GoogleEventTracker.sendPageView(pageWithDate)
        log.info("terminal component didMount")
      }
    )
    .build

  def apply(props: Props): VdomElement = component(props)
}
