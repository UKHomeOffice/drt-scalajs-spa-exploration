package drt.client.components

import diode.data.{Pending, Pot}
import diode.react.{ModelProxy, ReactConnectProxy}
import drt.auth.{ArrivalSource, LoggedInUser}
import drt.client.SPAMain
import drt.client.SPAMain.{Loc, TerminalPageTabLoc}
import drt.client.components.FlightComponents.SplitsGraph.splitsGraphComponentColoured
import drt.client.logger.log
import drt.client.modules.GoogleEventTracker
import drt.client.services.JSDateConversions.SDate
import drt.client.services.{ExportArrivals, ExportDesks, SPACircuit, ViewMode}
import drt.shared.Queues.Queue
import drt.shared._
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.{<, VdomAttr, VdomElement, ^, vdomElementFromComponent, vdomElementFromTag, _}
import japgolly.scalajs.react.{Callback, CtorType, ScalaComponent}
import org.scalajs.dom.html.Div

import scala.util.Try

object TerminalContentComponent {

  case class Props(portStatePot: Pot[PortState],
                   potShifts: Pot[ShiftAssignments],
                   potFixedPoints: Pot[FixedPointAssignments],
                   potStaffMovements: Pot[Seq[StaffMovement]],
                   airportConfig: AirportConfig,
                   terminalPageTab: TerminalPageTabLoc,
                   defaultTimeRangeHours: TimeRangeHours,
                   router: RouterCtl[Loc],
                   showActuals: Boolean,
                   viewMode: ViewMode,
                   loggedInUser: LoggedInUser,
                   minuteTicker: Int,
                   featureFlags: Pot[Map[String, Boolean]],
                   arrivalSources: Option[(UniqueArrival, Pot[List[Option[FeedSourceArrival]]])],
                  )

  case class State(activeTab: String, showExportDialogue: Boolean = false)

  def viewStartAndEnd(day: SDateLike, range: TimeRangeHours): (SDateLike, SDateLike) = {
    val startOfDay = SDate(day.getFullYear(), day.getMonth(), day.getDate())
    val startOfView = startOfDay.addHours(range.start)
    val endOfView = startOfDay.addHours(range.end)
    (startOfView, endOfView)
  }

  def airportWrapper(portCode: PortCode): ReactConnectProxy[Pot[AirportInfo]] = SPACircuit.connect(_.airportInfos.getOrElse(portCode, Pending()))

  def originMapper(portCode: PortCode): VdomElement = {
    Try {
      vdomElementFromComponent(airportWrapper(portCode) { proxy: ModelProxy[Pot[AirportInfo]] =>
        <.span(
          proxy().render(ai => <.span(^.title := s"${ai.airportName}, ${ai.city}, ${ai.country}", portCode.toString)),
          proxy().renderEmpty(<.span(portCode.toString))
        )
      })
    }.recover {
      case e =>
        log.error(s"origin mapper error $e")
        vdomElementFromTag(<.div(portCode.toString))
    }.get
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
                  <.a(^.id := "staffMovementsTab", VdomAttr("data-toggle") := "tab", "Staff Movements"), ^.onClick --> {
                    GoogleEventTracker.sendEvent(terminalName, "Staff Movements", props.terminalPageTab.dateFromUrlOrNow.toISODateOnly)
                    props.router.set(props.terminalPageTab.copy(subMode = "staffing"))
                  })
              ),
              <.div(^.className := "exports",
                <.a("Export Arrivals",
                  ^.className := "btn btn-default",
                  ^.href := SPAMain.exportUrl(ExportArrivals, props.terminalPageTab.viewMode, terminal),
                  ^.target := "_blank",
                  ^.id := "export-day-arrivals",
                  ^.onClick --> {
                  Callback(GoogleEventTracker.sendEvent(terminalName, "Export Arrivals", props.terminalPageTab.dateFromUrlOrNow.toISODateOnly))
                  }),
                <.a(
                  "Export Desks",
                  ^.className := "btn btn-default",
                  ^.href := SPAMain.exportUrl(ExportDesks, props.terminalPageTab.viewMode, terminal),
                  ^.target := "_blank",
                  ^.id := "export-day-desks",
                  ^.onClick --> {
                    Callback(GoogleEventTracker.sendEvent(terminalName, "Export Desks", props.terminalPageTab.dateFromUrlOrNow.toISODateOnly))
                  }),
                MultiDayExportComponent(terminal, props.terminalPageTab.dateFromUrlOrNow, props.loggedInUser)
              )
            ),
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
                        showWaitTime = features.get("enable-toggle-display-wait-times") match {
                          case Some(true) => false
                          case _ => true
                        },
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
                  arrivalsTableComponent(
                    FlightsWithSplitsTable.Props(
                      flightsForTerminal,
                      queueOrder,
                      props.airportConfig.hasEstChox,
                      props.arrivalSources,
                      props.loggedInUser.hasRole(ArrivalSource),
                      props.viewMode,
                      PcpPax.bestPaxEstimateWithApi,
                      props.airportConfig.hasTransfer
                    )
                  )
                } else ""
              }),
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
        }))
    }
  }

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
