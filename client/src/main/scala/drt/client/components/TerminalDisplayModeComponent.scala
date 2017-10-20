package drt.client.components

import diode.data.Pot
import drt.client.SPAMain.{Loc, TerminalPageTabLoc}
import drt.client.actions.Actions.SetViewMode
import drt.client.services.JSDateConversions.SDate
import drt.client.services._
import drt.shared.CrunchApi.CrunchState
import drt.shared.{AirportConfig, AirportInfo}
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._

object TerminalDisplayModeComponent {

  case class Props(crunchStatePot: Pot[CrunchState],
                   airportConfig: AirportConfig,
                   terminalPageTab: TerminalPageTabLoc,
                   airportInfoPot: Pot[AirportInfo],
                   timeRangeHours: TimeRangeHours,
                   viewMode: ViewMode,
                   router: RouterCtl[Loc]
                  )

  case class State(activeTab: String)

  val component = ScalaComponent.builder[Props]("Terminal")
    .initialStateFromProps(p => State(p.terminalPageTab.mode))
    .renderPS((scope, props, state) => {

      val terminalContentProps = TerminalContentComponent.Props(
        props.crunchStatePot,
        props.airportConfig,
        props.terminalPageTab,
        props.airportInfoPot,
        props.timeRangeHours,
        props.viewMode,
        props.router
      )

      val currentClass = if (state.activeTab == "current") "active" else ""
      val snapshotDataClass = if (state.activeTab == "snapshot") "active" else ""

      <.div(
        <.ul(^.className := "nav nav-tabs",
          <.li(^.className := currentClass, <.a(VdomAttr("data-toggle") := "tab", "Current"), ^.onClick --> {
            SPACircuit.dispatch(SetViewMode(ViewLive()))
            props.router.set(props.terminalPageTab.copy(mode="current", date = None)).runNow()
            scope.modState(_ => State("current"))
          }),
          <.li(^.className := snapshotDataClass,
            <.a(VdomAttr("data-toggle") := "tab", "Snapshot"), ^.onClick --> {
              props.router.set(props.terminalPageTab.copy(mode="snapshot", date = None)).runNow()
              SPACircuit.dispatch(SetViewMode(ViewPointInTime(SDate.now())))
              scope.modState(_ => State("snapshot"))
            }
          )
        ),
        <.div(^.className := "tab-content",
          <.div(^.id := "arrivals", ^.className := "tab-pane fade in active", {
            if (state.activeTab == "current") <.div(
              DatePickerComponent(DatePickerComponent.Props(props.viewMode, props.router, props.terminalPageTab)),
              TerminalContentComponent(terminalContentProps)
            ) else <.div(
              SnapshotSelector(props.router, props.terminalPageTab),
              TerminalContentComponent(terminalContentProps)
            )
          })))
    })
    .build

  def apply(props: Props): VdomElement = {
    component(props)
  }
}


