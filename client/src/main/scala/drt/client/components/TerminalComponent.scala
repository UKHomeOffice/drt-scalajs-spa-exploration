package drt.client.components

import diode.data.{Pending, Pot}
import drt.client.SPAMain.{Loc, TerminalPageTabLoc}
import drt.client.services._
import drt.shared.CrunchApi.CrunchState
import drt.shared._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._

object TerminalComponent {

  case class Props(terminalPageTab: TerminalPageTabLoc, router: RouterCtl[Loc])

  case class TerminalModel(
                            crunchStatePot: Pot[CrunchState],
                            airportConfig: Pot[AirportConfig],
                            airportInfos: Pot[AirportInfo],
                            viewMode: ViewMode,
                            timeRangeHours: TimeRangeHours
                          )

  def render(props: Props): Unit = {
    val modelRCP = SPACircuit.connect(model => TerminalModel(
      model.crunchStatePot,
      model.airportConfig,
      model.airportInfos.getOrElse(props.terminalPageTab.terminal, Pending()),
      model.viewMode,
      model.timeRangeFilter
    ))
  }

  implicit val pageReuse: Reusability[TerminalPageTabLoc] = Reusability.caseClass[TerminalPageTabLoc]
  implicit val propsReuse: Reusability[Props] = Reusability.caseClass[Props]

  val component = ScalaComponent.builder[Props]("Terminal")
    .renderPS(($, props, state) => {
      val modelRCP = SPACircuit.connect(model => TerminalModel(
        model.crunchStatePot,
        model.airportConfig,
        model.airportInfos.getOrElse(props.terminalPageTab.terminal, Pending()),
        model.viewMode,
        model.timeRangeFilter
      ))
      modelRCP(modelMP => {
        val model = modelMP.value
        <.div(model.airportConfig.renderReady(airportConfig => {
          <.div(
            TerminalDisplayModeComponent(TerminalDisplayModeComponent.Props(
              model.crunchStatePot,
              airportConfig,
              props.terminalPageTab,
              model.airportInfos,
              model.timeRangeHours,
              props.router)
            ))
        }))
      })
    })
    .build

  def apply(props: Props): VdomElement = {
    component(props)
  }
}
