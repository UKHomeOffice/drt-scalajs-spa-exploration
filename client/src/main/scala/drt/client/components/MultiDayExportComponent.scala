package drt.client.components

import drt.client.logger.{Logger, LoggerFactory}
import drt.client.services.JSDateConversions.SDate
import drt.shared.SDateLike
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

object MultiDayExportComponent {
  val today: SDateLike = SDate.now()
  val log: Logger = LoggerFactory.getLogger(getClass.getName)

  case class Props(
                    terminal: String,
                    selectedDate: SDateLike
                  )

  case class State(startDay: Int,
                   startMonth: Int,
                   startYear: Int,
                   endDay: Int,
                   endMonth: Int,
                   endYear: Int,
                   showDialogue: Boolean = false
                  ) {
    def startMillis = SDate(startYear, startMonth, startDay).millisSinceEpoch

    def endMillis = SDate(endYear, endMonth, endDay).millisSinceEpoch
  }

  implicit val stateReuse: Reusability[State] = Reusability.derive[State]
  implicit val propsReuse: Reusability[Props] = Reusability.by(p => (p.terminal, p.selectedDate.millisSinceEpoch))

  val component = ScalaComponent.builder[Props]("SnapshotSelector")
    .initialStateFromProps(p => State(
      startDay = p.selectedDate.getDate(),
      startMonth = p.selectedDate.getMonth(),
      startYear = p.selectedDate.getFullYear(),
      endDay = p.selectedDate.getDate(),
      endMonth = p.selectedDate.getMonth(),
      endYear = p.selectedDate.getFullYear()
    ))
    .renderPS((scope, props, state) => {

      val showClass = if (state.showDialogue) "show" else "fade"

      <.div(
        <.a(
          "Multi Day Export",
          ^.className := "btn btn-default",
          VdomAttr("data-toggle") := "modal",
          VdomAttr("data-target") := "#multi-day-export",
          ^.onClick --> scope.modState(_.copy(showDialogue = true))
        ),
        <.div(^.className := "multi-day-export modal " + showClass, ^.id := "#multi-day-export", ^.tabIndex := -1, ^.role := "dialog",
          <.div(
            ^.className := "modal-dialog modal-dialog-centered",
            ^.role := "document",
            <.div(
              ^.className := "modal-content",
              <.div(
                ^.className := "modal-header",
                <.h5(^.className := "modal-title", "Choose what to export")
              ),
              <.div(
                ^.className := "modal-body",
                DateSelector("From", today, d => {
                  scope.modState(_.copy(startDay = d.getDate(), startMonth = d.getMonth(), startYear = d.getFullYear()))
                }),
                DateSelector("To", today, d => {
                  scope.modState(_.copy(endDay = d.getDate(), endMonth = d.getMonth(), endYear = d.getFullYear()))
                }),
                <.div(
                  <.div(^.className := "multi-day-export-links",
                    <.a("Export Arrivals",
                      ^.className := "btn btn-default",
                      ^.href := s"${dom.window.location.pathname}/export/arrivals/${state.startMillis}/${state.endMillis}/${props.terminal}",
                      ^.target := "_blank"),
                    <.a("Export Desks",
                      ^.className := "btn btn-default",
                      ^.href := s"${dom.window.location.pathname}/export/desks/${state.startMillis}/${state.endMillis}/${props.terminal}",
                      ^.target := "_blank")
                  )
                )
              ),
              <.div(
                ^.className := "modal-footer",
                <.button(
                  ^.className := "btn btn-link",
                  VdomAttr("data-dismiss") := "modal", "Close",
                  ^.onClick --> scope.modState(_.copy(showDialogue = false))
                )
              )
            )
          )
        ))
    })
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(terminal: String, selectedDate: SDateLike): VdomElement = component(Props(terminal, selectedDate))
}
