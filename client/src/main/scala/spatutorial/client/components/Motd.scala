package spatutorial.client.components

import diode.data.Pot
import diode.react.ReactPot._
import diode.react._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import spatutorial.client.components.Bootstrap._
import spatutorial.client.logger._
import spatutorial.client.services.{UpdateCrunch, UpdateMotd}
import spatutorial.shared.CrunchResult

/**
  * This is a simple component demonstrating how to display async data coming from the server
  */
object Motd {

  // create the React component for holding the Message of the Day
  val Motd = ReactComponentB[ModelProxy[Pot[String]]]("Motd")
    .render_P { (proxy: ModelProxy[Pot[String]]) =>
      val proxy1: Pot[String] = proxy()
      Panel(Panel.Props("Message of the day"),
        // render messages depending on the state of the Pot
        proxy1.renderPending(_ > 500, _ => <.p("Loading...")),
        proxy().renderFailed(ex => <.p("Failed to load")),
        proxy().render(m => <.p(m)),
        Button(Button.Props(proxy.dispatch(UpdateMotd()), CommonStyle.danger), Icon.refresh, "Update")
      )
    }
    .componentDidMount(scope =>
      // update only if Motd is empty
      Callback.when(scope.props.value.isEmpty)({
        println("Updating because it's empty")
        scope.props.dispatch(UpdateMotd())
      })
    )
    .build

  def apply(proxy: ModelProxy[Pot[String]]) = Motd(proxy)
}

