package drt.client.components

import drt.client.SPAMain.{Loc, TerminalsDashboardLoc}
import drt.client.modules.GoogleEventTracker
import drt.client.services.SPACircuit
import drt.shared.{BorderForceStaff, PortOperatorStaff}
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}

object UserDashboardPage {

  case class Props(router: RouterCtl[Loc])

  val component = ScalaComponent.builder[Props]("UserDashboard")
    .render_P(p => {
      val loggedInUserRCP = SPACircuit.connect(_.loggedInUserPot)
      loggedInUserRCP(loggedInUserMP => {
        <.div(loggedInUserMP().renderReady(user => {
          if (user.hasRole(PortOperatorStaff))
            TerminalsExportDashboardPage(user)
          else if (user.hasRole(BorderForceStaff))
            TerminalsDashboardPage(p.router, TerminalsDashboardLoc(None))
          else
            <.div("You have successfully logged into DRT but your account has not been configured correctly. Please contact us for assistance.")
        }))
      })
    })
    .componentDidMount(p => Callback {
      GoogleEventTracker.sendPageView(s"dashboard")
    })
    .build

  def apply(router: RouterCtl[Loc]): VdomElement = component(Props(router))
}