package drt.client.components


import diode.data.Pot
import drt.client.SPAMain.{Loc, TerminalPageTabLoc}
import drt.client.components.FlightComponents.SplitsGraph.splitsGraphComponentColoured
import drt.client.components.TerminalContentComponent.originMapper
import drt.client.modules.GoogleEventTracker
import drt.client.services.JSDateConversions.SDate
import drt.client.services.ViewLive
import drt.shared.CrunchApi.CrunchMinute
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.{PassengerInfoSummary, WalkTimes}
import drt.shared.dates.UtcDate
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CtorType, ReactEventFromInput, ScalaComponent}
import uk.gov.homeoffice.drt.auth.LoggedInUser

import scala.collection.immutable.Map
import scala.scalajs.js.URIUtils
import scala.util.Try


object TerminalDashboardComponent {

  case class Props(
                    terminalPageTabLoc: TerminalPageTabLoc,
                    airportConfig: AirportConfig,
                    router: RouterCtl[Loc],
                    portState: PortState,
                    passengerInfoSummary: Map[UtcDate, Map[ArrivalKey, PassengerInfoSummary]],
                    featureFlags: Pot[FeatureFlags],
                    walkTimes: Pot[WalkTimes],
                    loggedInUser: LoggedInUser
                  )

  val defaultSlotSize = 120

  val component: Component[Props, Unit, Unit, CtorType.Props] = ScalaComponent.builder[Props](displayName = "TerminalDashboard")
    .render_P(p => {
      val slotSize = Try {
        p.terminalPageTabLoc.subMode.toInt
      }.getOrElse(defaultSlotSize)

      def timeSlotStart: SDateLike => SDateLike = timeSlotForTime(slotSize)

      val startPoint = p.terminalPageTabLoc.queryParams.get("start")
        .flatMap(s => SDate.parse(s))
        .getOrElse(SDate.now())
      val start = timeSlotStart(startPoint)
      val end = start.addMinutes(slotSize)
      val ps = p.portState.window(start, end)
      val prevSlotStart = start.addMinutes(-slotSize)

      val prevSlotPortState = p.portState.window(prevSlotStart, start)

      val urlPrevTime = URIUtils.encodeURI(prevSlotStart.toISOString())
      val urlNextTime = URIUtils.encodeURI(end.toISOString())

      val terminalPax = ps.crunchMinutes.collect {
        case (_, cm) if cm.terminal == p.terminalPageTabLoc.terminal => cm.paxLoad
      }.sum.round

      val terminal = p.terminalPageTabLoc.terminal

      <.div(^.className := "terminal-dashboard",

        if (p.terminalPageTabLoc.queryParams.contains("showArrivals")) {
          val closeArrivalsPopupLink = p.terminalPageTabLoc.copy(
            queryParams = p.terminalPageTabLoc.queryParams - "showArrivals"
          )
          <.div(<.div(^.className := "popover-overlay",
            ^.onClick --> p.router.set(closeArrivalsPopupLink)),

            <.div(^.className := "dashboard-arrivals-popup",
              <.h2("Arrivals"),
              <.div(^.className := "terminal-dashboard__arrivals_popup_table",
                p.featureFlags.renderReady(featureFlags =>
                p.walkTimes.renderReady(walkTimes =>
                  FlightsWithSplitsTable.ArrivalsTable(
                    None,
                    originMapper,
                    splitsGraphComponentColoured)(
                    FlightsWithSplitsTable.Props(
                      ps.flights.filter { case (ua, _) => ua.terminal == p.terminalPageTabLoc.terminal }.values.toList,
                      p.passengerInfoSummary,
                      p.airportConfig.queueTypeSplitOrder(p.terminalPageTabLoc.terminal),
                      p.airportConfig.hasEstChox,
                      None,
                      p.loggedInUser,
                      ViewLive,
                      walkTimes,
                      p.airportConfig.defaultWalkTimeMillis(p.terminalPageTabLoc.terminal),
                      hasTransfer = p.airportConfig.hasTransfer,
                      displayRedListInfo = featureFlags.displayRedListInfo
                    )
                  )))),
              p.router.link(closeArrivalsPopupLink)(^.className := "close-arrivals-popup btn btn-default", "close")
            ))

        } else <.div(),
        <.div(^.className := "terminal-dashboard-queues",
          <.div(^.className := "pax-bar row", s"$terminalPax passengers presenting at the PCP"),

          <.div(^.className := "row queue-boxes",
            p.airportConfig.nonTransferQueues(terminal).filterNot(_ == Queues.FastTrack).map(q => {
              val qCMs = cmsForTerminalAndQueue(ps, q, terminal)
              val prevSlotCMs = cmsForTerminalAndQueue(prevSlotPortState, q, terminal)
              val qPax = qCMs.map(_.paxLoad).sum.round
              val qWait = maxWaitInPeriod(qCMs)
              val prevSlotQWait = maxWaitInPeriod(prevSlotCMs)

              val waitIcon = (prevSlotQWait, qWait) match {
                case (p, c) if p > c => Icon.arrowDown
                case (p, c) if p < c => Icon.arrowUp
                case _ => Icon.arrowRight
              }

              <.div(^.className := s"queue-box col ${q.toString.toLowerCase} ${TerminalDesksAndQueuesRow.slaRagStatus(qWait, p.airportConfig.slaByQueue(q))}",
                <.div(^.className := "queue-name", s"${Queues.queueDisplayNames.getOrElse(q, q)}"),
                <.div(^.className := "queue-box-text", Icon.users, s"$qPax pax joining"),
                <.div(^.className := "queue-box-text", Icon.clockO, s"${MinuteAsAdjective(qWait).display} wait"),
                <.div(^.className := "queue-box-text", waitIcon, s"queue time"),
              )
            }).toTagMod
          ),

          <.div(^.className := "tb-bar row",
            p.router.link(p.terminalPageTabLoc.copy(queryParams = Map("start" -> s"$urlPrevTime")))(^.className := "dashboard-time-switcher prev-bar col", Icon.angleDoubleLeft),
            <.div(^.className := "time-label col", s"${start.prettyTime()} - ${end.prettyTime()}"),
            p.router.link(p.terminalPageTabLoc.copy(queryParams = Map("start" -> s"$urlNextTime")))(^.className := "dashboard-time-switcher next-bar col", Icon.angleDoubleRight)
          )
        )
        ,
        <.div(^.className := "terminal-dashboard-side",
          p.router
            .link(p.terminalPageTabLoc.copy(
              queryParams = p.terminalPageTabLoc.queryParams + ("showArrivals" -> "true")
            ))(^.className := "terminal-dashboard-side__sidebar_widget", "View Arrivals"),
          <.div(
            ^.className := "terminal-dashboard-side__sidebar_widget time-slot-changer",
            <.label(^.className := "terminal-dashboard-side__sidebar_widget__label", "Time slot duration"),
            <.select(
              ^.onChange ==> ((e: ReactEventFromInput) =>
                p.router.set(p.terminalPageTabLoc.copy(subMode = e.target.value))),
              ^.value := slotSize,
              <.option("15 minutes", ^.value := "15"),
              <.option("30 minutes", ^.value := "30"),
              <.option("1 hour", ^.value := "60"),
              <.option("2 hours", ^.value := "120"),
              <.option("3 hours", ^.value := "180")))
        )
      )
    })
    .componentDidMount(p => Callback {
      GoogleEventTracker.sendPageView(page = s"terminal-dashboard-${p.props.terminalPageTabLoc.terminal}")
    })
    .build


  def cmsForTerminalAndQueue(ps: PortStateLike, queue: Queue, terminal: Terminal): Iterable[CrunchMinute] = ps
    .crunchMinutes
    .collect {
      case (tqm, cm) if tqm.queue == queue && tqm.terminal == terminal => cm
    }

  def maxWaitInPeriod(cru: Iterable[CrunchApi.CrunchMinute]): Int = if (cru.nonEmpty)
    cru.map(cm => cm.deployedWait.getOrElse(cm.waitTime)).max
  else 0

  def apply(terminalPageTabLoc: TerminalPageTabLoc,
            airportConfig: AirportConfig,
            portState: PortState,
            passengerInfoSummaryByDay: Map[UtcDate, Map[ArrivalKey, PassengerInfoSummary]],
            router: RouterCtl[Loc],
            featureFlags: Pot[FeatureFlags],
            potWalktTimes: Pot[WalkTimes],
            loggedInUser: LoggedInUser,
           ): VdomElement =
    component(Props(
      terminalPageTabLoc,
      airportConfig,
      router,
      portState,
      passengerInfoSummaryByDay,
      featureFlags,
      potWalktTimes,
      loggedInUser
    ))

  def timeSlotForTime(slotSize: Int)(sd: SDateLike): SDateLike = {
    val offset: Int = sd.getMinutes() % slotSize

    sd.addMinutes(offset * -1)
  }
}
