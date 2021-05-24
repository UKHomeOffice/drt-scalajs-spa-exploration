package drt.client.components

import diode.data.Pot
import diode.react.ModelProxy
import drt.client.actions.Actions.{GetArrivalSources, GetArrivalSourcesForPointInTime, RemoveArrivalSources}
import drt.client.components.FlightComponents.SplitsGraph
import drt.client.components.FlightTableRow.SplitsGraphComponentFn
import drt.client.components.ToolTips._
import drt.client.components.styles.ArrivalsPageStylesDefault
import drt.client.services.JSDateConversions.SDate
import drt.client.services._
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.{Arrival, PassengerInfoSummary, WalkTimes}
import drt.shared.dates.UtcDate
import drt.shared.splits.ApiSplitsToSplitRatio
import japgolly.scalajs.react.component.Scala.{Component, Unmounted}
import japgolly.scalajs.react.vdom.html_<^.{<, ^, _}
import japgolly.scalajs.react.vdom.{TagMod, TagOf, html_<^}
import japgolly.scalajs.react.{CtorType, _}
import org.scalajs.dom.html.{Div, Span, TableSection}
import scalacss.ScalaCssReact
import uk.gov.homeoffice.drt.auth.LoggedInUser
import uk.gov.homeoffice.drt.auth.Roles.ArrivalSource

import scala.collection.immutable.Map

object FlightsWithSplitsTable {

  type BestPaxForArrivalF = Arrival => Int

  case class Props(flightsWithSplits: List[ApiFlightWithSplits],
                   passengerInfoSummaryByDay: Map[UtcDate, Map[ArrivalKey, PassengerInfoSummary]],
                   queueOrder: Seq[Queue], hasEstChox: Boolean,
                   arrivalSources: Option[(UniqueArrival, Pot[List[Option[FeedSourceArrival]]])],
                   loggedInUser: LoggedInUser,
                   viewMode: ViewMode,
                   walkTimes: WalkTimes,
                   defaultWalkTime: Long,
                   hasTransfer: Boolean,
                   displayRedListInfo: Boolean,
                  )

  implicit val propsReuse: Reusability[Props] = Reusability.by((props: Props) => {
    (props.flightsWithSplits, props.arrivalSources, props.passengerInfoSummaryByDay).hashCode()
  })

  def ArrivalsTable(timelineComponent: Option[Arrival => VdomNode] = None,
                    originMapper: PortCode => VdomNode = portCode => portCode.toString,
                    splitsGraphComponent: SplitsGraphComponentFn = (_: SplitsGraph.Props) => <.div()
                   ): Component[Props, Unit, Unit, CtorType.Props] = ScalaComponent.builder[Props](displayName = "ArrivalsTable")
    .render_P(props => {

      val flightsWithSplits = props.flightsWithSplits
      val flightsWithCodeShares: Seq[(ApiFlightWithSplits, Set[Arrival])] = FlightTableComponents.uniqueArrivalsWithCodeShares(flightsWithSplits)
      val sortedFlights = flightsWithCodeShares.sortBy(_._1.apiFlight.PcpTime)
      val isTimeLineSupplied = timelineComponent.isDefined
      val timelineTh = (if (isTimeLineSupplied) <.th("Timeline") :: Nil else List[TagMod]()).toTagMod

      if (sortedFlights.nonEmpty) {
        val dataStickyAttr = VdomAttr("data-sticky") := "data-sticky"
        val classesAttr = ^.className := "table table-responsive table-striped table-hover table-sm"
        <.div(
          (props.loggedInUser.hasRole(ArrivalSource), props.arrivalSources) match {
            case (true, Some((_, sourcesPot))) =>
              <.div(^.tabIndex := 0,
                <.div(^.className := "popover-overlay", ^.onClick --> Callback(SPACircuit.dispatch(RemoveArrivalSources))),
                <.div(^.className := "dashboard-arrivals-popup", ArrivalInfo.SourcesTable(ArrivalInfo.Props(sourcesPot)))
              )
            case _ => <.div()
          },

          <.div(^.id := "toStick", ^.className := "container sticky",
            <.table(
              ^.id := "sticky",
              classesAttr,
              tableHead(props, timelineTh, props.queueOrder)))
          ,
          <.table(
            ^.id := "sticky-body",
            dataStickyAttr,
            classesAttr,
            tableHead(props, timelineTh, props.queueOrder),
            <.tbody(
              sortedFlights.zipWithIndex.map {
                case ((flightWithSplits, codeShares), idx) =>
                  val maybePassengerInfo: Option[PassengerInfoSummary] = props
                    .passengerInfoSummaryByDay
                    .get(SDate(flightWithSplits.apiFlight.Scheduled).toUtcDate)
                    .flatMap(_.get(ArrivalKey(flightWithSplits.apiFlight)))

                  FlightTableRow.component(FlightTableRow.Props(
                    flightWithSplits = flightWithSplits,
                    maybePassengerInfoSummary = maybePassengerInfo,
                    codeShares = codeShares,
                    idx = idx,
                    timelineComponent = timelineComponent,
                    originMapper = originMapper,
                    splitsGraphComponent = splitsGraphComponent,
                    splitsQueueOrder = props.queueOrder,
                    hasEstChox = props.hasEstChox,
                    loggedInUser = props.loggedInUser,
                    viewMode = props.viewMode,
                    walkTimes = props.walkTimes,
                    defaultWalkTime = props.defaultWalkTime,
                    hasTransfer = props.hasTransfer,
                    displayRedListInfo = props.displayRedListInfo
                  ))
              }.toTagMod)
          ),
          "* Passengers from CTA arrivals do not contribute to PCP workload"
        )
      }
      else
        <.div("No flights to display")
    })
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_ => StickyTableHeader("[data-sticky]"))
    .build

  def tableHead(props: Props, timelineTh: TagMod, queues: Seq[Queue]): TagOf[TableSection] = {
    val redListPassportHeading = "Red List Passports"
    val columns = List(
      ("Flight", None),
      ("Origin", None),
      ("Country", Option("country")),
      (redListPassportHeading, None),
      ("Gate / Stand", Option("gate-stand")),
      ("Status", Option("status")),
      ("Sch", None),
      ("Est", None),
      ("Act", None),
      ("Est Chox", None),
      ("Act Chox", None),
      ("Est PCP", None),
      ("Est PCP Pax", None))

    val portColumnThs = columns
      .filter {
        case (label, _) => label != "Est Chox" || props.hasEstChox
      }
      .filter {
        case (label, _) => label != redListPassportHeading || props.displayRedListInfo
      }
      .map {
        case (label, None) if label == "Flight" => <.th(
          <.div(^.cls := "arrivals__table__flight-code-wrapper", label, " ", wbrFlightColorTooltip)
        )
        case (label, None) => <.th(label)
        case (label, Some(className)) if className == "status" => <.th(label, " ", arrivalStatusTooltip, ^.className := className)
        case (label, Some(className)) if className == "gate-stand" => <.th(label, " ", gateOrStandTh, ^.className := className)
        case (label, Some(className)) if className == "country" => <.th(label, " ", countryTooltip, ^.className := className)
        case (label, Some(className)) => <.th(label, ^.className := className)
      }
      .toTagMod

    val queueDisplayNames = queues.map { q =>
      val queueName: String = Queues.queueDisplayNames(q)
      <.th(queueName, " ", splitsTableTooltip)
    }.toTagMod

    val transferPaxTh = <.th("Transfer Pax")

    <.thead(
      if (props.hasTransfer) {
        <.tr(
          timelineTh,
          portColumnThs,
          queueDisplayNames,
          transferPaxTh
        )
      } else {
        <.tr(
          timelineTh,
          portColumnThs,
          queueDisplayNames
        )
      }
    )
  }

  private def gateOrStandTh = {
    <.span(
      Tippy.info(
        "Select any gate / stand below to see the walk time. If it's not correct, contact us and we'll change it for you."
      )
    )
  }
}

object FlightTableRow {

  import FlightTableComponents._

  type OriginMapperF = PortCode => VdomNode
  type BestPaxForArrivalF = Arrival => Int

  type SplitsGraphComponentFn = SplitsGraph.Props => TagOf[Div]

  case class Props(flightWithSplits: ApiFlightWithSplits,
                   maybePassengerInfoSummary: Option[PassengerInfoSummary],
                   codeShares: Set[Arrival],
                   idx: Int,
                   timelineComponent: Option[Arrival => html_<^.VdomNode],
                   originMapper: OriginMapperF = portCode => portCode.toString,
                   splitsGraphComponent: SplitsGraphComponentFn = (_: SplitsGraph.Props) => <.div(),
                   splitsQueueOrder: Seq[Queue],
                   hasEstChox: Boolean,
                   loggedInUser: LoggedInUser,
                   viewMode: ViewMode,
                   walkTimes: WalkTimes,
                   defaultWalkTime: Long,
                   hasTransfer: Boolean,
                   displayRedListInfo: Boolean,
                  )

  case class RowState(hasChanged: Boolean)

  implicit val propsReuse: Reusability[Props] = Reusability.by(p => (p.flightWithSplits.hashCode, p.idx, p.maybePassengerInfoSummary.hashCode))
  implicit val stateReuse: Reusability[RowState] = Reusability.derive[RowState]

  def bestArrivalTime(f: Arrival): MillisSinceEpoch = {
    val best = (
      Option(SDate(f.Scheduled)),
      f.Estimated.map(SDate(_)),
      f.Actual.map(SDate(_))
    ) match {
      case (Some(sd), None, None) => sd
      case (_, Some(est), None) => est
      case (_, _, Some(act)) => act
      case _ => throw new Exception(s"Flight has no scheduled date: $f")
    }

    best.millisSinceEpoch
  }

  val component: Component[Props, RowState, Unit, CtorType.Props] = ScalaComponent.builder[Props](displayName = "TableRow")
    .initialState[RowState](RowState(false))
    .render_PS((props, state) => {
      val codeShares = props.codeShares
      val flightWithSplits = props.flightWithSplits
      val flight = flightWithSplits.apiFlight
      val allCodes = flight.flightCodeString :: codeShares.map(_.flightCodeString).toList

      val hasChangedStyle = if (state.hasChanged) ^.background := "rgba(255, 200, 200, 0.5) " else ^.outline := ""
      val timeIndicatorClass = if (flight.PcpTime.getOrElse(0L) < SDate.now().millisSinceEpoch) "before-now" else "from-now"

      val queuePax: Map[Queue, Int] = ApiSplitsToSplitRatio
        .paxPerQueueUsingBestSplitsAsRatio(flightWithSplits).getOrElse(Map())

      val flightCodeClass = if (props.loggedInUser.hasRole(ArrivalSource))
        "arrivals__table__flight-code arrivals__table__flight-code--clickable"
      else
        "arrivals__table__flight-code"

      val ctaMarker = if (flight.Origin.isDomesticOrCta) "*" else ""
      val flightCodes = s"${allCodes.mkString(" - ")}$ctaMarker"
      val flightCodeElement = if (props.loggedInUser.hasRole(ArrivalSource))
        <.span(
          ^.cls := "arrivals__table__flight-code-value",
          ^.onClick --> Callback(SPACircuit.dispatch {
            props.viewMode match {
              case vm: ViewDay if vm.isHistoric(SDate.now()) =>
                GetArrivalSourcesForPointInTime(props.viewMode.time.addHours(28), props.flightWithSplits.unique)
              case _: ViewPointInTime =>
                GetArrivalSourcesForPointInTime(props.viewMode.time, props.flightWithSplits.unique)
              case _ =>
                GetArrivalSources(props.flightWithSplits.unique)
            }
          }),
          flightCodes)
      else
        <.span(
          ^.cls := "arrivals__table__flight-code-value",
          flightCodes
        )

      val firstCells = List[TagMod](
        <.td(
          ^.className := flightCodeClass,
          <.div(
            ^.cls := "arrivals__table__flight-code-wrapper",
            flightCodeElement,
            if (flightWithSplits.hasValidApi)
              props
                .maybePassengerInfoSummary
                .map(info => FlightChartComponent(FlightChartComponent.Props(flightWithSplits, info)))
            else EmptyVdom
          )
        ),
        <.td(props.originMapper(flight.Origin)),
        <.td(TerminalContentComponent.airportWrapper(flight.Origin) { proxy: ModelProxy[Pot[AirportInfo]] =>
          <.span(
            proxy().renderEmpty(<.span()),
            proxy().render(ai => {
              val style = if (props.displayRedListInfo && NationalityFinderComponent.isRedListCountry(ai.country)) {
                ScalaCssReact.scalacssStyleaToTagMod(
                ArrivalsPageStylesDefault.redListCountryField)
              } else EmptyVdom

              <.span(
                style,
                ai.country
              )
            })
          )
        }),
        if (props.displayRedListInfo)
          <.td(props.maybePassengerInfoSummary.map(
            info => NationalityFinderComponent(NationalityFinderComponent.Props(NationalityFinderComponent.redListNats, info))))
        else
          EmptyVdom,
        <.td(gateOrStand(props.walkTimes, props.defaultWalkTime, flight)),
        <.td(flight.displayStatus.description),
        <.td(localDateTimeWithPopup(Option(flight.Scheduled))),
        <.td(localDateTimeWithPopup(flight.Estimated)),
        <.td(localDateTimeWithPopup(flight.Actual))
      )
      val estCell = List(<.td(localDateTimeWithPopup(flight.EstimatedChox)))
      val lastCells = List[TagMod](
        <.td(localDateTimeWithPopup(flight.ActualChox)),
        <.td(pcpTimeRange(flight)),
        <.td(FlightComponents.paxComp(flightWithSplits))
      )
      val flightFields = if (props.hasEstChox) firstCells ++ estCell ++ lastCells else firstCells ++ lastCells

      val paxClass = FlightComponents.paxClassFromSplits(flightWithSplits)

      val flightId = flight.uniqueId.toString

      val timeLineTagMod = props.timelineComponent.map(timeline => <.td(timeline(flight))).toList.toTagMod

      val cancelledClass = if (flight.isCancelled) " arrival-cancelled" else ""
      val ctaClass = if (flight.Origin.isCta) " arrival-cta" else ""
      val trClassName = s"${offScheduleClass(flight)} $timeIndicatorClass$cancelledClass$ctaClass"

      val queueTagMod = props.splitsQueueOrder.map { q =>
        val pax = if (!flight.Origin.isDomesticOrCta) queuePax.getOrElse(q, 0) else "-"
        <.td(<.span(s"$pax"), ^.className := s"queue-split $paxClass ${q.toString.toLowerCase()}-queue-pax right")
      }.toTagMod

      if (props.hasTransfer) {
        <.tr(
          ^.key := flightId,
          ^.className := trClassName,
          hasChangedStyle,
          timeLineTagMod,
          flightFields.toTagMod,
          queueTagMod,
          <.td(FlightComponents.paxTransferComponent(flight))
        )
      } else {
        <.tr(
          ^.key := flightId,
          ^.className := trClassName,
          hasChangedStyle,
          timeLineTagMod,
          flightFields.toTagMod,
          queueTagMod
        )
      }

    })
    .configure(Reusability.shouldComponentUpdate)
    .build

  private def gateOrStand(walkTimes: WalkTimes, defaultWalkTime: Long, flight: Arrival): VdomTagOf[Span] = {
    val walkTimeProvider: (Option[String], Option[String], Terminal) => String =
      walkTimes.walkTimeForArrival(defaultWalkTime)
    val gateOrStand = <.span(s"${flight.Gate.getOrElse("")} / ${flight.Stand.getOrElse("")}")
    val gateOrStandWithWalkTimes = Tippy.interactive(
      <.span(walkTimeProvider(flight.Gate, flight.Stand, flight.Terminal)),
      gateOrStand
    )
    val displayGatesOrStands = if (walkTimes.isEmpty) gateOrStand else <.span(gateOrStandWithWalkTimes)
    displayGatesOrStands
  }

  def offScheduleClass(arrival: Arrival): String = {
    val eta = bestArrivalTime(arrival)
    val differenceFromScheduled = eta - arrival.Scheduled
    val hourInMillis = 3600000
    val offScheduleClass = if (differenceFromScheduled > hourInMillis || differenceFromScheduled < -1 * hourInMillis)
      "danger"
    else ""
    offScheduleClass
  }

  def apply(props: Props): Unmounted[Props, RowState, Unit] = component(props)

}
