package drt.client.components

import drt.client.components.ChartJSComponent.{ChartJsData, ChartJsOptions, ChartJsProps}
import drt.client.logger.{Logger, LoggerFactory}
import drt.shared.api.PassengerInfoSummary
import drt.shared.{ApiFlightWithSplits, Nationality, PaxTypes}
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js.special.debugger

object FlightChartComponent {

  def summariseNationlaties(nats: Map[Nationality, Int], numberToShow: Int): Map[Nationality, Int] =
    nats
      .toList
      .sortBy {
        case (_, total) => total
      }
      .reverse
      .splitAt(numberToShow) match {
      case (relevant, other) if other.nonEmpty =>
        relevant.toMap + (Nationality("Other") -> other.map(_._2).sum)
      case (all, _) => all.toMap
    }

  val log: Logger = LoggerFactory.getLogger(getClass.getName)

  case class Props(
                    flightWithSplits: ApiFlightWithSplits,
                    passengerInfo: PassengerInfoSummary
                  )

  val component = ScalaComponent.builder[Props]("FlightChart")
    .render_P(p => {
      val sortedNats = summariseNationlaties(p.passengerInfo.nationalities, 10)
        .toList
        .sortBy {
          case (_, pax) => pax
        }

      val nationalityData = ChartJsData(sortedNats.map(_._1.code), sortedNats.map(_._2.toDouble), "Live API")

      val sortedAges = p.passengerInfo.ageRanges.toList.sortBy(_._1.title)
      val ageData: ChartJsData = ChartJsData(sortedAges.map(_._1.title), sortedAges.map(_._2.toDouble), "Live API")

      val sortedPaxTypes = p.passengerInfo.paxTypes.toList.sortBy(_._1.cleanName)
      println(sortedPaxTypes)
      debugger()
      val paxTypeData: ChartJsData = ChartJsData(sortedPaxTypes.map {
        case (pt, _) => PaxTypes.displayNameShort(pt)
      }, sortedAges.map(_._2.toDouble), "Live API")

      TippyJSComponent(
        <.div(^.cls := "container arrivals__table__flight__chart-box",
          <.div(^.cls := "row",
            if (sortedNats.toMap.values.sum > 0)
              <.div(^.cls := "col-sm arrivals__table__flight__chart-box__chart",
                ChartJSComponent.Bar(
                  ChartJsProps(
                    data = nationalityData,
                    300,
                    300,
                    options = ChartJsOptions("Nationality breakdown")
                  )
                ))
            else
              EmptyVdom,
            if (sortedPaxTypes.toMap.values.sum > 0 && sortedAges.toMap.values.sum > 0)
              <.div(^.cls := "col-sm arrivals__table__flight__chart-box__chart",
                ChartJSComponent.Bar(
                  ChartJsProps(
                    data = paxTypeData,
                    300,
                    300,
                    options = ChartJsOptions("Passenger types")
                  )))
            else
              EmptyVdom,
            if (sortedAges.toMap.values.sum > 0)
              <.div(^.cls := "col-sm arrivals__table__flight__chart-box__chart",
                ChartJSComponent.Bar(
                  ChartJsProps(
                    data = ageData,
                    300,
                    300,
                    options = ChartJsOptions("Age breakdown")
                  ))
              )
            else
              EmptyVdom
          )
        ).rawElement, interactive = true, <.span(Icon.infoCircle))
    })
    .build

  def apply(props: Props): VdomElement = component(props)
}
