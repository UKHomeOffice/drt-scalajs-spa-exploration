package drt.client.components

import drt.client.logger.{Logger, LoggerFactory}
import drt.shared.{ApiIndirectRedListPax, RedList}
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CtorType, ScalaComponent}

object NationalityFinderComponent {
  def isRedListCountry(country: String): Boolean = RedList.countryToCode.keys.exists(_.toLowerCase == country.toLowerCase)

  val log: Logger = LoggerFactory.getLogger(getClass.getName)
  val component: Component[ApiIndirectRedListPax, Unit, Unit, CtorType.Props] = ScalaComponent.builder[ApiIndirectRedListPax]("FlightChart")
    .render_P { props =>
      <.span(
        props.maybeNationalities match {
          case Some(nats) if nats.values.sum > 0 =>
            NationalityFinderChartComponent(
              NationalityFinderChartComponent.Props(nats, <.span(^.className := "badge", nats.values.sum))
            )
          case Some(_) => 0
          case None => EmptyVdom
        }
      )
    }
    .build

  def apply(apiIndirectRedListPax: ApiIndirectRedListPax): VdomElement = component(apiIndirectRedListPax)

}


