package drt.client.components

import drt.client.logger.{Logger, LoggerFactory}
import drt.shared.Nationality
import drt.shared.api.PassengerInfoSummary
import japgolly.scalajs.react.{CtorType, ScalaComponent}
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.vdom.html_<^._

object NationalityFinderComponent {
  def isRedListCountry(country: String): Boolean = redList.keys.exists(_.toLowerCase == country.toLowerCase)

  val log: Logger = LoggerFactory.getLogger(getClass.getName)
  val component: Component[Props, Unit, Unit, CtorType.Props] = ScalaComponent.builder[Props]("FlightChart")
    .render_P(p => {

      val nats = p.ofInterest.map(n => p.passengerInfo.nationalities.getOrElse(n, 0)).sum
      <.span(
        if (nats > 0)
          NationalityFinderChartComponent(
            NationalityFinderChartComponent.Props(
              p.passengerInfo.nationalities.filter {
                case (nat, _) => p.ofInterest.toList.contains(nat)
              },
              <.span(^.className := "badge", nats)
            )
          )
        else
          <.span(nats),

      )
    })
    .build

  def apply(props: Props): VdomElement = component(props)

  case class Props(
                    ofInterest: Iterable[Nationality],
                    passengerInfo: PassengerInfoSummary
                  )

  val redList = Map(
    "Angola" -> "AGO",
    "Argentina" -> "ARG",
    "Bangladesh" -> "BGD",
    "Bolivia" -> "BOL",
    "Botswana" -> "BWA",
    "Brazil" -> "BRA",
    "Burundi" -> "BDI",
    "Cape Verde" -> "CPV",
    "Chile" -> "CHL",
    "Colombia" -> "COL",
    "Democratic Republic of the Congo" -> "COD",
    "Ecuador" -> "ECU",
    "Eswatini" -> "SWZ",
    "Ethiopia" -> "ETH",
    "French Guiana" -> "GUF",
    "Guyana" -> "GUY",
    "India" -> "IND",
    "Kenya" -> "KEN",
    "Lesotho" -> "LSO",
    "Malawi" -> "MWI",
    "Mozambique" -> "MOZ",
    "Namibia" -> "NAM",
    "Oman" -> "OMN",
    "Pakistan" -> "PAK",
    "Panama" -> "PAN",
    "Paraguay" -> "PRY",
    "Peru" -> "PER",
    "Philippines" -> "PHL",
    "Qatar" -> "QAT",
    "Rwanda" -> "RWA",
    "Seychelles" -> "SYC",
    "Somalia" -> "SOM",
    "South Africa" -> "ZAF",
    "Suriname" -> "SUR",
    "Tanzania" -> "TZA",
    "United Arab Emirates" -> "ARE",
    "Uruguay" -> "URY",
    "Venezuela" -> "VEN",
    "Zambia" -> "ZMB",
    "Zimbabwe" -> "ZWE",
  )

  val redListNats: Iterable[Nationality] = redList.values.map(Nationality(_))

}


