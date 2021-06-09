package services.graphstages

import drt.shared.{AirportConfig, ApiFlightWithSplits, PortCode}
import drt.shared.Terminals.{T2, T5, Terminal}
import services.AirportToCountry

case class FlightFilter(filters: List[ApiFlightWithSplits => Boolean]) {
  def +(other: FlightFilter): FlightFilter = FlightFilter(filters ++ other.filters)
  def apply(fws: ApiFlightWithSplits): Boolean = filters.forall(_(fws))
}

object FlightFilter {
  def apply(filter: ApiFlightWithSplits => Boolean): FlightFilter = FlightFilter(List(filter))

  case object ValidTerminalFilter {
    def apply(validTerminals: List[Terminal]): FlightFilter = FlightFilter(fws => validTerminals.contains(fws.apiFlight.Terminal))
  }

  val notCancelledFilter: FlightFilter = FlightFilter(fws => !fws.apiFlight.isCancelled)

  val outsideCtaFilter: FlightFilter = FlightFilter(fws => !fws.apiFlight.Origin.isCta)

  val lhrRedListFilter: FlightFilter = FlightFilter { fws =>
    val isGreenOnlyTerminal = List(T2, T5).contains(fws.apiFlight.Terminal)
    val isRedListOrigin = AirportToCountry.isRedListed(fws.apiFlight.Origin)
    val okToProcess = !isRedListOrigin || !isGreenOnlyTerminal
    println(s"okToProcess: $okToProcess")
    okToProcess
  }

  def regular(validTerminals: Iterable[Terminal]): FlightFilter =
    ValidTerminalFilter(validTerminals.toList) + notCancelledFilter + outsideCtaFilter

  def forPortConfig(config: AirportConfig): FlightFilter = config.portCode match {
    case PortCode("LHR") => regular(config.terminals) + lhrRedListFilter
    case _ => regular(config.terminals)
  }
}
