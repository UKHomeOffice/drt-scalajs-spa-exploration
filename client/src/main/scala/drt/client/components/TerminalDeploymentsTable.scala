package drt.client.components

import diode.data.Pot
import diode.react._
import drt.client.TableViewUtils._
import drt.client.components.StaffMovementsPopover.StaffMovementPopoverState
import drt.client.logger._
import drt.client.services.HandyStuff.QueueStaffDeployments
import drt.client.services.JSDateConversions.SDate
import drt.client.services.RootModel.QueueCrunchResults
import drt.client.services._
import drt.shared.FlightsApi.{FlightsWithSplits, QueueName, TerminalName}
import drt.shared._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.{Builder, Lifecycle}
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.{TagOf, html_<^}
import org.scalajs.dom.html.{TableHeaderCell, TableRow}

import scala.collection.immutable.{Map, Seq}
import scala.scalajs.js.Date
import scala.util.Try

object TerminalDeploymentsTable {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  sealed trait QueueDeploymentsRow {
    def queueName: QueueName

    def timestamp: Long
  }

  case class QueuePaxRowEntry(timestamp: Long, queueName: QueueName, pax: Double) extends QueueDeploymentsRow

  case class QueueDeploymentsRowEntry(
                                       timestamp: Long,
                                       pax: Double,
                                       crunchDeskRec: Int,
                                       userDeskRec: DeskRecTimeslot,
                                       actualDeskRec: Option[Int] = None,
                                       waitTimeWithCrunchDeskRec: Int,
                                       waitTimeWithUserDeskRec: Int,
                                       actualWaitTime: Option[Int] = None,
                                       queueName: QueueName
                                     ) extends QueueDeploymentsRow

  case class TerminalDeploymentsRow(time: Long, queueDetails: List[QueueDeploymentsRow])

  case class Props(
                    terminalName: String,
                    items: List[TerminalDeploymentsRow],
                    flights: Pot[FlightsWithSplits],
                    airportConfig: AirportConfig,
                    airportInfos: ReactConnectProxy[Map[String, Pot[AirportInfo]]]
                  )

  case class State(showActuals: Boolean = false)

  object jsDateFormat {

    def zeroPadTo2Digits(number: Int) = {
      if (number < 10)
        "0" + number
      else
        number.toString
    }

    def formatDate(date: Date): String = {
      val formattedDate: String = date.getFullYear() + "-" + zeroPadTo2Digits(date.getMonth() + 1) + "-" + zeroPadTo2Digits(date.getDate()) + " " + date.toLocaleTimeString().replaceAll(":00$", "")
      formattedDate
    }
  }

  def deskUnitLabel(queueName: QueueName): String = {
    queueName match {
      case "eGate" => "Banks"
      case _ => "Desks"
    }
  }

  def renderTerminalUserTable(terminalName: TerminalName, airportInfoConnectProxy: ReactConnectProxy[Map[String, Pot[AirportInfo]]],
                              peMP: ModelProxy[PracticallyEverything], rows: List[TerminalDeploymentsRow], airportConfig: AirportConfig): VdomElement = {
    <.div(
      TerminalDeploymentsTable(
        terminalName,
        rows,
        peMP().flights,
        airportConfig,
        airportInfoConnectProxy
      ))
  }

  case class PracticallyEverything(
                                    airportInfos: Map[String, Pot[AirportInfo]],
                                    flights: Pot[FlightsWithSplits],
                                    simulationResult: Map[TerminalName, Map[QueueName, Pot[SimulationResult]]],
                                    workload: Pot[Workloads],
                                    queueCrunchResults: Map[TerminalName, QueueCrunchResults],
                                    userDeskRec: Map[TerminalName, QueueStaffDeployments],
                                    shiftsRaw: Pot[String]
                                  )

  case class TerminalProps(terminalName: String)

  def terminalDeploymentsComponent(props: TerminalProps) = {
    val practicallyEverything = SPACircuit.connect(model => {
      PracticallyEverything(
        model.airportInfos,
        model.flightsWithSplitsPot,
        model.simulationResult,
        model.workloadPot,
        model.queueCrunchResults,
        model.staffDeploymentsByTerminalAndQueue,
        model.shiftsRaw
      )
    })

    val terminalUserDeskRecsRows = SPACircuit.connect(model =>
      model.calculatedDeploymentRows.getOrElse(Map()).get(props.terminalName)
    )
    val airportWrapper = SPACircuit.connect(_.airportInfos)
    val airportConfigPotRCP = SPACircuit.connect(_.airportConfig)

    practicallyEverything(peMP => {
      <.div(
        terminalUserDeskRecsRows((rowsOptMP: ModelProxy[Option[Pot[List[TerminalDeploymentsRow]]]]) => {
          rowsOptMP() match {
            case None => <.div("No rows yet")
            case Some(rowsPot) =>
              log.debug(s"rowLen ${rowsPot.map(_.length)}")
              log.debug(s"rowsAre $rowsPot")
              <.div(
                rowsPot.renderReady(rows =>
                  airportConfigPotRCP(airportConfigPotMP => {
                    <.div(
                      airportConfigPotMP().renderReady(airportConfig =>
                        renderTerminalUserTable(props.terminalName, airportWrapper, peMP, rows, airportConfig))
                    )
                  })))
          }
        }),
        peMP().workload.renderPending(_ => <.div("Waiting for workloads")))
    })
  }

  case class RowProps(item: TerminalDeploymentsRow, index: Int,
                      flights: Pot[FlightsWithSplits],
                      airportConfig: AirportConfig,
                      airportInfos: ReactConnectProxy[Map[String, Pot[AirportInfo]]],
                      showActuals: Boolean)

  val itemRow = ScalaComponent.builder[RowProps]("deploymentRow")
    .render_P((p) => renderRow(p))
    .build

  def renderRow(props: RowProps): TagOf[TableRow] = {
    val item = props.item
    val index = props.index
    //    log.info(s"rendering terminalDeploymentsRow $index")
    val time = item.time
    val windowSize = 60000 * 15
    val flights: Pot[FlightsWithSplits] = props.flights.map(flights =>
      flights.copy(flights = flights.flights.filter(f => time <= f.apiFlight.PcpTime && f.apiFlight.PcpTime <= (time + windowSize))))

    val formattedDate: String = SDate(MilliDate(item.time)).toHoursAndMinutes()
    val airportInfo: ReactConnectProxy[Map[String, Pot[AirportInfo]]] = props.airportInfos
    val airportInfoPopover = FlightsPopover(formattedDate, flights, airportInfo)

    val queueRowCells: Seq[TagMod] = item.queueDetails.collect {
      case (q: QueueDeploymentsRowEntry) => {
        val warningClasses = if (q.waitTimeWithCrunchDeskRec < q.waitTimeWithUserDeskRec) "table-warning" else ""
        val dangerWait = if (q.waitTimeWithUserDeskRec > props.airportConfig.slaByQueue.getOrElse(q.queueName, 0)) "table-danger" else ""

        def qtd(xs: TagMod*): TagMod = <.td((^.className := queueColour(q.queueName)) :: xs.toList: _*)
        def qtdActuals(xs: TagMod*): TagMod = <.td((^.className := queueActualsColour(q.queueName)) :: xs.toList: _*)

        if (props.showActuals) {
          val actDesks: String = q.actualDeskRec.map(act => s"$act").getOrElse("-")
          val actWaits: String = q.actualWaitTime.map(act => s"$act").getOrElse("-")
          Seq(
            qtd(q.pax),
            qtd(^.title := s"Rec: ${q.crunchDeskRec}", q.userDeskRec.deskRec),
            qtd(^.cls := dangerWait + " " + warningClasses, q.waitTimeWithUserDeskRec),
            qtdActuals(actDesks),
            qtdActuals(^.cls := dangerWait + " " + warningClasses, actWaits))
        }
        else
          Seq(
            qtd(q.pax),
            qtd(^.title := s"Rec: ${q.crunchDeskRec}", q.userDeskRec.deskRec),
            qtd(^.cls := dangerWait + " " + warningClasses, q.waitTimeWithUserDeskRec))
      }
    }.flatten

    val transferCells: TagMod = item.queueDetails.collect {
      case q: QueuePaxRowEntry => <.td(^.className := queueColour(q.queueName), q.pax)
    }.toTagMod

    val staffRequiredQueues: Seq[QueueDeploymentsRowEntry] = item.queueDetails.collect { case q: QueueDeploymentsRowEntry => q }
    val totalRequired: Int = staffRequiredQueues.map(_.crunchDeskRec).sum
    val totalDeployed: Int = staffRequiredQueues.map(_.userDeskRec.deskRec).sum
    val ragClass = totalRequired.toDouble / totalDeployed match {
      case diff if diff >= 1 => "red"
      case diff if diff >= 0.75 => "amber"
      case _ => ""
    }
    val queueRowCellsWithTotal: List[html_<^.TagMod] = (queueRowCells :+
      <.td(^.className := s"total-deployed $ragClass", totalRequired) :+
      <.td(^.className := s"total-deployed $ragClass", totalDeployed) :+ transferCells
      ).toList
    <.tr(<.td(^.cls := "date-field", airportInfoPopover()) :: queueRowCellsWithTotal: _*)
  }

  def queueColour(queueName: String): String = queueName + "-user-desk-rec"
  def queueActualsColour(queueName: String): String = s"${queueColour(queueName)} actuals"

  object Backend {
    def apply(scope: Builder.Step3[Props, State, Unit]#$, props: Props, state: State) = {
      val t = Try {
        def qth(queueName: String, xs: TagMod*) = <.th((^.className := queueName + "-user-desk-rec") :: xs.toList: _*)

        val queueHeadings: List[TagMod] = props.airportConfig.queues(props.terminalName).collect {
          case queueName if queueName != Queues.Transfer =>
            val colsToSpan = if (state.showActuals) 5 else 3
            qth(queueName, queueDisplayName(queueName), ^.colSpan := colsToSpan, ^.className := "top-heading")
        }.toList

        val transferHeading: TagMod = props.airportConfig.queues(props.terminalName).collect {
          case (queueName@Queues.Transfer) => qth(queueName, queueDisplayName(queueName), ^.colSpan := 1)
        }.toList.toTagMod

        val headings: List[TagMod] = queueHeadings :+ <.th(^.className := "total-deployed", ^.colSpan := 2, "PCP") :+ transferHeading

        val defaultNumberOfQueues = 3
        val headOption = props.items.headOption
        val numQueues = headOption match {
          case Some(item) => item.queueDetails.length
          case None => defaultNumberOfQueues
        }
        val showActsClassSuffix = if (state.showActuals) "-with-actuals" else ""
        val colsClass = s"cols-$numQueues$showActsClassSuffix"

        val function = (e: ReactEventFromInput) => {
          val newValue: Boolean = e.target.checked
          scope.modState(_.copy(showActuals = newValue))
        }
        <.div(
          <.input.checkbox(^.value := "yeah", "Show acts", ^.onChange ==> function),
          <.table(^.cls := s"table table-striped table-hover table-sm user-desk-recs $colsClass",
            <.thead(
              ^.display := "block",
              <.tr(<.th("") :: headings: _*),
              <.tr(<.th("Time", ^.className := "time") :: subHeadingLevel2(props.airportConfig.queues(props.terminalName).toList, state): _*)),
            <.tbody(
              ^.display := "block",
              ^.overflow := "scroll",
              ^.height := "500px",
              props.items.zipWithIndex.map {
                case (item, index) => renderRow(RowProps(item, index, props.flights, props.airportConfig, props.airportInfos, state.showActuals))
              }.toTagMod)))
      } recover {
        case t =>
          log.error(s"Failed to render deployment table $t", t.asInstanceOf[Exception])
          <.div(s"Can't render bcause $t")
      }

      t.get
    }


    val headerGroupStart = ^.borderLeft := "solid 1px #fff"

    private def subHeadingLevel2(queueNames: List[QueueName], state: State): List[TagMod] = {
      val queueSubHeadings: List[TagMod] = queueNames.collect {
        case queueName if queueName != Queues.Transfer => <.th(^.className := queueColour(queueName), "Pax") :: staffDeploymentSubheadings(queueName, state)
      }.flatten

      val transferSubHeadings: TagMod = queueNames.collect {
        case Queues.Transfer => <.th(^.className := queueColour(Queues.Transfer), "Pax") :: Nil
      }.flatten.toTagMod

      val list: List[TagMod] = queueSubHeadings :+
        <.th(^.className := "total-deployed", "Rec", ^.title := "Total staff recommended for desks") :+
        <.th(^.className := "total-deployed", "Dep", ^.title := "Total staff deployed based on assignments entered") :+
        transferSubHeadings

      list
    }

    private def thHeaderGroupStart(title: String, xs: TagMod*): VdomTagOf[TableHeaderCell] = {
      <.th(headerGroupStart, title, xs.toTagMod)
    }
  }

  private def staffDeploymentSubheadings(queueName: QueueName, state: State) = {
    val queueColumnClass = queueColour(queueName)
    val queueColumnActualsClass = queueActualsColour(queueName)
    val depls: List[VdomTagOf[TableHeaderCell]] = if (state.showActuals)
      List(
        <.th(^.title := "Suggested deployment given available staff", s"Dep ${deskUnitLabel(queueName)}", ^.className := queueColumnClass),
        <.th(^.title := "Suggested deployment given available staff", "Est wait", ^.className := queueColumnClass),
        <.th(^.title := "Suggested deployment given available staff", s"Act ${deskUnitLabel(queueName)}", ^.className := queueColumnActualsClass),
        <.th(^.title := "Suggested deployment given available staff", "Act wait", ^.className := queueColumnActualsClass))
    else
      List(
        <.th(^.title := "Suggested deployment given available staff", s"Rec ${deskUnitLabel(queueName)}", ^.className := queueColumnClass),
        <.th(^.title := "Suggested deployment given available staff", "Est wait", ^.className := queueColumnClass))
    depls
  }

  def initState() = false


  implicit val deskRecTimeslotReuse = Reusability.caseClass[DeskRecTimeslot]
  implicit val doubleReuse = Reusability.double(1)
  implicit val queueRowEntryReuse = Reusability.caseClass[QueueDeploymentsRowEntry]
  implicit val queuePaxRowEntryReuse = Reusability.caseClass[QueuePaxRowEntry]
  implicit val queueRowEntryBaseReuse = Reusability[QueueDeploymentsRow] {
    case (a: QueuePaxRowEntry, b: QueuePaxRowEntry) => queuePaxRowEntryReuse.test(a, b)
    case (a: QueueDeploymentsRowEntry, b: QueueDeploymentsRowEntry) => queueRowEntryReuse.test(a, b)
    case _ => false
  }
  implicit val terminalReuse = Reusability.caseClass[TerminalDeploymentsRow]
  implicit val propsReuse = Reusability.caseClassExcept[Props]('terminalName, 'flights, 'airportConfig, 'airportInfos)
  implicit val stateReuse = Reusability.caseClassExcept[State]('showActuals)

  private val component = ScalaComponent.builder[Props]("TerminalDeployments")
    .initialState[State](State(false))
    .renderPS((sc, p, s) => Backend(sc, p, s))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(terminalName: String, rows: List[TerminalDeploymentsRow], flights: Pot[FlightsWithSplits],
            airportConfig: AirportConfig,
            airportInfos: ReactConnectProxy[Map[String, Pot[AirportInfo]]]) =
    component(Props(terminalName, rows, flights, airportConfig, airportInfos))

}
