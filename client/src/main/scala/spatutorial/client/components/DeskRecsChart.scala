package spatutorial.client.components

import diode.react.ReactConnectProxy

import scala.collection.immutable._
import diode.data.Pot

//import diode.react.ReactPot._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.DomCallbackResult._
import japgolly.scalajs.react.vdom.prefix_<^._
import spatutorial.client.components.Bootstrap.Panel
import spatutorial.client.components.Bootstrap.Panel.Props
import spatutorial.client.logger._
import spatutorial.client.modules.Dashboard.DashboardModels
import spatutorial.client.services._
import spatutorial.shared._
import spatutorial.shared.FlightsApi.{QueueName, TerminalName}
import spatutorial.client.modules.Dashboard._
import spatutorial.shared.EdiAirportConfig

object DeskRecsChart {
  type DeskRecsModel = DashboardModels

  log.info("initialising deskrecschart")

  case class State(crunchResultWrapper: ReactConnectProxy[Map[TerminalName, QueueCrunchResults]],
                   deskRecs: ReactConnectProxy[Map[TerminalName, QueueUserDeskRecs]])

  case class Props(
                    deskRecsModelMP: ModelProxy[DeskRecsModel],
                    airportConfigPot: Pot[AirportConfig])

  val DeskRecs = ReactComponentB[Props]("CrunchResults")
    .initialState_P((props: Props) => State(props.deskRecsModelMP.connect(_.queueCrunchResults), props.deskRecsModelMP.connect(_.potUserDeskRecs)))
    .renderPS((_, props: Props, state) => {
      val workloads = props.deskRecsModelMP().workloads
      <.div(
        props.deskRecsModelMP().queueCrunchResults.map {
          case (terminalName, terminalQueueCrunchResults) =>
            terminalQueueCrunchResults.map {
              case (queueName, queueCrunchResults) =>
                log.info(s"rendering ${terminalName}, ${queueName}")
                <.div(
                  queueCrunchResults.renderPending(t => s"Waiting for crunchResult for ${queueName}"),
                  queueCrunchResults.renderReady(queueWorkload => {
                    log.info("We think crunch results are ready!!!!")
                    val potCrunchResult: Pot[CrunchResult] = queueWorkload._1
                    <.div(^.key := queueName,
                      workloads.renderReady(wl => {
                        props.airportConfigPot.renderReady(airportConfig => {
                          val labels = wl.labels
                          Panel(Panel.Props(s"Desk Recommendations and Wait times for '$terminalName' '${queueName}'"),
                            potCrunchResult.renderPending(time => <.p(s"Waiting for crunch result ${time}")),
                            potCrunchResult.renderEmpty(<.p("Waiting for crunch result")),
                            potCrunchResult.renderFailed((t) => <.p("Error retrieving crunch result")),
                            deskRecsChart(queueName, labels, potCrunchResult),
                            waitTimesChart(labels, potCrunchResult, airportConfig.slaByQueue(queueName)))
                        })
                      })
                    )
                  }))
            }
        })
    }

    )
    .componentDidMount(scope =>
      Callback.log("Mounted DeskRecs")
    ).build


  def waitTimesChart(labels: IndexedSeq[String], potCrunchResult: Pot[CrunchResult], sla: Int): ReactNode = {
    potCrunchResult.render(chartData => {
      val sampledWaitTimesSimulation: List[Double] = sampledWaitTimes(chartData.waitTimes)
      val fakeSLAData = sampledWaitTimesSimulation.map(_ => sla.toDouble)
      val sampledLabels = takeEvery15th(labels)
      Chart(
        Chart.ChartProps("Wait Times",
          Chart.LineChart,
          ChartData(
            sampledLabels,
            Seq(
              ChartDataset(sampledWaitTimesSimulation, "Wait Times"),
              ChartDataset(fakeSLAData, label = "SLA", backgroundColor = "#fff", borderColor = "red")))))
    })
  }

  case class UserSimulationProps(simulationResult: ModelProxy[Pot[SimulationResult]],
                                 crunchResult: ModelProxy[Pot[CrunchResult]])

  def userSimulationWaitTimesChart(
                                    terminalName: TerminalName,
                                    queueName: QueueName,
                                    airportConfig: AirportConfig,
                                    labels: IndexedSeq[String],
                                    simulationResultPotMP: ModelProxy[Pot[SimulationResult]],
                                    crunchResultPotMP: ModelProxy[Pot[CrunchResult]]) = {
    val component = ReactComponentB[UserSimulationProps]("UserSimulationChart").render_P(props => {
      val sampledWaitTimesSimulation: List[Double] = sampledWaitTimes(props.simulationResult().isReady match {
        case true => props.simulationResult().get.waitTimes
        case _ => props.crunchResult().get.waitTimes
      })
      val sampledWaitTimesCrunch: List[Double] = sampledWaitTimes(props.crunchResult().get.waitTimes)
      val fakeSLAData = sampledWaitTimesSimulation.map(_ => airportConfig.slaByQueue(queueName).toDouble)
      val sampledLabels = takeEvery15th(labels)
      <.div(
        Chart(
          Chart.ChartProps("Simulated Wait Times",
            Chart.LineChart,
            ChartData(sampledLabels,
              Seq(
                ChartDataset(sampledWaitTimesCrunch, "Wait Times with Recommended Desks", backgroundColor = "rgba(10, 10, 55, 0)",
                  borderColor = "rgba(10,10, 110, 1)"),
                ChartDataset(sampledWaitTimesSimulation, "Wait Times with your desks", backgroundColor = "rgba(10, 10, 55, 0)", borderColor = "rgb(143, 35, 179)"),
                ChartDataset(fakeSLAData, label = "SLA", backgroundColor = "rgba(10, 10, 55, 0)", borderColor = "red"))
            ))))
    }).build

    component(UserSimulationProps(simulationResultPotMP, crunchResultPotMP))
  }


  def sampledWaitTimes(times: Seq[Int]): List[Double] = {
    val grouped: Iterator[Seq[Int]] = times.grouped(15)
    val maxInEachGroup: Iterator[Int] = grouped.map(_.max)
    val sampledWaitTimes = maxInEachGroup.map(_.toDouble).toList
    sampledWaitTimes
  }

  def deskRecsChart(queueName: QueueName, labels: IndexedSeq[String], potCrunchResult: Pot[CrunchResult]): ReactNode = {
    potCrunchResult.render(chartData =>
      Chart(Chart.ChartProps(s"Desk Recs ${
        queueName
      }",
        Chart.LineChart,
        ChartData(takeEvery15th(labels), Seq(
          ChartDataset(
            takeEvery15th(chartData.recommendedDesks).map(_.toDouble), s"Desk Recommendations ${
              queueName
            }")))
      )))
  }

  def takeEvery15th[N](desks: Seq[N]) = desks.zipWithIndex.collect {
    case (n, i) if (i % 15 == 0) => n
  }

  def apply(deskRecsModelMP: ModelProxy[DeskRecsModel], airportConfigPot: Pot[AirportConfig]) = DeskRecs(Props(deskRecsModelMP, airportConfigPot))
}
