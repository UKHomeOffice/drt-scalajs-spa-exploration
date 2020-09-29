package controllers.application

import actors.GetState
import actors.PartitionedPortStateActor.GetFlightsForTerminalDateRange
import actors.queues.FlightsRouterActor
import akka.NotUsed
import akka.actor.Props
import akka.pattern.ask
import akka.stream.UniqueKillSwitch
import akka.stream.scaladsl.{Source, SourceQueueWithComplete}
import akka.util.Timeout
import controllers.Application
import controllers.application.exports.CsvFileStreaming
import drt.auth.ArrivalSimulationUpload
import drt.shared.CrunchApi.{CrunchMinute, DeskRecMinutes, MillisSinceEpoch}
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared._
import play.api.mvc._
import services.crunch.desklimits.PortDeskLimits
import services.crunch.deskrecs.{DesksAndWaitsPortProvider, RunnableDeskRecs}
import services.exports.Exports
import services.exports.summaries.queues.TerminalQueuesSummary
import services.imports.ArrivalCrunchSimulationActor
import services.{SDate, TryRenjin}

import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

trait WithSimulations {
  self: Application =>

  def simulationImport(): Action[AnyContent] = authByRole(ArrivalSimulationUpload) {
    Action(parse.defaultBodyParser).async {
      request =>
        implicit val timeout: Timeout = new Timeout(2 minutes)

        SimulationParams
          .fromQueryStringParams(request.queryString) match {
          case Success(simulationParams) =>
            val simulationConfig = simulationParams.applyToAirportConfig(airportConfig)

            val date = SDate(simulationParams.date)
            val eventualFlightsWithSplitsStream: Future[Source[FlightsWithSplits, NotUsed]] = (ctrl.portStateActor ? GetFlightsForTerminalDateRange(
              date.getLocalLastMidnight.millisSinceEpoch,
              date.getLocalNextMidnight.millisSinceEpoch,
              simulationParams.terminal
            )).mapTo[Source[FlightsWithSplits, NotUsed]]

            FlightsRouterActor.runAndCombine(eventualFlightsWithSplitsStream).map { fws =>

              retrieveSimulationDesks(simulationParams, simulationConfig, date, fws)
            }.flatten

          case Failure(e) =>
            log.error("Invalid Simulation attempt", e)
            Future(BadRequest(e.getMessage))
        }
    }
  }

  def retrieveSimulationDesks(simulationParams: SimulationParams, simulationConfig: AirportConfig, date: SDateLike, fws: FlightsWithSplits): Future[Result] = {
    implicit val timeout: Timeout = new Timeout(2 minutes)
    val portStateActor = system.actorOf(Props(new ArrivalCrunchSimulationActor(simulationParams.applyPassengerWeighting(fws))))

    val (runnableDeskRecs, _): (SourceQueueWithComplete[MillisSinceEpoch], UniqueKillSwitch) = RunnableDeskRecs(
      portStateActor,
      DesksAndWaitsPortProvider(simulationConfig, TryRenjin.crunch, PcpPax.bestPaxEstimateWithApi),
      PortDeskLimits.fixed(simulationConfig)
    ).run()

    runnableDeskRecs.offer(date.millisSinceEpoch)

    val futureDeskRecMinutes: Future[DeskRecMinutes] = (portStateActor ? GetState).map {
      case drm: DeskRecMinutes => DeskRecMinutes(drm.minutes.filter(_.terminal == simulationParams.terminal))
    }

    val queues = simulationConfig.nonTransferQueues(simulationParams.terminal)
    val minutes = date.getLocalLastMidnight.millisSinceEpoch to date.getLocalNextMidnight.millisSinceEpoch by 15 * MilliTimes.oneMinuteMillis

    futureDeskRecMinutes.map(deskRecMinutes => {

      val crunchMinutes: SortedMap[TQM, CrunchMinute] = SortedMap[TQM, CrunchMinute]() ++ deskRecMinutes
        .minutes
        .map(dr => dr.key -> dr.toMinute).toMap

      val desks = TerminalQueuesSummary(queues, Exports.queueSummaries(queues, 15, minutes, crunchMinutes, SortedMap())).toCsvWithHeader

      Exports.csvFileResult(
        CsvFileStreaming.makeFileName(s"simulation-${simulationParams.passengerWeighting}",
          simulationParams.terminal,
          date,
          date,
          airportConfig.portCode
        ),
        desks
      )
    })
  }

}
