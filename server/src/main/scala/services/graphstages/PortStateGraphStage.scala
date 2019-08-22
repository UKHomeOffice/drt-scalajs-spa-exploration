package services.graphstages

import actors.FlightMessageConversion
import akka.stream._
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{FlightsWithSplits, QueueName, TerminalName}
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import server.protobuf.messages.CrunchState.{CrunchDiffMessage, CrunchMinuteMessage, StaffMinuteMessage}
import services.SDate

import scala.collection.immutable.Map

case class PortStateWithDiff(portState: PortState, diff: PortStateDiff, diffMessage: CrunchDiffMessage) {
  def window(start: SDateLike, end: SDateLike, portQueues: Map[TerminalName, Seq[QueueName]]): PortStateWithDiff = {
    PortStateWithDiff(portState.window(start, end, portQueues), diff, crunchDiffWindow(start, end))
  }

  def crunchDiffWindow(start: SDateLike, end: SDateLike): CrunchDiffMessage = {
    val flightsToRemove = diffMessage.flightIdsToRemove
    val flightsToUpdate = diffMessage.flightsToUpdate.filter(smm => smm.flight.exists(f => start.millisSinceEpoch <= f.scheduled.getOrElse(0L) && f.scheduled.getOrElse(0L) <= end.millisSinceEpoch))
    val staffToUpdate = diffMessage.staffMinutesToUpdate.filter(smm => start.millisSinceEpoch <= smm.minute.getOrElse(0L) && smm.minute.getOrElse(0L) <= end.millisSinceEpoch)
    val crunchToUpdate = diffMessage.crunchMinutesToUpdate.filter(cmm => start.millisSinceEpoch <= cmm.minute.getOrElse(0L) && cmm.minute.getOrElse(0L) <= end.millisSinceEpoch)

    CrunchDiffMessage(Option(SDate.now().millisSinceEpoch), None, flightsToRemove, flightsToUpdate, crunchToUpdate, staffToUpdate)
  }
}

class PortStateGraphStage(name: String = "",
                          optionalInitialPortState: Option[PortState],
                          airportConfig: AirportConfig,
                          expireAfterMillis: MillisSinceEpoch,
                          now: () => SDateLike)
  extends GraphStage[FanInShape5[FlightsWithSplits, DeskRecMinutes, ActualDeskStats, StaffMinutes, SimulationMinutes, PortStateWithDiff]] {

  val inFlightsWithSplits: Inlet[FlightsWithSplits] = Inlet[FlightsWithSplits]("FlightWithSplits.in")
  val inDeskRecMinutes: Inlet[DeskRecMinutes] = Inlet[DeskRecMinutes]("DeskRecMinutes.in")
  val inActualDeskStats: Inlet[ActualDeskStats] = Inlet[ActualDeskStats]("ActualDeskStats.in")
  val inStaffMinutes: Inlet[StaffMinutes] = Inlet[StaffMinutes]("StaffMinutes.in")
  val inSimulationMinutes: Inlet[SimulationMinutes] = Inlet[SimulationMinutes]("SimulationMinutes.in")
  val outPortState: Outlet[PortStateWithDiff] = Outlet[PortStateWithDiff]("PortStateWithDiff.out")

  override val shape = new FanInShape5(
    inFlightsWithSplits,
    inDeskRecMinutes,
    inActualDeskStats,
    inStaffMinutes,
    inSimulationMinutes,
    outPortState
  )

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    val portState: PortStateMutable = PortStateMutable.empty
    var maybePortStateDiff: Option[PortStateDiff] = None
    val log: Logger = LoggerFactory.getLogger(s"$getClass-$name")

    override def preStart(): Unit = {
      log.info(s"Received initial port state")
      optionalInitialPortState.map { ps =>
        portState.flights ++ ps.flights
        portState.crunchMinutes ++ ps.crunchMinutes
        portState.staffMinutes ++ ps.staffMinutes
      }
      super.preStart()
    }

    shape.inlets.foreach(inlet => {
      setHandler(inlet, new InHandler {
        override def onPush(): Unit = {
          val stateDiff = grab(inlet) match {
            case incoming: PortStateMinutes =>
              log.info(s"Incoming ${inlet.toString}")
              val startTime = now().millisSinceEpoch
              val expireThreshold = now().addMillis(-1 * expireAfterMillis.toInt).millisSinceEpoch
              val diff = incoming.applyTo(portState, startTime)
//              portState.purgeOlderThanDate(expireThreshold)
              val elapsedSeconds = (now().millisSinceEpoch - startTime).toDouble / 1000
              log.info(f"Finished processing $inlet data in $elapsedSeconds%.2f seconds")
              diff
          }

          maybePortStateDiff = mergeDiff(maybePortStateDiff, stateDiff)

          pushIfAppropriate()

          pullAllInlets()
        }
      })
    })

    setHandler(outPortState, new OutHandler {
      override def onPull(): Unit = {
        val start = now()
        log.info(s"onPull() called")
        pushIfAppropriate()

        pullAllInlets()
        log.info(s"outPortState Took ${now().millisSinceEpoch - start.millisSinceEpoch}ms")
      }
    })

    def mergeDiff(maybePortStateDiff: Option[PortStateDiff], newDiff: PortStateDiff): Option[PortStateDiff] = maybePortStateDiff match {
      case None => Option(newDiff)
      case Some(existingDiff) =>
        val updatedFlightRemovals = newDiff.flightRemovals.foldLeft(existingDiff.flightRemovals) {
          case (soFar, newRemoval) => if (soFar.contains(newRemoval)) soFar else soFar :+ newRemoval
        }
        val updatedFlights = newDiff.flightUpdates.foldLeft(existingDiff.flightUpdates) {
          case (soFar, (id, newFlight)) => if (soFar.contains(id)) soFar else soFar.updated(id, newFlight)
        }
        val updatedCms = newDiff.crunchMinuteUpdates.foldLeft(existingDiff.crunchMinuteUpdates) {
          case (soFar, (id, newCm)) => if (soFar.contains(id)) soFar else soFar.updated(id, newCm)
        }
        val updatedSms = newDiff.staffMinuteUpdates.foldLeft(existingDiff.staffMinuteUpdates) {
          case (soFar, (id, newSm)) => if (soFar.contains(id)) soFar else soFar.updated(id, newSm)
        }
        Option(existingDiff.copy(
          flightRemovals = updatedFlightRemovals,
          flightUpdates = updatedFlights,
          crunchMinuteUpdates = updatedCms,
          staffMinuteUpdates = updatedSms))
    }

    def pullAllInlets(): Unit = {
      shape.inlets.foreach(i => if (!hasBeenPulled(i)) {
        log.info(s"Pulling $i")
        pull(i)
      })
    }

    def pushIfAppropriate(): Unit = {
      maybePortStateDiff match {
        case None => log.info(s"No port state diff to push yet")
        case Some(_) if !isAvailable(outPortState) =>
          log.info(s"outPortState not available for pushing")
        case Some(portStateDiff) if portStateDiff.isEmpty =>
          log.info(s"Empty PortStateDiff. Nothing to push")
        case Some(portStateDiff) =>
          log.info(s"Pushing port state with diff")
          val portStateWithDiff = PortStateWithDiff(PortState.empty, portStateDiff, diffMessage(portStateDiff))
          maybePortStateDiff = None
          push(outPortState, portStateWithDiff)
      }
    }
  }

  def diffMessage(diff: PortStateDiff): CrunchDiffMessage = CrunchDiffMessage(
    createdAt = Option(now().millisSinceEpoch),
    crunchStart = Option(0),
    flightIdsToRemove = diff.flightRemovals.map(_.flightKey.uniqueId),
    flightsToUpdate = diff.flightUpdates.values.map(FlightMessageConversion.flightWithSplitsToMessage).toList,
    crunchMinutesToUpdate = diff.crunchMinuteUpdates.values.map(crunchMinuteToMessage).toList,
    staffMinutesToUpdate = diff.staffMinuteUpdates.values.map(staffMinuteToMessage).toList
  )

  def crunchMinuteToMessage(cm: CrunchMinute): CrunchMinuteMessage = CrunchMinuteMessage(
    terminalName = Option(cm.terminalName),
    queueName = Option(cm.queueName),
    minute = Option(cm.minute),
    paxLoad = Option(cm.paxLoad),
    workLoad = Option(cm.workLoad),
    deskRec = Option(cm.deskRec),
    waitTime = Option(cm.waitTime),
    simDesks = cm.deployedDesks,
    simWait = cm.deployedWait,
    actDesks = cm.actDesks,
    actWait = cm.actWait
  )

  def staffMinuteToMessage(sm: StaffMinute): StaffMinuteMessage = StaffMinuteMessage(
    terminalName = Option(sm.terminalName),
    minute = Option(sm.minute),
    shifts = Option(sm.shifts),
    fixedPoints = Option(sm.fixedPoints),
    movements = Option(sm.movements))
}
