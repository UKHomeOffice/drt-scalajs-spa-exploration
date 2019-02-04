package services.graphstages

import akka.stream._
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{QueueName, TerminalName}
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.graphstages.Crunch._
import services.graphstages.StaffDeploymentCalculator.deploymentWithinBounds
import services.{SDate, _}

import scala.collection.immutable
import scala.collection.immutable.{Map, NumericRange}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}


class SimulationGraphStage(name: String = "",
                           optionalInitialCrunchMinutes: Option[CrunchMinutes],
                           optionalInitialStaffMinutes: Option[StaffMinutes],
                           airportConfig: AirportConfig,
                           expireAfterMillis: MillisSinceEpoch,
                           now: () => SDateLike,
                           simulate: Simulator,
                           crunchPeriodStartMillis: SDateLike => SDateLike,
                           minutesToCrunch: Int)
  extends GraphStage[FanInShape2[Loads, StaffMinutes, SimulationMinutes]] {

  type TerminalLoad = Map[QueueName, Map[MillisSinceEpoch, Double]]
  type PortLoad = Map[TerminalName, TerminalLoad]

  val inLoads: Inlet[Loads] = Inlet[Loads]("inLoads.in")
  val inStaffMinutes: Inlet[StaffMinutes] = Inlet[StaffMinutes]("inStaffMinutes.in")
  val outSimulationMinutes: Outlet[SimulationMinutes] = Outlet[SimulationMinutes]("outSimulationMinutes.out")

  override val shape = new FanInShape2[Loads, StaffMinutes, SimulationMinutes](inLoads, inStaffMinutes, outSimulationMinutes)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    var loadMinutes: Map[TQM, LoadMinute] = Map()
    var staffMinutes: Map[TM, StaffMinute] = Map()
    var deployments: Map[(TerminalName, QueueName, MillisSinceEpoch), Int] = Map()
    var allSimulationMinutes: Map[TQM, SimulationMinute] = Map()
    var simulationMinutesToPush: Map[TQM, SimulationMinute] = Map()

    val log: Logger = LoggerFactory.getLogger(s"$getClass-$name")

    override def preStart(): Unit = {
      loadMinutes = optionalInitialCrunchMinutes match {
        case None => Map()
        case Some(CrunchMinutes(cms)) =>
          log.info(s"Received ${cms.size} initial crunch minutes")
          cms.map(cm => {
            val lm = LoadMinute(cm.terminalName, cm.queueName, cm.paxLoad, cm.workLoad, cm.minute)
            (lm.uniqueId, lm)
          }).toMap
      }

      staffMinutes = optionalInitialStaffMinutes match {
        case None => Map()
        case Some(StaffMinutes(sms)) =>
          log.info(s"Received ${sms.size} initial staff minutes")
          sms.map(sm => {
            (sm.key, sm)
          }).toMap
      }

      deployments = optionalInitialCrunchMinutes match {
        case None => Map()
        case Some(CrunchMinutes(cms)) => cms
          .groupBy(_.terminalName)
          .flatMap {
            case (tn: TerminalName, tCms) => tCms
              .groupBy(_.queueName)
              .flatMap {
                case (qn: QueueName, qCms: Set[CrunchMinute]) => qCms
                  .toSeq
                  .map(cm => ((tn, qn, cm.minute), cm.deployedDesks.getOrElse(0)))
                  .toMap
              }
          }
      }

      super.preStart()
    }

    setHandler(inLoads, new InHandler {
      override def onPush(): Unit = {
        val start = SDate.now()
        val incomingLoads = grab(inLoads)
        log.info(s"Received ${incomingLoads.loadMinutes.size} loads")

        val affectedTerminals = incomingLoads.loadMinutes.map(_.terminalName)

        val updatedLoads: Map[TQM, LoadMinute] = mergeLoads(incomingLoads.loadMinutes, loadMinutes)
        loadMinutes = purgeExpired(updatedLoads, (lm: LoadMinute) => lm.minute, now, expireAfterMillis)

        val allMinuteMillis = incomingLoads.loadMinutes.map(_.minute)
        val firstMinute = crunchPeriodStartMillis(SDate(allMinuteMillis.min))
        val lastMinute = firstMinute.addMinutes(minutesToCrunch)

        if (availableStaffForPeriodWithNonZeroDeployments(affectedTerminals, firstMinute, lastMinute).nonEmpty) {
          val accessor = (x: (TerminalName, QueueName, MillisSinceEpoch)) => x._3
          val updatedDeployments: Map[(TerminalName, QueueName, MillisSinceEpoch), Int] = updateDeployments(availableStaffForPeriodWithNonZeroDeployments(affectedTerminals, firstMinute, lastMinute), firstMinute, lastMinute, deployments)
          deployments = Crunch.purgeExpiredTuple(updatedDeployments, accessor, now, expireAfterMillis)
          updateSimulationsForPeriod(firstMinute, lastMinute, affectedTerminals)

          pushStateIfReady()
        } else log.info(s"Zero deployments. Skipping simulations")

        pullAll()
        log.info(s"inLoads Took ${SDate.now().millisSinceEpoch - start.millisSinceEpoch}ms")
      }
    })

    setHandler(inStaffMinutes, new InHandler {
      override def onPush(): Unit = {
        val start = SDate.now()
        val incomingStaffMinutes: StaffMinutes = grab(inStaffMinutes)
        log.info(s"Grabbed ${incomingStaffMinutes.minutes.length} staff minutes")

        val affectedTerminals = incomingStaffMinutes.minutes.map(_.terminalName).toSet

        log.info(s"Staff updates affect ${affectedTerminals.mkString(", ")}")

        staffMinutes = purgeExpired(updateStaffMinutes(staffMinutes, incomingStaffMinutes), (sm: StaffMinute) => sm.minute, now, expireAfterMillis)

        log.info(s"Purged expired staff minutes")

        val firstMinute = crunchPeriodStartMillis(SDate(incomingStaffMinutes.minutes.map(_.minute).min))
        val lastMinute = firstMinute.addDays(1)

        log.info(s"Got first ${firstMinute.toLocalDateTimeString()} and last minutes ${lastMinute.toLocalDateTimeString()}")

        deployments = updateDeployments(affectedTerminals, firstMinute, lastMinute, deployments)

        log.info(s"Got deployments, updating simulations")
        updateSimulationsForPeriod(firstMinute, lastMinute, affectedTerminals)

        pushStateIfReady()

        pullAll()
        log.info(s"inStaffMinutes Took ${SDate.now().millisSinceEpoch - start.millisSinceEpoch}ms")
      }
    })

    def updateDeployments(affectedTerminals: Set[TerminalName],
                          firstMinute: SDateLike,
                          lastMinute: SDateLike,
                          existingDeployments: Map[(TerminalName, QueueName, MillisSinceEpoch), Int]
                         ): Map[(TerminalName, QueueName, MillisSinceEpoch), Int] = {
      val firstMillis = firstMinute.millisSinceEpoch
      val lastMillis = lastMinute.millisSinceEpoch

      val deploymentUpdates = deploymentsForMillis(firstMillis, lastMillis, affectedTerminals)

      log.info(s"Merging updated deployments into existing")
      val updatedDeployments = deploymentUpdates.foldLeft(existingDeployments) {
        case (soFar, (tqm, staff)) => soFar.updated(tqm, staff)
      }

      updatedDeployments
    }

    setHandler(outSimulationMinutes, new OutHandler {
      override def onPull(): Unit = {
        val start = SDate.now()
        log.debug(s"outSimulationMinutes onPull called")
        pushStateIfReady()
        pullAll()
        log.info(s"outSimulationMinutes Took ${SDate.now().millisSinceEpoch - start.millisSinceEpoch}ms")
      }
    })

    def updateSimulationsForPeriod(firstMinute: SDateLike,
                                   lastMinute: SDateLike,
                                   terminalsToUpdate: Set[TerminalName]
                                  ): Unit = {
      log.info(s"Simulation for ${firstMinute.toLocalDateTimeString()} - ${lastMinute.toLocalDateTimeString()} ${terminalsToUpdate.mkString(", ")}")

      val newSimulationsForPeriod: Set[SimulationMinute] = simulateLoads(firstMinute.millisSinceEpoch, lastMinute.millisSinceEpoch, terminalsToUpdate)

      val diff = newSimulationsForPeriod -- existingSimulationsForPeriod(firstMinute, lastMinute, terminalsToUpdate).values.toSet

      val updatedSims = diff.foldLeft(allSimulationMinutes) {
        case (allSims, sm) => allSims.updated(TQM(sm.terminalName, sm.queueName, sm.minute), sm)
      }

      allSimulationMinutes = purgeExpired(updatedSims, (sm: SimulationMinute) => sm.minute, now, expireAfterMillis)

      val mergedSimulationMinutesToPush = mergeSimulationMinutes(diff, simulationMinutesToPush)
      simulationMinutesToPush = purgeExpired(mergedSimulationMinutesToPush, (sm: SimulationMinute) => sm.minute, now, expireAfterMillis)
      log.info(s"Now have ${simulationMinutesToPush.size} simulation minutes to push")
    }

    def existingSimulationsForPeriod(firstMinute: SDateLike,
                                     lastMinute: SDateLike,
                                     terminalsToUpdate: Set[TerminalName]): Map[TQM, SimulationMinute] = allSimulationMinutes
      .filter {
        case (TQM(t, _, m), _) => terminalsToUpdate.contains(t) && firstMinute.millisSinceEpoch <= m && lastMinute.millisSinceEpoch <= m
      }

    def updateStaffMinutes(existingStaffMinutes: Map[TM, StaffMinute], incomingStaffMinutes: StaffMinutes): Map[TM, StaffMinute] = incomingStaffMinutes
      .minutes
      .foldLeft(existingStaffMinutes) {
        case (soFar, sm) => soFar.updated(sm.key, sm)
      }

    def simulateLoads(firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, terminalsToUpdate: Set[TerminalName]): Set[SimulationMinute] = {
      log.info(s"calling workloadForPeriod")
      val workload: PortLoad = workloadForPeriod(firstMinute, lastMinute, terminalsToUpdate)
      val minuteMillis = firstMinute until lastMinute by 60000
      log.info(s"millis range: ${minuteMillis.min} - ${minuteMillis.max}")

      val simulationMinutes = terminalsToUpdate.flatMap(tn => {
        workload.getOrElse(tn, Map()).flatMap {
          case (qn, queueWorkload) => simulationForQueue(minuteMillis, tn, qn, queueWorkload)
        }
      })
      log.info(s"done simulating")

      simulationMinutes
    }

    def simulationForQueue(minuteMillis: NumericRange[MillisSinceEpoch], tn: TerminalName, qn: QueueName, queueWorkload: Map[MillisSinceEpoch, Double]): Set[SimulationMinute] = {
      val sla = airportConfig.slaByQueue.getOrElse(qn, 15)
      val adjustedWorkloadMinutes = if (qn == Queues.EGate) adjustEgatesWorkload(queueWorkload) else queueWorkload
      val fullWorkMinutes = minuteMillis.map(m => adjustedWorkloadMinutes.getOrElse(m, 0d))


      minuteMillis.map(m => deployments.getOrElse((tn, qn, m), 0)) match {
        case noStaff if noStaff.count(_ > 0) == 0 =>
          log.info(s"No deployed staff. Skipping simulations")
          Set()
        case deployedDesks if deployedDesks.count(_ > 0) > 0 =>
          log.info(s"Running $tn, $qn simulation with ${fullWorkMinutes.length} workloads & ${deployedDesks.length} desks")
          Try(simulate(fullWorkMinutes, deployedDesks, OptimizerConfig(sla))) match {
            case Success(waits) =>
              minuteMillis.zipWithIndex.map {
                case (minute, idx) => SimulationMinute(tn, qn, minute, deployedDesks(idx), waits(idx))
              }.toSet
            case Failure(t) =>
              val start = SDate(minuteMillis.min).toLocalDateTimeString()
              val end = SDate(minuteMillis.max).toLocalDateTimeString()
              log.error(s"Failed to run simulations for $tn / $qn - $start -> $end: $t")
              log.error(s"${deployedDesks.length} desks: $deployedDesks")
              log.error(s"${fullWorkMinutes.length} works minutes: $fullWorkMinutes")
              Set()
          }
      }
    }

    def deploymentsForMillis(firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, terminalsToUpdate: Set[TerminalName]): Map[(TerminalName, String, MillisSinceEpoch), Int] = {
      val workload = workloadForPeriod(firstMinute, lastMinute, terminalsToUpdate)

      val minuteMillis = firstMinute until lastMinute by 60000
      log.info(s"Getting available staff")
      val availableStaff: Map[TerminalName, Map[MillisSinceEpoch, Int]] = availableStaffForPeriod(firstMinute, lastMinute, terminalsToUpdate)
      log.info(s"Getting min max desks")
      val minMaxDesks: Map[TerminalName, Map[QueueName, Map[MillisSinceEpoch, (Int, Int)]]] = minMaxDesksForMillis(minuteMillis)

      log.info(s"Getting queue deployments")
      terminalsToUpdate
        .flatMap(tn => {
          val terminalWorkloads: Map[QueueName, Map[MillisSinceEpoch, Double]] = workload.getOrElse(tn, Map())
          val queues = airportConfig.nonTransferQueues(tn)

          minuteMillis
            .sliding(15, 15)
            .flatMap(slotMillis => {
              val queueDeps = availableStaff.getOrElse(tn, Map()).getOrElse(slotMillis.min, 0) match {
                case 0 => zeroQueueDeployments(tn)
                case availableForSlot => queueDeployments(availableForSlot, minMaxDesks, tn, queues, terminalWorkloads, slotMillis)
              }
              queueDeps.flatMap { case (qn, staff) => slotMillis.map(millis => ((tn, qn, millis), staff)) }
            }
            )
        })
        .toMap
    }

    def queueDeployments(available: Int,
                         minMaxDesks: Map[TerminalName, Map[QueueName, Map[MillisSinceEpoch, (Int, Int)]]],
                         terminalName: TerminalName,
                         queues: Seq[String],
                         terminalWorkloads: Map[QueueName, Map[MillisSinceEpoch, Double]],
                         slotMillis: immutable.IndexedSeq[MillisSinceEpoch]
                        ): Seq[(String, Int)] = {
      val queuesWithoutTransfer = airportConfig.nonTransferQueues(terminalName)
      val queueWl = slaWeightedLoadByQueue(queuesWithoutTransfer, terminalWorkloads, slotMillis)
      val slotStartMilli = slotMillis.min
      val queueMm = minMaxDesks.getOrElse(terminalName, Map()).mapValues(_.getOrElse(slotStartMilli, (0, 0)))

      deployer(queueWl, available, queueMm)
    }

    def slaWeightedLoadByQueue(queuesWithoutTransfer: Seq[QueueName], terminalWorkloads: TerminalLoad, slotMillis: IndexedSeq[Long]): Seq[(QueueName, Double)] = queuesWithoutTransfer
      .map(qn => {
        val queueWorkloads = terminalWorkloads.getOrElse(qn, Map())
        val slaWeight = Math.log(airportConfig.slaByQueue(qn))
        (qn, slotMillis.map(milli => {
          val workloadForMilli = queueWorkloads.getOrElse(milli, 0d)
          val slaWeightedWorkload = workloadForMilli * (10d / slaWeight)
          val adjustedForEgates = if (qn == Queues.EGate) slaWeightedWorkload / airportConfig.eGateBankSize else slaWeightedWorkload
          adjustedForEgates
        }).sum)
      })

    def minMaxDesksForMillis(minuteMillis: Seq[Long]): Map[TerminalName, Map[QueueName, Map[MillisSinceEpoch, (Int, Int)]]] = airportConfig
      .minMaxDesksByTerminalQueue
      .mapValues(qmm => qmm.mapValues {
        case (minDesks, maxDesks) =>
          minuteMillis.map(m => {
            val min = desksForHourOfDayInUKLocalTime(m, minDesks)
            val max = desksForHourOfDayInUKLocalTime(m, maxDesks)
            (m, (min, max))
          }).toMap
      })

    def workloadForPeriod(firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, terminalsToUpdate: Set[TerminalName]): PortLoad = {
      val loadsByTerminal = filterTerminalQueueMinutes(firstMinute, lastMinute, terminalsToUpdate, loadMinutes)
        .groupBy(_.terminalName)

      terminalsToUpdate
        .map(tn => {
          val terminalLoads = loadsByTerminal
            .getOrElse(tn, Set())
            .groupBy(_.queueName)
            .mapValues(qlms => qlms.toSeq.map(lm => (lm.minute, lm.workLoad)).toMap)
          (tn, terminalLoads)
        })
        .toMap
    }

    def filterTerminalQueueMinutes[A <: TerminalQueueMinute](firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, terminalsToUpdate: Set[TerminalName], toFilter: Map[TQM, A]): Set[A] = {
      val maybeThings = for {
        terminalName <- terminalsToUpdate
        queueName <- airportConfig.nonTransferQueues(terminalName)
        minute <- firstMinute until lastMinute by oneMinuteMillis
      } yield
        toFilter.get(MinuteHelper.key(terminalName, queueName, minute))

      maybeThings.collect { case Some(thing) => thing }
    }

    def filterTerminalMinutes[A <: TerminalMinute](firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, terminalsToUpdate: Set[TerminalName], toFilter: Map[TM, A]): Set[A] = {
      val maybeThings = for {
        terminalName <- terminalsToUpdate
        minute <- firstMinute until lastMinute by oneMinuteMillis
      } yield toFilter.get(MinuteHelper.key(terminalName, minute))

      maybeThings.collect { case Some(thing) => thing }
    }

    def adjustEgatesWorkload(workload: Map[MillisSinceEpoch, Double]): Map[MillisSinceEpoch, Double] = workload
      .mapValues(wl => adjustEgateWorkload(airportConfig.eGateBankSize, wl))

    def adjustEgateWorkload(eGateBankSize: Int, wl: Double): Double = wl / eGateBankSize

    var deploymentCache: Map[Int, Seq[(String, Int)]] = Map()

    @scala.annotation.tailrec
    def addOneStaffToQueueAtIndex(deployments: List[(String, Int, Int)], index: Int, numberOfQueues: Int, staffAvailable: Int): List[(String, Int, Int)] = {
      val safeIndex = if (index > numberOfQueues - 1) 0 else index
      val deployedStaff = deployments.map(_._2).sum
      val maxStaff = deployments.map(_._3).sum
      val freeStaff = staffAvailable - deployedStaff

      if (deployedStaff != maxStaff && freeStaff > 0) {
        val (queue, staffDeployments, maxStaff) = deployments(safeIndex)
        val newDeployments = if (staffDeployments < maxStaff) deployments.updated(safeIndex, Tuple3(queue, staffDeployments + 1, maxStaff)) else deployments
        addOneStaffToQueueAtIndex(newDeployments, safeIndex + 1, numberOfQueues, staffAvailable)
      } else {
        deployments
      }
    }

    def queueRecsToDeployments(round: Double => Int)
                              (queueRecs: Seq[(String, Double)], staffAvailable: Int, minMaxDesks: Map[String, (Int, Int)]): Seq[(String, Int)] = {
      val key = (queueRecs, staffAvailable, minMaxDesks).hashCode()

      deploymentCache.get(key) match {
        case Some(deps) => deps
        case None =>
          val queueRecsCorrected = if (queueRecs.map(_._2).sum == 0) queueRecs.map(qr => (qr._1, 1d)) else queueRecs

          val totalStaffRec = queueRecsCorrected.map(_._2).sum

          val deployments = queueRecsCorrected.foldLeft(List[(String, Int, Int)]()) {
            case (agg, (queue, deskRec)) if agg.length < queueRecsCorrected.length - 1 =>
              val ideal = round(staffAvailable * (deskRec.toDouble / totalStaffRec))
              val totalRecommended = agg.map(_._2).sum
              val maxStaff = minMaxDesks.getOrElse(queue, (0, 10))._2
              val staffDeployments = deploymentWithinBounds(minMaxDesks.getOrElse(queue, (0, 10))._1, maxStaff, ideal, staffAvailable - totalRecommended)
              agg :+ Tuple3(queue, staffDeployments, maxStaff)
            case (agg, (queue, _)) =>
              val totalRecommended = agg.map(_._2).sum
              val ideal = staffAvailable - totalRecommended
              val maxStaff = minMaxDesks.getOrElse(queue, (0, 10))._2
              val staffDeployments = deploymentWithinBounds(minMaxDesks.getOrElse(queue, (0, 10))._1, maxStaff, ideal, staffAvailable - totalRecommended)
              agg :+ Tuple3(queue, staffDeployments, maxStaff)
          }
          val newDeployments = addOneStaffToQueueAtIndex(deployments, index = 0, queueRecsCorrected.length, staffAvailable)
          newDeployments.map(tuple3 => Tuple2(tuple3._1, tuple3._2))
      }
    }

    val deployer: (Seq[(String, Double)], Int, Map[String, (Int, Int)]) => Seq[(String, Int)] = queueRecsToDeployments(_.toInt)

    def minMaxDesksForQueue(simulationMinutes: Seq[MillisSinceEpoch], tn: TerminalName, qn: QueueName): (Seq[Int], Seq[Int]) = {
      val defaultMinMaxDesks = (Seq.fill(24)(0), Seq.fill(24)(10))
      val queueMinMaxDesks = airportConfig.minMaxDesksByTerminalQueue.getOrElse(tn, Map()).getOrElse(qn, defaultMinMaxDesks)
      val minDesks = simulationMinutes.map(desksForHourOfDayInUKLocalTime(_, queueMinMaxDesks._1))
      val maxDesks = simulationMinutes.map(desksForHourOfDayInUKLocalTime(_, queueMinMaxDesks._2))
      (minDesks, maxDesks)
    }

    def mergeSimulationMinutes(updatedCms: Set[SimulationMinute], existingCms: Map[TQM, SimulationMinute]): Map[TQM, SimulationMinute] = updatedCms.foldLeft(existingCms) {
      case (soFar, newLoadMinute) => soFar.updated(newLoadMinute.key, newLoadMinute)
    }

    def loadDiff(updatedLoads: Set[LoadMinute], existingLoads: Set[LoadMinute]): Set[LoadMinute] = {
      val loadDiff = updatedLoads -- existingLoads
      log.info(s"${loadDiff.size} updated load minutes")

      loadDiff
    }

    def mergeLoads(incomingLoads: Set[LoadMinute], existingLoads: Map[TQM, LoadMinute]): Map[TQM, LoadMinute] = incomingLoads.foldLeft(existingLoads) {
      case (soFar, load) => soFar.updated(load.uniqueId, load)
    }

    def pullAll(): Unit = {
      if (!hasBeenPulled(inLoads)) {
        log.info(s"Pulling inFlightsWithSplits")
        pull(inLoads)
      }
      if (!hasBeenPulled(inStaffMinutes)) {
        log.info(s"Pulling inStaffMinutes")
        pull(inStaffMinutes)
      }
    }

    def pushStateIfReady(): Unit = {
      if (simulationMinutesToPush.isEmpty) log.info(s"We have no simulation minutes. Nothing to push")
      else if (isAvailable(outSimulationMinutes)) {
        log.info(s"Pushing ${simulationMinutesToPush.size} simulation minutes")
        push(outSimulationMinutes, SimulationMinutes(simulationMinutesToPush.values.toSet))
        simulationMinutesToPush = Map()
      } else log.info(s"outSimulationMinutes not available to push")
    }

    def availableStaffForPeriod(firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, terminalNames: Set[TerminalName]): Map[TerminalName, Map[MillisSinceEpoch, Int]] =
      filterTerminalMinutes(firstMinute, lastMinute, terminalNames, staffMinutes)
        .groupBy(_.terminalName)
        .mapValues { sms =>
          sms.map(sm => (sm.minute, sm.availableAtPcp)).toMap
        }

    def availableStaffForPeriodWithNonZeroDeployments(affectedTerminals: Set[TerminalName], firstMinute: SDateLike, lastMinute: SDateLike): Set[TerminalName] = {
      availableStaffForPeriod(firstMinute.millisSinceEpoch, lastMinute.millisSinceEpoch, affectedTerminals)
        .foldLeft(List[TerminalName]()) {
          case (tns, (tn, staffByMillis)) => if (staffByMillis.count(_._2 > 0) > 0) tn :: tns else tns
        }
        .toSet
    }
  }

  val zeroQueueDeployments: Map[TerminalName, Seq[(QueueName, Int)]] = airportConfig
    .terminalNames
    .map(tn => (tn, airportConfig.nonTransferQueues(tn).map(qn => (qn, 0))))
    .toMap
}

case class SimulationMinute(terminalName: TerminalName,
                            queueName: QueueName,
                            minute: MillisSinceEpoch,
                            desks: Int,
                            waitTime: Int) extends SimulationMinuteLike {
  lazy val key: TQM = MinuteHelper.key(terminalName, queueName, minute)
}

case class SimulationMinutes(minutes: Set[SimulationMinute]) extends PortStateMinutes {
  def applyTo(maybePortState: Option[PortState], now: SDateLike): Option[PortState] = {
    maybePortState match {
      case None => Option(PortState(Map(), newCrunchMinutes, Map()))
      case Some(portState) =>
        val updatedCrunchMinutes = minutes
          .foldLeft(portState.crunchMinutes) {
            case (soFar, updatedCm) =>
              val maybeMinute: Option[CrunchMinute] = soFar.get(updatedCm.key)
              val mergedCm: CrunchMinute = mergeMinute(maybeMinute, updatedCm)
              soFar.updated(updatedCm.key, mergedCm.copy(lastUpdated = Option(now.millisSinceEpoch)))
          }
        Option(portState.copy(crunchMinutes = updatedCrunchMinutes))
    }
  }

  def newCrunchMinutes: Map[TQM, CrunchMinute] = minutes
    .map(CrunchMinute(_))
    .map(cm => (cm.key, cm))
    .toMap

  def mergeMinute(maybeMinute: Option[CrunchMinute], updatedSm: SimulationMinute): CrunchMinute = maybeMinute
    .map(existingCm => existingCm.copy(
      deployedDesks = Option(updatedSm.desks),
      deployedWait = Option(updatedSm.waitTime),
      lastUpdated = Option(SDate.now().millisSinceEpoch)
    ))
    .getOrElse(CrunchMinute(updatedSm))
}
