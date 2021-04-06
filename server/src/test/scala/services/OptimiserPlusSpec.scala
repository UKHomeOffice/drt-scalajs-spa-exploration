package services

import org.slf4j.{Logger, LoggerFactory}
import org.specs2.mutable.Specification

import scala.collection.{immutable, mutable}
import scala.util.{Failure, Success, Try}

case class OptimizerConfig2(sla: Int, processorUnitSize: Processors)

class OptimiserPlusSpec extends Specification {
  //  "Given some 1 minutes of workload per minute, and desks fixed at 1 per minute" >> {
  //    "I should see all the workload completed each minute, leaving zero wait times" >> {
  //      val result: Try[OptimizerCrunchResult] = OptimiserPlus.crunch(Seq.fill(30)(1), Seq.fill(30)(1), Seq.fill(30)(1), OptimizerConfig2(20, None))
  //
  //      val expected = Seq.fill(30)(0)
  //
  //      result.get.waitTimes === expected
  //    }
  //  }
  //
  //  "Given some 2 minutes of workload per minute, and desks fixed at 1 per minute" >> {
  //    "I should see workload spilling over each minute, leaving increasing wait times" >> {
  //      val result: Try[OptimizerCrunchResult] = OptimiserPlus.crunch(Seq.fill(30)(2), Seq.fill(30)(1), Seq.fill(30)(1), OptimizerConfig2(20, None))
  //
  //      val expected = Seq(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10 , 11, 11, 12, 12, 13, 13, 14, 14, 15, 15)
  //
  //      result.get.waitTimes === expected
  //    }
  //  }

  "Given some 10 minutes of workload per minute, and egate banks of size 10 gates fixed at 1 bank per minute" >> {
    "I should see all the workload completed each minute, leaving zero wait times" >> {
      val result: Try[OptimizerCrunchResult] = OptimiserPlus.crunch(List.fill(60)(10), Seq.fill(60)(1), Seq.fill(60)(1), OptimizerConfig2(20, Processors(Iterable(10, 10, 10))))

      val expected = OptimizerCrunchResult(immutable.IndexedSeq.fill(60)(1), Seq.fill(60)(0))

      result.get === expected
    }
  }

  "Given some 10 minutes of workload per minute, and egate banks of size 10 gates fixed at 1 bank per minute" >> {
    "I should see all the workload completed each minute, leaving zero wait times" >> {
      val result: Try[OptimizerCrunchResult] = OptimiserPlus.crunch(List.fill(60)(20), Seq.fill(60)(1), Seq.fill(60)(1), OptimizerConfig2(20, Processors(Iterable(19, 10, 10))))

      val expected = OptimizerCrunchResult(immutable.IndexedSeq.fill(60)(1), Seq.fill(60)(0))

      result.get === expected
    }
  }
}

case class Processors(processors: Iterable[Int]) {
  val processorsWithZero: Iterable[Int] = processors.headOption match {
    case None => Iterable(0)
    case Some(zero) if zero == 0 => processors
    case Some(_) => Iterable(0) ++ processors
  }

  val cumulativeCapacity: List[Int] = processorsWithZero
    .foldLeft(List[Int]()) {
      case (acc, processors) => acc.sum + processors :: acc
    }
    .reverse

  val capacityForUnits: Map[Int, Int] = cumulativeCapacity.indices.zip(cumulativeCapacity).toMap

  val byWorkload: Map[Int, Int] = cumulativeCapacity
    .sliding(2).toList.zipWithIndex
    .flatMap {
      case (capacities, idx) => ((capacities.min + 1) to capacities.max).map(c => (c, idx + 1))
    }.toMap + (0 -> 0)

  val maxCapacity: Int = byWorkload.keys.max
  val maxProcessorUnits: Int = byWorkload.values.max

  val forWorkload: PartialFunction[Double, Int] = {
    case noWorkload if noWorkload <= 0 => 0
    case someWorkload => byWorkload.getOrElse(someWorkload.ceil.toInt, maxProcessorUnits)
  }
}

object OptimiserPlus {
  val log: Logger = LoggerFactory.getLogger(getClass)

  val weightSla = 10
  val weightChurn = 50
  val weightPax = 0.05
  val weightStaff = 3
  val blockSize = 5
  val targetWidth = 60
  val rollingBuffer = 120

  def crunch(workloads: Iterable[Double],
             minDesks: Iterable[Int],
             maxDesks: Iterable[Int],
             config: OptimizerConfig2): Try[OptimizerCrunchResult] = {
    val indexedWork = workloads.toIndexedSeq
    val indexedMinDesks = minDesks.toIndexedSeq

    val bestMaxDesks = /*if (workloads.size >= 60) {
      val fairMaxDesks = rollingFairXmax(indexedWork, indexedMinDesks, blockSize, (0.75 * config.sla).round.toInt, targetWidth, rollingBuffer, config.processorUnitSize)
      fairMaxDesks.zip(maxDesks).map { case (fair, orig) => List(fair, orig).min }
    } else*/ maxDesks.toIndexedSeq

    if (bestMaxDesks.exists(_ < 0)) log.warn(s"Max desks contains some negative numbers")

    for {
      desks <- tryOptimiseWin(indexedWork, indexedMinDesks, bestMaxDesks, config.sla, weightChurn, weightPax, weightStaff, weightSla, config.processorUnitSize)
      processedWork <- tryProcessWork(indexedWork, desks, config.sla, IndexedSeq(), config.processorUnitSize)
    } yield OptimizerCrunchResult(desks.toIndexedSeq, processedWork.waits)
  }

  def runSimulationOfWork(workloads: Iterable[Double], desks: Iterable[Int], config: OptimizerConfig): Try[Seq[Int]] =
    Optimiser.tryProcessWork(workloads.toIndexedSeq, desks.toIndexedSeq, config.sla, IndexedSeq()).map(_.waits)

  def approx(x: IndexedSeq[Int], y: IndexedSeq[Int], i: Seq[Double]): List[Double] = {
    val diffX = x(1) - x.head
    val diffY = y(1) - y.head
    val ratio = diffY.toDouble / diffX
    i.map(_ * ratio).toList
  }

  //  def leftwardDesks(work: IndexedSeq[Double],
  //                    xmin: IndexedSeq[Int],
  //                    xmax: IndexedSeq[Int],
  //                    blockSize: Int,
  //                    backlog: Double): IndexedSeq[Int] = {
  //    val workWithMinMaxDesks: Iterator[(IndexedSeq[Double], (IndexedSeq[Int], IndexedSeq[Int]))] = work.grouped(blockSize).zip(xmin.grouped(blockSize).zip(xmax.grouped(blockSize)))
  //
  //    workWithMinMaxDesks.foldLeft((List[Int](), backlog)) {
  //      case ((desks, bl), (workBlock, (xminBlock, xmaxBlock))) =>
  //        var guess = List(((bl + workBlock.sum) / blockSize).round.toInt, xmaxBlock.head).min
  //
  //        while (cumulativeSum(workBlock.map(_ - guess)).min < 0 - bl && guess > xminBlock.head) {
  //          guess = guess - 1
  //        }
  //
  //        guess = List(guess, xminBlock.head).max
  //
  //        val newBacklog = (0 until blockSize).foldLeft(bl) {
  //          case (accBl, i) =>
  //            List(accBl + workBlock(i) - guess, 0).max
  //        }
  //
  //        (desks ++ List.fill(blockSize)(guess), newBacklog)
  //    }._1.toIndexedSeq
  //  }

  def tryProcessWork(work: IndexedSeq[Double],
                     capacity: IndexedSeq[Int],
                     sla: Int,
                     qstart: IndexedSeq[Double],
                     processors: Processors): Try[ProcessedWork] = {
    if (capacity.length != work.length) {
      Failure(new Exception(s"capacity & work don't match: ${capacity.length} vs ${work.length}"))
    } else Try {
      var q = qstart
      var totalWait: Double = 0d
      var excessWait: Double = 0d

      val (finalWait, finalUtil) = work.indices.foldLeft((List[Int](), List[Double]())) {
        case ((wait, util), minute) =>
          q = work(minute) +: q
          val totalResourceForMinute = processors.capacityForUnits(capacity(minute))
          var resource: Double = totalResourceForMinute.toDouble
          var age = q.size

          while (age > 0) {
            val nextWorkToProcess = q(age - 1)
            val surplus = resource - nextWorkToProcess
            if (surplus >= 0) {
              totalWait = totalWait + nextWorkToProcess * (age - 1)
              if (age - 1 >= sla) excessWait = excessWait + nextWorkToProcess * (age - 1)
              q = q.dropRight(1)
              resource = surplus
              age = age - 1
            } else {
              totalWait = totalWait + resource * (age - 1)
              if (age - 1 >= sla) excessWait = excessWait + resource * (age - 1)
              q = q.dropRight(1) :+ (nextWorkToProcess - resource)
              resource = 0
              age = 0
            }
          }

          (q.size :: wait, (1 - (resource / totalResourceForMinute)) :: util)
      }

      val waitReversed = finalWait.reverse
      val utilReversed = finalUtil.reverse

      ProcessedWork(utilReversed, waitReversed, q, totalWait, excessWait)
    }
  }

  //  def rollingFairXmax(work: IndexedSeq[Double], xmin: IndexedSeq[Int], blockSize: Int, sla: Int, targetWidth: Int, rollingBuffer: Int, processors: Processors): IndexedSeq[Int] = {
  //    val workWithOverrun = work ++ List.fill(targetWidth)(0d)
  //    val xminWithOverrun = xmin ++ List.fill(targetWidth)(xmin.takeRight(1).head)
  //
  //    var backlog = 0d
  //
  //    val result = (workWithOverrun.indices by targetWidth).foldLeft(IndexedSeq[Int]()) { case (acc, startSlot) =>
  //      val winStart: Int = List(startSlot - rollingBuffer, 0).max
  //      val i = startSlot + targetWidth + rollingBuffer
  //      val i1 = workWithOverrun.size
  //      val winStop: Int = List(i, i1).min
  //      val winWork = workWithOverrun.slice(winStart, winStop)
  //      val winXmin = xminWithOverrun.slice(winStart, winStop)
  //
  //      if (winStart == 0) backlog = 0
  //
  //      val runAv = runningAverage(winWork, List(blockSize, sla).min, processors)
  //      val guessMax: Int = runAv.max.ceil.toInt
  //
  //      val lowerLimit = List(winXmin.max, (winWork.sum / winWork.size).ceil.toInt).max
  //
  //      var winXmax = guessMax
  //      var hasExcessWait = false
  //      var lowerLimitReached = false
  //
  //      if (guessMax <= lowerLimit)
  //        winXmax = lowerLimit
  //      else {
  //        do {
  //          val trialDesks = leftwardDesks(winWork, winXmin, IndexedSeq.fill(winXmin.size)(winXmax), blockSize, backlog)
  //          val trialProcessExcessWait = tryProcessWork(winWork, trialDesks, sla, IndexedSeq(0)) match {
  //            case Success(pw) => pw.excessWait
  //            case Failure(t) => throw t
  //          }
  //          if (trialProcessExcessWait > 0) {
  //            winXmax = List(winXmax + 1, guessMax).min
  //            hasExcessWait = true
  //          }
  //          if (winXmax <= lowerLimit) lowerLimitReached = true
  //          if (!lowerLimitReached && !hasExcessWait) winXmax = winXmax - 1
  //        } while (!lowerLimitReached && !hasExcessWait)
  //      }
  //
  //      val newXmax = acc ++ List.fill(targetWidth)(winXmax)
  //      0 until targetWidth foreach { j =>
  //        backlog = List(backlog + winWork(j) - newXmax(winStart), 0).max
  //      }
  //      newXmax
  //    }.take(work.size)
  //
  //    result
  //  }

  def runningAverage(work: Iterable[Double], windowLength: Int, processors: Processors): Iterable[Int] = {
    val slidingAverages = work
      .sliding(windowLength)
      .map(_.sum / windowLength).toList

    (List.fill(windowLength - 1)(slidingAverages.head) ::: slidingAverages).map(processors.forWorkload)
  }

  def cumulativeSum(values: Iterable[Double]): Iterable[Double] = values
    .foldLeft(List[Double]()) {
      case (Nil, element) => List(element)
      case (head :: tail, element) => element + head :: head :: tail
    }.reverse

  def blockMean(values: Iterable[Int], blockWidth: Int): Iterable[Int] = values
    .grouped(blockWidth)
    .flatMap(nos => List.fill(blockWidth)(nos.sum / blockWidth))
    .toIterable

  def blockMax(values: Iterable[Double], blockWidth: Int): Iterable[Double] = values
    .grouped(blockWidth)
    .flatMap(nos => List.fill(blockWidth)(nos.max))
    .toIterable

  def seqR(from: Int, by: Int, length: Int): IndexedSeq[Int] = 0 to length map (i => (i + from) * by)

  def churn(churnStart: Int, capacity: IndexedSeq[Int]): Int = capacity.zip(churnStart +: capacity)
    .collect { case (x, xLag) => x - xLag }
    .filter(_ > 0)
    .sum

  def cost(work: IndexedSeq[Double],
           sla: Int,
           weightChurn: Double,
           weightPax: Double,
           weightStaff: Double,
           weightSla: Double,
           qStart: IndexedSeq[Double],
           churnStart: Int)
          (capacity: IndexedSeq[Int], processors: Processors): Cost = {
    var simRes = tryProcessWork(work, capacity, sla, qStart, processors) match {
      case Success(pw) => pw
      case Failure(t) => throw t
    }

    var finalCapacity = capacity.takeRight(1).head
    val backlog = simRes.residual.reverse
    val totalBacklog = backlog.sum

    if (backlog.nonEmpty) {
      finalCapacity = List(finalCapacity, 1).max
      val cumBacklog = cumulativeSum(backlog)
      val cumCapacity = seqR(0, finalCapacity, (totalBacklog / finalCapacity).ceil.toInt)
      val overrunSlots = cumCapacity.indices
      val backlogBoundaries = approx(cumCapacity, overrunSlots, cumBacklog.toList)
      val startSlots = 0d :: backlogBoundaries.dropRight(1).map(_.floor)
      val endSlots = backlogBoundaries.map(_.floor)
      val alreadyWaited = (1 to backlog.length).reverse
      val meanWaits = startSlots
        .zip(endSlots)
        .map { case (x, y) => (x + y) / 2 }
        .zip(alreadyWaited)
        .map { case (x, y) => x + y }

      val excessFilter = meanWaits.map(_ > sla)
      val newTotalWait = simRes.totalWait + backlog.zip(meanWaits).map { case (x, y) => x * y }.sum
      val newExcessWait = simRes.excessWait + excessFilter
        .zip(backlog.zip(meanWaits))
        .map {
          case (true, (x, y)) => x * y
          case _ => 0
        }.sum

      simRes = simRes.copy(totalWait = newTotalWait, excessWait = newExcessWait)
    }

    val paxPenalty = simRes.totalWait
    val slaPenalty = simRes.excessWait
    val staffPenalty = simRes.util.zip(capacity).map { case (u, c) => (1 - u) * c.toDouble }.sum
    val churnPenalty = churn(churnStart, capacity :+ finalCapacity)

    val totalPenalty = (weightPax * paxPenalty) +
      (weightStaff * staffPenalty.toDouble) +
      (weightChurn * churnPenalty.toDouble) +
      (weightSla * slaPenalty.toDouble)

    Cost(paxPenalty.toInt, slaPenalty.toInt, staffPenalty, churnPenalty, totalPenalty)
  }

  def neighbouringPoints(x0: Int, xmin: Int, xmax: Int): IndexedSeq[Int] = (xmin to xmax)
    .filterNot(_ == x0)
    .sortBy(x => (x - x0).abs)

  def branchBound(startingX: IndexedSeq[Int],
                  cost: IndexedSeq[Int] => Cost,
                  xmin: IndexedSeq[Int],
                  xmax: IndexedSeq[Int],
                  concavityLimit: Int): Iterable[Int] = {
    val desks = startingX.to[mutable.IndexedSeq]
    var incumbent = startingX
    val minutes = desks.length
    var bestSoFar = cost(incumbent.toIndexedSeq).totalPenalty
    val candidates = (0 until minutes)
      .map(i => neighbouringPoints(startingX(i), xmin(i), xmax(i)))
      .to[mutable.IndexedSeq]

    var cursor = minutes - 1

    while (cursor >= 0) {
      while (candidates(cursor).nonEmpty) {
        desks(cursor) = candidates(cursor).head
        candidates(cursor) = candidates(cursor).drop(1)

        val trialPenalty = cost(desks.toIndexedSeq).totalPenalty

        if (trialPenalty > bestSoFar + concavityLimit) {
          if (desks(cursor) > incumbent(cursor)) {
            candidates(cursor) = candidates(cursor).filter(_ < desks(cursor))
          } else {
            candidates(cursor) = candidates(cursor).filter(_ > desks(cursor))
          }
        } else {
          if (trialPenalty < bestSoFar) {
            incumbent = desks.toIndexedSeq
            bestSoFar = trialPenalty
          }
          if (cursor < minutes - 1) cursor = cursor + 1
        }
      }
      candidates(cursor) = neighbouringPoints(incumbent(cursor), xmin(cursor), xmax(cursor))
      desks(cursor) = incumbent(cursor)
      cursor = cursor - 1
    }
    desks
  }

  def tryOptimiseWin(work: IndexedSeq[Double],
                     minDesks: IndexedSeq[Int],
                     maxDesks: IndexedSeq[Int],
                     sla: Int,
                     weightChurn: Double,
                     weightPax: Double,
                     weightStaff: Double,
                     weightSla: Double,
                     processors: Processors): Try[IndexedSeq[Int]] = {
    if (work.length != minDesks.length) {
      Failure(new Exception(s"work & minDesks are not equal length: ${work.length} vs ${minDesks.length}"))
    } else if (work.length != maxDesks.length) {
      Failure(new Exception(s"work & maxDesks are not equal length: ${work.length} vs ${maxDesks.length}"))
    } else Try {
      val blockWidth = 15
      val concavityLimit = 30
      val winStep = 60
      val smoothingWidth = blockWidth
      val winWidth = List(90, work.length).min

      var winStart = 0
      var winStop = winWidth
      var qStart = IndexedSeq(0d)
      var churnStart = 0

      val desks = blockMean(runningAverage(work, smoothingWidth, processors), blockWidth)
        .zip(maxDesks)
        .map {
          case (d, max) => List(d, max).min
        }
        .zip(minDesks)
        .map {
          case (d, min) => List(d, min).max
        }.to[mutable.IndexedSeq]

      def myCost(costWork: IndexedSeq[Double], costQStart: IndexedSeq[Double], costChurnStart: Int)
                (capacity: IndexedSeq[Int]): Cost =
        cost(costWork, sla, weightChurn, weightPax, weightStaff, weightSla, costQStart, costChurnStart)(capacity.flatMap(c => IndexedSeq.fill(blockWidth)(c)), processors)

      var shouldStop = false

      do {
        val currentWork = work.slice(winStart, winStop)
        val blockGuess = desks.slice(winStart, winStop).grouped(blockWidth).map(_.head).toIndexedSeq
        val xminCondensed = minDesks.slice(winStart, winStop).grouped(blockWidth).map(_.head).toIndexedSeq
        val xmaxCondensed = maxDesks.slice(winStart, winStop).grouped(blockWidth).map(_.head).toIndexedSeq

        val windowIndices = winStart until winStop
        branchBound(blockGuess, myCost(currentWork, qStart, churnStart), xminCondensed, xmaxCondensed, concavityLimit)
          .flatMap(o => List.fill(blockWidth)(o))
          .zip(windowIndices)
          .foreach {
            case (d, i) => desks(i) = d
          }

        shouldStop = winStop == work.length

        if (!shouldStop) {
          val stop = winStart + winStep
          val workToProcess = work.slice(winStart, stop)
          val desksToProcess = desks.slice(winStart, stop)
          qStart = tryProcessWork(workToProcess.toIndexedSeq, desksToProcess.toIndexedSeq, sla, qStart.toIndexedSeq, processors) match {
            case Success(pw) => pw.residual
            case Failure(t) => throw t
          }
          churnStart = desks(stop)
          winStart = winStart + winStep
          winStop = List(winStop + winStep, work.length).min
        }
      } while (!shouldStop)

      desks
    }
  }
}
