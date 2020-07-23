package manifests.graph

import akka.actor.{ActorSystem, Cancellable}
import akka.stream._
import akka.stream.stage._
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.api.Arrival
import drt.shared.{ArrivalKey, SDateLike}
import manifests.actors.RegisteredArrivals
import org.slf4j.{Logger, LoggerFactory}
import services.graphstages.Crunch
import services.metrics.{Metrics, StageTimer}

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

class BatchStage(now: () => SDateLike,
                 isDueLookup: (MillisSinceEpoch, MillisSinceEpoch, SDateLike) => Boolean,
                 batchSize: Int,
                 expireAfterMillis: Int,
                 maybeInitialState: Option[RegisteredArrivals],
                 sleepMillisOnEmptyPush: Long,
                 lookupRefreshDue: MillisSinceEpoch => Boolean)(implicit actorSystem: ActorSystem, executionContext: ExecutionContext)
  extends GraphStage[FanOutShape2[List[Arrival], List[ArrivalKey], RegisteredArrivals]] {
  val inArrivals: Inlet[List[Arrival]] = Inlet[List[Arrival]]("inArrivals.in")
  val outArrivals: Outlet[List[ArrivalKey]] = Outlet[List[ArrivalKey]]("outArrivals.out")
  val outRegisteredArrivals: Outlet[RegisteredArrivals] = Outlet[RegisteredArrivals]("outRegisteredArrivals.out")
  val stageName = "batch-manifest-requests"

  override def shape = new FanOutShape2(inArrivals, outArrivals, outRegisteredArrivals)

  val log: Logger = LoggerFactory.getLogger(getClass)
  var maybeCancellable: Option[Cancellable] = None

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    var registeredArrivals: SortedMap[ArrivalKey, Option[Long]] = SortedMap()
    var registeredArrivalsUpdates: SortedMap[ArrivalKey, Option[Long]] = SortedMap()
    val lookupQueue: mutable.SortedSet[ArrivalKey] = mutable.SortedSet()
    var lastLookupRefresh = 0L

    override def preStart(): Unit = {
      if (maybeInitialState.isEmpty) log.warn(s"Did not receive any initial registered arrivals")
      maybeInitialState.foreach { state =>
        log.info(s"Received ${state.arrivals.size} initial registered arrivals")
        registeredArrivals = registeredArrivals ++ state.arrivals

        refreshLookupQueue(now())

        log.info(s"${registeredArrivals.size} registered arrivals. ${lookupQueue.size} arrivals in lookup queue")
      }
    }

    override def postStop(): Unit = maybeCancellable.foreach { cancellable =>
      log.warn(s"Cancelling cancellable in postStop()")
      cancellable.cancel
    }

    setHandler(inArrivals, new InHandler {
      override def onPush(): Unit = {
        val timer = StageTimer(stageName, inArrivals)
        val incoming = grab(inArrivals)

        log.info(s"grabbed ${incoming.length} requests for arrival manifests")

        val (registeredArrivals_, registeredArrivalsUpdates_) = BatchStage.registerNewArrivals(incoming, registeredArrivals, registeredArrivalsUpdates)
        registeredArrivals = registeredArrivals_
        registeredArrivalsUpdates = registeredArrivalsUpdates_

        Metrics.counter("manifests.registered-arrivals", registeredArrivals.size)

        if (isAvailable(outArrivals)) prioritiseAndPush()
        if (isAvailable(outRegisteredArrivals)) pushRegisteredArrivalsUpdates()

        pullIfAvailable()
        timer.stopAndReport()
      }
    })

    setHandler(outArrivals, new OutHandler {
      override def onPull(): Unit = {
        val timer = StageTimer(stageName, outArrivals)
        prioritiseAndPush()

        pullIfAvailable()
        timer.stopAndReport()
      }
    })

    setHandler(outRegisteredArrivals, new OutHandler {
      override def onPull(): Unit = {
        val timer = StageTimer(stageName, outRegisteredArrivals)
        pushRegisteredArrivalsUpdates()

        pullIfAvailable()
        timer.stopAndReport()
      }
    })

    private def pullIfAvailable(): Unit = {
      if (!hasBeenPulled(inArrivals)) pull(inArrivals)
    }

    private def prioritiseAndPush(): Unit = {
      Crunch.purgeExpired(lookupQueue, ArrivalKey.atTime, now, expireAfterMillis.toInt)
      Crunch.purgeExpired(registeredArrivals, ArrivalKey.atTime, now, expireAfterMillis.toInt)

      val lookupBatch = updatePrioritisedAndSubscribers()

      if (lookupBatch.nonEmpty) {
        Metrics.counter(s"$stageName", lookupBatch.size)
        push(outArrivals, lookupBatch.toList)
      } else if (maybeCancellable.isEmpty) {
        object PushAfterDelay extends Runnable {
          override def run(): Unit = if (!isClosed(outArrivals) && isAvailable(outArrivals)) {
            log.info(s"Pushing empty list after delay of ${sleepMillisOnEmptyPush}ms")
            push(outArrivals, List())
          }
        }
        maybeCancellable = Option(actorSystem.scheduler.scheduleOnce(sleepMillisOnEmptyPush milliseconds, PushAfterDelay))
      }
    }

    private def pushRegisteredArrivalsUpdates(): Unit = if (registeredArrivalsUpdates.nonEmpty) {
      Metrics.counter("manifests.lookup-requests", registeredArrivalsUpdates.size)
      push(outRegisteredArrivals, RegisteredArrivals(registeredArrivalsUpdates))
      registeredArrivalsUpdates = SortedMap()
    }

    private def updatePrioritisedAndSubscribers(): Set[ArrivalKey] = {
      if (lookupRefreshDue(lastLookupRefresh)) {
        refreshLookupQueue(now())
        lastLookupRefresh = now().millisSinceEpoch
      }

      val nextLookupBatch = lookupQueue.take(batchSize)

      lookupQueue --= nextLookupBatch

      val lookupTime: MillisSinceEpoch = now().millisSinceEpoch

      val updatedLookupTimes = nextLookupBatch.map { arrivalForLookup =>
        (arrivalForLookup, Option(lookupTime))
      }

      registeredArrivals ++= updatedLookupTimes
      registeredArrivalsUpdates ++= updatedLookupTimes

      nextLookupBatch.toSet
    }

    private def refreshLookupQueue(currentNow: SDateLike): Unit = registeredArrivals.foreach {
      case (arrival, None) =>
        lookupQueue += arrival
      case (arrival, Some(lastLookup)) =>
        val dueLookup = isDueLookup(arrival.scheduled, lastLookup, currentNow)
        val notAlreadyInQueue = !lookupQueue.contains(arrival)

        if (notAlreadyInQueue && dueLookup) {
          lookupQueue += arrival
        }
    }
  }
}

object BatchStage {
  def registerNewArrivals(incoming: List[Arrival],
                          arrivalsRegistry: SortedMap[ArrivalKey, Option[MillisSinceEpoch]],
                          updatesRegistry: SortedMap[ArrivalKey, Option[MillisSinceEpoch]]): (SortedMap[ArrivalKey, Option[MillisSinceEpoch]], SortedMap[ArrivalKey, Option[MillisSinceEpoch]]) = {
    val unregisteredArrivals = BatchStage.arrivalsToRegister(incoming, arrivalsRegistry)
    (arrivalsRegistry ++ unregisteredArrivals, updatesRegistry ++ unregisteredArrivals)
  }

  def arrivalsToRegister(arrivalsToConsider: List[Arrival], arrivalsRegistry: SortedMap[ArrivalKey, Option[MillisSinceEpoch]]): List[(ArrivalKey, Option[Long])] = arrivalsToConsider
    .map(a => (ArrivalKey(a), None))
    .filterNot { case (k, _) => arrivalsRegistry.contains(k) }
}
