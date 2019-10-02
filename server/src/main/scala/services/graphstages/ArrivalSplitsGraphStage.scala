package services.graphstages

import akka.stream._
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared._
import manifests.passengers.BestAvailableManifest
import manifests.queues.SplitsCalculator
import org.slf4j.{Logger, LoggerFactory}
import server.feeds._
import services._
import services.graphstages.Crunch.purgeExpired

import scala.collection.immutable.Map
import scala.collection.mutable

case class UpdateStats(updatesCount: Int, additionsCount: Int)


class ArrivalSplitsGraphStage(name: String = "",
                              portCode: String,
                              optionalInitialFlights: Option[FlightsWithSplits],
                              splitsCalculator: SplitsCalculator, //keep this for now, we'll need to move this into it's own graph stage later..
                              groupFlightsByCodeShares: Seq[ApiFlightWithSplits] => Seq[(ApiFlightWithSplits, Set[Arrival])],
                              expireAfterMillis: Long,
                              now: () => SDateLike)
  extends GraphStage[FanInShape3[ArrivalsDiff, ManifestsFeedResponse, ManifestsFeedResponse, FlightsWithSplits]] {

  val log: Logger = LoggerFactory.getLogger(s"$getClass-$name")

  val inArrivalsDiff: Inlet[ArrivalsDiff] = Inlet[ArrivalsDiff]("ArrivalsDiffIn.in")
  val inManifestsLive: Inlet[ManifestsFeedResponse] = Inlet[ManifestsFeedResponse]("ManifestsLiveIn.in")
  val inManifestsHistoric: Inlet[ManifestsFeedResponse] = Inlet[ManifestsFeedResponse]("ManifestsHistoricIn.in")
  val outArrivalsWithSplits: Outlet[FlightsWithSplits] = Outlet[FlightsWithSplits]("ArrivalsWithSplitsOut.out")

  override val shape = new FanInShape3(inArrivalsDiff, inManifestsLive, inManifestsHistoric, outArrivalsWithSplits)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    val flightsByFlightId: mutable.SortedMap[ArrivalKey, ApiFlightWithSplits] = mutable.SortedMap()
    val codeShares: mutable.SortedMap[CodeShareKey, Set[ArrivalKey]] = mutable.SortedMap()
    var arrivalsWithSplitsDiff: Map[ArrivalKey, ApiFlightWithSplits] = Map()
    var arrivalsToRemove: Set[Arrival] = Set()
    val manifestBuffer: mutable.Map[ArrivalKey, BestAvailableManifest] = mutable.Map()

    override def preStart(): Unit = {

      optionalInitialFlights match {
        case Some(FlightsWithSplits(flights, _)) =>
          log.info(s"Received initial flights. Setting ${flights.size}")
          flights.foreach(fws => flightsByFlightId += (ArrivalKey(fws.apiFlight) -> fws))
          purgeExpired(flightsByFlightId, ArrivalKey.atTime, now, expireAfterMillis.toInt)

          flightsByFlightId.foreach { case (arrivalKey, fws) =>
            val csKey = CodeShareKey(fws.apiFlight.Scheduled, fws.apiFlight.Terminal, fws.apiFlight.Origin, Set())
            val existingEntry: Set[ArrivalKey] = codeShares.getOrElse(csKey, Set())
            val updatedArrivalKeys = existingEntry + arrivalKey
            codeShares += (csKey.copy(arrivalKeys = updatedArrivalKeys) -> updatedArrivalKeys)
          }

        case _ =>
          log.warn(s"Did not receive any flights to initialise with")
      }

      super.preStart()
    }

    setHandler(outArrivalsWithSplits, new OutHandler {
      override def onPull(): Unit = {
        val start = SDate.now()
        log.debug(s"arrivalsWithSplitsOut onPull called")
        pushStateIfReady()
        pullAll()
        log.info(s"outArrivalsWithSplits Took ${SDate.now().millisSinceEpoch - start.millisSinceEpoch}ms")
      }
    })

    setHandler(inArrivalsDiff, new InHandler {
      override def onPush(): Unit = {
        val start = SDate.now()
        log.debug(s"inFlights onPush called")
        val arrivalsDiff = grab(inArrivalsDiff)

        log.info(s"Grabbed ${arrivalsDiff.toUpdate.size} updates, ${arrivalsDiff.toRemove.size} removals")

        val flightsWithUpdates = applyUpdatesToFlights(arrivalsDiff)

        updateCodeSharesFromDiff(arrivalsDiff)
        updateFlightsFromIncoming(arrivalsDiff)

        purgeExpired(flightsByFlightId, ArrivalKey.atTime, now, expireAfterMillis.toInt)

        val uniqueFlightsWithUpdates = flightsWithUpdates.filterKeys(flightsByFlightId.contains)

        arrivalsWithSplitsDiff = mergeDiffSets(uniqueFlightsWithUpdates, arrivalsWithSplitsDiff)
        arrivalsToRemove = arrivalsToRemove ++ arrivalsDiff.toRemove
        log.info(s"${arrivalsWithSplitsDiff.size} updated arrivals waiting to push")

        pushStateIfReady()
        pullAll()
        log.info(s"inArrivalsDiff Took ${SDate.now().millisSinceEpoch - start.millisSinceEpoch}ms")
      }
    })

    def applyUpdatesToFlights(arrivalsDiff: ArrivalsDiff): Map[ArrivalKey, ApiFlightWithSplits] = {
      val flightsWithUpdates = arrivalsDiff.toUpdate.foldLeft(Map[ArrivalKey, ApiFlightWithSplits]()) {
        case (withUpdatesSoFar, (_, newArrival)) =>
          val arrivalKey = ArrivalKey(newArrival)
          flightsByFlightId.get(arrivalKey) match {
          case None =>
            val splits: Set[Splits] = initialSplits(newArrival, arrivalKey)
            val newFlightWithSplits: ApiFlightWithSplits = ApiFlightWithSplits(newArrival, splits, nowMillis)
            withUpdatesSoFar.updated(arrivalKey, newFlightWithSplits)
          case Some(existingArrival) =>
            if (!existingArrival.apiFlight.equals(newArrival))
              withUpdatesSoFar.updated(arrivalKey, existingArrival.copy(apiFlight = newArrival, lastUpdated = nowMillis))
            else withUpdatesSoFar
        }
      }
      flightsWithUpdates
    }

    setHandler(inManifestsLive, InManifestsHandler(inManifestsLive))

    setHandler(inManifestsHistoric, InManifestsHandler(inManifestsHistoric))

    def InManifestsHandler(inlet: Inlet[ManifestsFeedResponse]): InHandler =
      new InHandler() {
        override def onPush(): Unit = {
          val start = SDate.now()
          log.info(s"inSplits onPush called")

          val incoming: ManifestsFeedResponse = grab(inlet) match {
            case ManifestsFeedSuccess(DqManifests(_, manifests), createdAt) => BestManifestsFeedSuccess(manifests.toSeq.map(vm => BestAvailableManifest(vm)), createdAt)
            case other => other
          }

          incoming match {
            case BestManifestsFeedSuccess(bestAvailableManifests, connectedAt) =>
              log.info(s"Grabbed ${bestAvailableManifests.size} BestAvailableManifests from connection at ${connectedAt.toISOString()}")

              val flightsWithUpdates = updateFlightsWithManifests(bestAvailableManifests)
              log.info(s"We now have ${flightsByFlightId.size} flights")

              arrivalsWithSplitsDiff = mergeDiffSets(flightsWithUpdates, arrivalsWithSplitsDiff)
              purgeExpired(flightsByFlightId, ArrivalKey.atTime, now, expireAfterMillis.toInt)
              log.info(s"Done diff")

              pushStateIfReady()

            case unexpected => log.error(s"Unexpected feed response: ${unexpected.getClass}")
          }
          pullAll()
          log.info(s"inManifests Took ${SDate.now().millisSinceEpoch - start.millisSinceEpoch}ms")
        }
      }

    def pullAll(): Unit = {
      List(inManifestsLive, inManifestsHistoric, inArrivalsDiff).foreach(in => if (!hasBeenPulled(in)) {
        log.debug(s"Pulling ${in.toString}")
        pull(in)
      })
    }

    def updateFlightsFromIncoming(arrivalsDiff: ArrivalsDiff): Unit = {
      log.info(s"${arrivalsDiff.toUpdate.size} diff updates, ${flightsByFlightId.size} existing flights")

      flightsByFlightId --= arrivalsDiff.toRemove.map(ArrivalKey(_))

      val updateStats = arrivalsDiff.toUpdate.foldLeft(UpdateStats(0, 0)) {
        case (statsSoFar, (_, updatedFlight)) => updateWithFlight(statsSoFar, updatedFlight)
      }

      log.info(s"${flightsByFlightId.size} flights after updates. ${updateStats.updatesCount} updates & ${updateStats.additionsCount} additions")

      val codeSharesToRemove = codeShares.foldLeft(Set[ArrivalKey]()) {
        case (removalsSoFar, (_, codeShareArrivalKeys)) =>
          val shares = codeShareArrivalKeys
            .map(arrivalKey => flightsByFlightId.get(arrivalKey))
            .collect { case Some(fws) => fws }
            .toSeq.sortBy(_.apiFlight.ActPax.getOrElse(0)).reverse
          val toRemove = shares.drop(1)
          val keysToRemove = toRemove.map(fws => ArrivalKey(fws.apiFlight))
          removalsSoFar ++ keysToRemove
      }

      flightsByFlightId --= codeSharesToRemove

      log.info(s"${flightsByFlightId.size} flights after accounting for codeshares")
    }

    def updateWithFlight(updatedFlights: UpdateStats, updatedFlight: Arrival): UpdateStats = {
      val key = ArrivalKey(updatedFlight)
      flightsByFlightId.get(key) match {
        case None =>
          val splits: Set[Splits] = initialSplits(updatedFlight, key)
          val newFlightWithSplits: ApiFlightWithSplits = ApiFlightWithSplits(updatedFlight, splits, nowMillis)
          flightsByFlightId += (key -> newFlightWithSplits.copy(lastUpdated = nowMillis))
          updatedFlights.copy(additionsCount = updatedFlights.additionsCount + 1)

        case Some(existingFlight) if existingFlight.apiFlight != updatedFlight =>
          flightsByFlightId += (key -> existingFlight.copy(apiFlight = updatedFlight, lastUpdated = nowMillis))
          updatedFlights.copy(updatesCount = updatedFlights.updatesCount + 1)

        case _ => updatedFlights
      }
    }

    def initialSplits(updatedFlight: Arrival, key: ArrivalKey): Set[Splits] =
      if (manifestBuffer.contains(key)) {
        val splits = splitsCalculator.portDefaultSplits + splitsFromManifest(updatedFlight, manifestBuffer(key))
        manifestBuffer -= key
        splits
      }
      else splitsCalculator.portDefaultSplits

    def updateFlightsWithManifests(manifests: Seq[BestAvailableManifest]): Map[ArrivalKey, ApiFlightWithSplits] = {
      manifests.foldLeft(Map[ArrivalKey, ApiFlightWithSplits]()) {
        case (flightsWithNewSplits, newManifest) =>
          val key = ArrivalKey(newManifest.departurePortCode, newManifest.voyageNumber, newManifest.scheduled.millisSinceEpoch)
          flightsByFlightId.get(key) match {
            case Some(flightForManifest) =>
              val manifestSplits: Splits = splitsFromManifest(flightForManifest.apiFlight, newManifest)

              if (isNewManifestForFlight(flightForManifest, manifestSplits)) {
                val flightWithManifestSplits = updateFlightWithSplits(flightForManifest, manifestSplits)
                flightsByFlightId += (key -> flightWithManifestSplits)
                flightsWithNewSplits.updated(key, flightWithManifestSplits)
              } else flightsWithNewSplits
            case None =>
              manifestBuffer += (key -> newManifest)
              flightsWithNewSplits
          }
      }
    }

    def pushStateIfReady(): Unit = {
      if (isAvailable(outArrivalsWithSplits)) {
        if (arrivalsWithSplitsDiff.nonEmpty || arrivalsToRemove.nonEmpty) {
          log.info(s"Pushing ${arrivalsWithSplitsDiff.size} updated arrivals with splits and ${arrivalsToRemove.size} removals")
          push(outArrivalsWithSplits, FlightsWithSplits(arrivalsWithSplitsDiff.values.toSeq, arrivalsToRemove.toSeq))
          arrivalsWithSplitsDiff = Map()
          arrivalsToRemove = Set()
        } else log.debug(s"No updated arrivals with splits to push")
      } else log.debug(s"outArrivalsWithSplits not available to push")
    }

    def isNewManifestForFlight(flightWithSplits: ApiFlightWithSplits, newSplits: Splits): Boolean = !flightWithSplits.splits.contains(newSplits)

    def updateFlightWithSplits(flightWithSplits: ApiFlightWithSplits,
                               newSplits: Splits): ApiFlightWithSplits = {
      val apiFlight = flightWithSplits.apiFlight
      flightWithSplits.copy(
        apiFlight = apiFlight.copy(FeedSources = apiFlight.FeedSources + ApiFeedSource),
        splits = flightWithSplits.splits.filterNot(_.source == newSplits.source) ++ Set(newSplits)
      )
    }

    def updateCodeSharesFromDiff(arrivalsDiff: ArrivalsDiff): Unit = arrivalsDiff.toUpdate
      .foreach { case (_, arrival) =>
        val csKey = CodeShareKey(arrival.Scheduled, arrival.Terminal, arrival.Origin, Set())
        val existingEntry: Set[ArrivalKey] = codeShares.getOrElse(csKey, Set())
        val updatedArrivalKeys = existingEntry + ArrivalKey(arrival)
        codeShares += (csKey.copy(arrivalKeys = updatedArrivalKeys) -> updatedArrivalKeys)
      }
  }

  def splitsFromManifest(arrival: Arrival, manifest: BestAvailableManifest): Splits = {
    splitsCalculator.bestSplitsForArrival(manifest.copy(carrierCode = arrival.carrierCode), arrival)
  }

  def nowMillis: Option[MillisSinceEpoch] = {
    Option(now().millisSinceEpoch)
  }

  def mergeDiffSets(latestDiff: Map[ArrivalKey, ApiFlightWithSplits],
                    existingDiff: Map[ArrivalKey, ApiFlightWithSplits]
                   ): Map[ArrivalKey, ApiFlightWithSplits] = latestDiff
    .foldLeft(existingDiff) {
      case (diffSoFar, (newArrivalKey, newFws)) => diffSoFar.updated(newArrivalKey, newFws)
    }
}

