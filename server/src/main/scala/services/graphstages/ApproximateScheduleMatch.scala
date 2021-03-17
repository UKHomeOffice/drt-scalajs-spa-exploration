package services.graphstages

import drt.shared.api.Arrival
import drt.shared.{MilliTimes, PortCode, UniqueArrivalWithOrigin}
import org.slf4j.{Logger, LoggerFactory}
import services.arrivals.LiveArrivalsUtil


object ApproximateScheduleMatch {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def findInScheduledWindow(uniqueArrival: UniqueArrivalWithOrigin,
                            origin: PortCode,
                            arrivalsToSearch: Map[UniqueArrivalWithOrigin, Arrival],
                            windowMillis: Int): Either[Arrival, List[Arrival]] = {
    val approxMatches = arrivalsToSearch
      .filterKeys(_.equalWithinScheduledWindow(uniqueArrival, windowMillis))
      .filter {
        case (_, potentialArrival) =>
          val originMatches = potentialArrival.Origin == origin
          if (!originMatches) log.warn(s"Origin mismatch for $uniqueArrival approximate match ${potentialArrival.unique} ($origin != ${potentialArrival.Origin})")
          originMatches
      }

    approxMatches.values.toList match {
      case singleMatch :: Nil => Left(singleMatch)
      case nonSingleMatch => Right(nonSingleMatch)
    }
  }

  def mergeApproxIfFoundElseNone(arrival: Arrival,
                                 origin: PortCode,
                                 sourcesToSearch: List[(ArrivalsSourceType, Map[UniqueArrivalWithOrigin, Arrival])]): Option[Arrival] =
    sourcesToSearch.foldLeft[Option[Arrival]](None) {
      case (Some(found), _) => Some(found)
      case (None, (sourceToSearch, arrivalsForSource)) => maybeMergeApproxMatch(arrival, origin, sourceToSearch, arrivalsForSource)
    }

  def mergeApproxIfFoundElseOriginal(arrival: Arrival,
                                     origin: PortCode,
                                     sourcesToSearch: List[(ArrivalsSourceType, Map[UniqueArrivalWithOrigin, Arrival])]): Option[Arrival] =
    mergeApproxIfFoundElseNone(arrival, origin, sourcesToSearch) match {
      case Some(found) => Some(found)
      case None => Some(arrival)
    }

  def maybeMergeApproxMatch(arrival: Arrival,
                            origin: PortCode,
                            searchSource: ArrivalsSourceType,
                            arrivalsToSearch: Map[UniqueArrivalWithOrigin, Arrival]): Option[Arrival] = {
    val key = arrival.unique
    findInScheduledWindow(key, origin, arrivalsToSearch, MilliTimes.oneHourMillis) match {
      case Left(matchedArrival) =>
        log.info(s"Merging approximate schedule match ${matchedArrival.unique} with ${arrival.unique}")
        if (searchSource == LiveBaseArrivals)
          Option(LiveArrivalsUtil.mergePortFeedWithLiveBase(arrival, matchedArrival))
        else
          Option(LiveArrivalsUtil.mergePortFeedWithLiveBase(matchedArrival, arrival))
      case Right(none) if none.isEmpty =>
        log.warn(s"No approximate schedule $searchSource matches found for arrival $key")
        None
      case Right(_) =>
        log.warn(s"Multiple approximate schedule $searchSource matches found for arrival $key")
        None
    }
  }
}
