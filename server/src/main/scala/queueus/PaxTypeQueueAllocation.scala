package queueus

import drt.shared.PassengerSplits.QueueType
import drt.shared._
import manifests.passengers.{BestAvailableManifest, ManifestPassengerProfile}

case class PaxTypeQueueAllocation(paxTypeAllocator: PaxTypeAllocator, queueAllocator: QueueAllocator) {

  def toQueues(bestManifest: BestAvailableManifest): Map[QueueType, List[(QueueType, PaxType, ManifestPassengerProfile, Double)]] = {
    val queueAllocatorForFlight = queueAllocator(bestManifest) _
    val paxTypeAllocatorForFlight = paxTypeAllocator(bestManifest) _
    bestManifest.passengerList.flatMap(mpp => {
      val paxType = paxTypeAllocatorForFlight(mpp)
      val queueAllocations = queueAllocatorForFlight(paxType)
      queueAllocations.map {
        case (queue, allocation) => (queue, paxType, mpp, allocation)
      }
    })
  }.groupBy {
    case (queueType, _, _, _) => queueType
  }

  def toSplits(bestManifest: BestAvailableManifest): Splits = {
    val splits = toQueues(bestManifest).flatMap {
      case (_, passengerProfileTypeByQueueCount) =>
        passengerProfileTypeByQueueCount.foldLeft(Map[PaxTypeAndQueue, ApiPaxTypeAndQueueCount]())(
          (
            soFar: Map[PaxTypeAndQueue, ApiPaxTypeAndQueueCount],
            next: (QueueType, PaxType, ManifestPassengerProfile, Double)
          ) => {

            val (queue: QueueType, paxType: PaxType, mpp: ManifestPassengerProfile, paxCount: Double) = next

            val paxTypeAndQueue = PaxTypeAndQueue(paxType, queue)
            val ptqc: ApiPaxTypeAndQueueCount = soFar.get(paxTypeAndQueue) match {
              case Some(apiPaxTypeAndQueueCount) => apiPaxTypeAndQueueCount.copy(
                paxCount = apiPaxTypeAndQueueCount.paxCount + paxCount,
                nationalities = apiPaxTypeAndQueueCount.nationalities.map(nats => {
                  val existingOfNationality: Double = nats.getOrElse(mpp.nationality, 0)
                  val newNats: Map[String, Double] = nats + (mpp.nationality -> (existingOfNationality + paxCount))
                  Some(newNats)
                }).getOrElse(Some(Map(mpp.nationality -> paxCount)))
              )

              case None => ApiPaxTypeAndQueueCount(paxType, queue, paxCount, Some(Map(mpp.nationality -> paxCount)))
            }
            soFar + (paxTypeAndQueue -> ptqc)
          }
        )
    }.values.toSet

    Splits(splits, bestManifest.source, None, Ratio)
  }
}
