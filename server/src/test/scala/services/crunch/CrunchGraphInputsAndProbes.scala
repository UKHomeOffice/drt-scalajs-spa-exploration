package services.crunch

import akka.actor.ActorRef
import akka.stream.scaladsl.SourceQueueWithComplete
import akka.testkit.TestProbe
import drt.shared.CrunchApi.ActualDeskStats
import drt.shared.{FixedPointAssignments, ShiftAssignments, StaffMovement}
import server.feeds.{ArrivalsFeedResponse, ManifestsFeedResponse}

case class CrunchGraphInputsAndProbes(baseArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      forecastArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      liveArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      manifestsLiveInput: SourceQueueWithComplete[ManifestsFeedResponse],
                                      shiftsInput: SourceQueueWithComplete[ShiftAssignments],
                                      fixedPointsInput: SourceQueueWithComplete[FixedPointAssignments],
                                      liveStaffMovementsInput: SourceQueueWithComplete[Seq[StaffMovement]],
                                      forecastStaffMovementsInput: SourceQueueWithComplete[Seq[StaffMovement]],
                                      actualDesksAndQueuesInput: SourceQueueWithComplete[ActualDeskStats],
                                      portStateTestProbe: TestProbe,
                                      baseArrivalsTestProbe: TestProbe,
                                      forecastArrivalsTestProbe: TestProbe,
                                      liveArrivalsTestProbe: TestProbe,
                                      aggregatedArrivalsActor: ActorRef,
                                      portStateActor: ActorRef) {
  def shutdown(): Unit = {
    baseArrivalsInput.complete()
    forecastArrivalsInput.complete()
    liveArrivalsInput.complete()
    manifestsLiveInput.complete()
    shiftsInput.complete()
    fixedPointsInput.complete()
    liveStaffMovementsInput.complete()
    forecastStaffMovementsInput.complete()
    actualDesksAndQueuesInput.complete()
  }
}