package actors.queues

import actors.StreamingJournalLike
import drt.shared.SDateLike


class CrunchQueueActor(now: () => SDateLike, journalType: StreamingJournalLike, crunchOffsetMinutes: Int) extends QueueLikeActor(now, journalType, crunchOffsetMinutes) {
  override val persistenceId: String = "crunch-queue"
}
