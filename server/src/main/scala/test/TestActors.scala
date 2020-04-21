package test

import actors.Sizes.oneMegaByte
import actors._
import actors.acking.AckingReceiver.Ack
import akka.actor.{ActorRef, Props}
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared.{PortCode, SDateLike}
import slickdb.ArrivalTable


object TestActors {

  case object ResetData

  class TestForecastBaseArrivalsActor(override val now: () => SDateLike, expireAfterMillis: Int)
    extends ForecastBaseArrivalsActor(oneMegaByte, now, expireAfterMillis) {

    def reset: Receive = {
      case ResetData =>
        state.clear()
        sender() ! Ack
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  class TestForecastPortArrivalsActor(override val now: () => SDateLike, expireAfterMillis: Int)
    extends ForecastPortArrivalsActor(oneMegaByte, now, expireAfterMillis) {

    def reset: Receive = {
      case ResetData =>
        state.clear()
        sender() ! Ack
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  class TestLiveArrivalsActor(override val now: () => SDateLike, expireAfterMillis: Int)
    extends LiveArrivalsActor(oneMegaByte, now, expireAfterMillis) {

    def reset: Receive = {
      case ResetData =>
        state.clear()
        sender() ! Ack
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  class TestVoyageManifestsActor(override val now: () => SDateLike, expireAfterMillis: Int, snapshotInterval: Int)
    extends VoyageManifestsActor(oneMegaByte, now, expireAfterMillis, Option(snapshotInterval)) {

    def reset: Receive = {
      case ResetData =>
        state = initialState
        sender() ! Ack
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  class TestShiftsActor(override val now: () => SDateLike,
                        override val expireBefore: () => SDateLike) extends ShiftsActor(now, expireBefore) {

    def reset: Receive = {
      case ResetData =>
        state = initialState
        subscribers = List()
        sender() ! Ack
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  class TestFixedPointsActor(override val now: () => SDateLike) extends FixedPointsActor(now) {

    def reset: Receive = {
      case ResetData =>
        state = initialState
        subscribers = List()
        sender() ! Ack
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  class TestStaffMovementsActor(override val now: () => SDateLike,
                                override val expireBefore: () => SDateLike) extends StaffMovementsActor(now, expireBefore) {

    def reset: Receive = {
      case ResetData =>
        state = initialState
        subscribers = List()
        sender() ! Ack
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  class TestAggregatedArrivalsActor() extends {
    private val portCode = PortCode("LHR")
  } with AggregatedArrivalsActor(ArrivalTable(portCode, PostgresTables)) {
    def reset: Receive = {
      case ResetData =>
        sender() ! Ack
    }

    override def receive: Receive = reset orElse super.receive
  }

  class TestPortStateActor(live: ActorRef, forecast: ActorRef, now: () => SDateLike, liveDaysAhead: Int)
    extends PortStateActor(live, forecast, now, liveDaysAhead) {
    def reset: Receive = {
      case ResetData =>
        state.clear()
        sender() ! Ack
    }

    override def receive: Receive = reset orElse super.receive
  }

  class TestCrunchStateActor(snapshotInterval: Int,
                             name: String,
                             portQueues: Map[Terminal, Seq[Queue]],
                             override val now: () => SDateLike,
                             expireAfterMillis: Int,
                             purgePreviousSnapshots: Boolean)
    extends CrunchStateActor(
      initialMaybeSnapshotInterval = None,
      initialSnapshotBytesThreshold = oneMegaByte,
      name = name,
      portQueues = portQueues,
      now = now,
      expireAfterMillis = expireAfterMillis,
      purgePreviousSnapshots = purgePreviousSnapshots,
      forecastMaxMillis = () => now().addDays(2).millisSinceEpoch) {

    def reset: Receive = {
      case ResetData =>
        state = initialState
        sender() ! Ack
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  def logMessage(m: Any): String = s"Got this message: ${m.getClass} but not doing anything because this is a test."
}
