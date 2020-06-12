package test

import actors.MinuteLookupsLike
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.Queues.Queue
import drt.shared.SDateLike
import drt.shared.Terminals.Terminal
import services.SDate
import test.TestActors.{ResetData, TestQueueMinutesActor, TestStaffMinutesActor, TestTerminalDayQueuesActor, TestTerminalDayStaffActor}

import scala.concurrent.{ExecutionContext, Future}

case class TestMinuteLookups(system: ActorSystem,
                             now: () => SDateLike,
                             expireAfterMillis: Int,
                             queuesByTerminal: Map[Terminal, Seq[Queue]])
                            (implicit val ec: ExecutionContext) extends MinuteLookupsLike {
  val resetQueuesData: (Terminal, MillisSinceEpoch) => Future[Any] = (terminal: Terminal, millis: MillisSinceEpoch) => {
    val date = SDate(millis)
    val actor = system.actorOf(Props(new TestTerminalDayQueuesActor(date.getFullYear(), date.getMonth(), date.getDate(), terminal, now)))
    actor.ask(ResetData).map(_ => actor ! PoisonPill)
  }

  val resetStaffData: (Terminal, MillisSinceEpoch) => Future[Any] = (terminal: Terminal, millis: MillisSinceEpoch) => {
    val date = SDate(millis)
    val actor = system.actorOf(Props(new TestTerminalDayStaffActor(date.getFullYear(), date.getMonth(), date.getDate(), terminal, now)))
    actor.ask(ResetData).map(_ => actor ! PoisonPill)
  }

  override val queueMinutesActor: ActorRef = system.actorOf(Props(new TestQueueMinutesActor(now, queuesByTerminal.keys, primaryQueuesLookup, secondaryQueuesLookup, updateCrunchMinutes, resetQueuesData)))

  override val staffMinutesActor: ActorRef = system.actorOf(Props(new TestStaffMinutesActor(now, queuesByTerminal.keys, primaryStaffLookup, secondaryStaffLookup, updateStaffMinutes, resetStaffData)))
}