package actors

import actors.daily.{RequestAndTerminate, RequestAndTerminateActor, TerminalDayQueuesActor, TerminalDayStaffActor}
import actors.pointInTime.{CrunchStateReadActor, GetCrunchMinutes, GetStaffMinutes}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import drt.shared.CrunchApi.{CrunchMinute, MinutesContainer, StaffMinute}
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared.{SDateLike, TM, TQM}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

trait MinuteLookupsLike {
  val system: ActorSystem
  implicit val ec: ExecutionContext
  implicit val timeout: Timeout = new Timeout(15 seconds)

  val now: () => SDateLike
  val expireAfterMillis: Int
  val queuesByTerminal: Map[Terminal, Seq[Queue]]
  val requestAndTerminateActor: ActorRef = system.actorOf(Props(new RequestAndTerminateActor()))

  val updateCrunchMinutes: (Terminal, SDateLike, MinutesContainer[CrunchMinute, TQM]) => Future[MinutesContainer[CrunchMinute, TQM]] = (terminal: Terminal, date: SDateLike, container: MinutesContainer[CrunchMinute, TQM]) => {
    val actor = system.actorOf(TerminalDayQueuesActor.props(terminal, date, now))
    requestAndTerminateActor.ask(RequestAndTerminate(actor, container)).mapTo[MinutesContainer[CrunchMinute, TQM]]
  }

  val updateStaffMinutes: (Terminal, SDateLike, MinutesContainer[StaffMinute, TM]) => Future[MinutesContainer[StaffMinute, TM]] = (terminal: Terminal, date: SDateLike, container: MinutesContainer[StaffMinute, TM]) => {
    val actor = system.actorOf(TerminalDayStaffActor.props(terminal, date, now))
    requestAndTerminateActor.ask(RequestAndTerminate(actor, container)).mapTo[MinutesContainer[StaffMinute, TM]]
  }

  val primaryQueuesLookup: (Terminal, SDateLike) => Future[Option[MinutesContainer[CrunchMinute, TQM]]] = (terminal: Terminal, date: SDateLike) => {
    val actor = system.actorOf(TerminalDayQueuesActor.props(terminal, date, now))
    requestAndTerminateActor.ask(RequestAndTerminate(actor, GetState)).mapTo[Option[MinutesContainer[CrunchMinute, TQM]]]
  }

  val secondaryQueuesLookup: (Terminal, SDateLike) => Future[Option[MinutesContainer[CrunchMinute, TQM]]] = (terminal: Terminal, date: SDateLike) => {
    val actor = crunchStateReadActor(date)
    requestAndTerminateActor.ask(RequestAndTerminate(actor, GetCrunchMinutes(terminal))).mapTo[Option[MinutesContainer[CrunchMinute, TQM]]]
  }

  val primaryStaffLookup: (Terminal, SDateLike) => Future[Option[MinutesContainer[StaffMinute, TM]]] = (terminal: Terminal, date: SDateLike) => {
    val actor = system.actorOf(TerminalDayStaffActor.props(terminal, date, now))
    requestAndTerminateActor.ask(RequestAndTerminate(actor, GetState)).mapTo[Option[MinutesContainer[StaffMinute, TM]]]
  }

  val secondaryStaffLookup: (Terminal, SDateLike) => Future[Option[MinutesContainer[StaffMinute, TM]]] = (terminal: Terminal, date: SDateLike) => {
    val actor = crunchStateReadActor(date)
    requestAndTerminateActor.ask(RequestAndTerminate(actor, GetStaffMinutes(terminal))).mapTo[Option[MinutesContainer[StaffMinute, TM]]]
  }

  def crunchStateReadActor(date: SDateLike): ActorRef = {
    system.actorOf(Props(new CrunchStateReadActor(1000, date.addHours(4), expireAfterMillis, queuesByTerminal, date.millisSinceEpoch, date.addDays(1).millisSinceEpoch)))
  }

  def queueMinutesActor(clazz: Class[_]): ActorRef

  def staffMinutesActor(clazz: Class[_]): ActorRef
}

case class MinuteLookups(system: ActorSystem,
                         now: () => SDateLike,
                         expireAfterMillis: Int,
                         queuesByTerminal: Map[Terminal, Seq[Queue]])
                        (implicit val ec: ExecutionContext) extends MinuteLookupsLike {
  override def queueMinutesActor(clazz: Class[_]): ActorRef = system.actorOf(Props(clazz, now, queuesByTerminal.keys, primaryQueuesLookup, secondaryQueuesLookup, updateCrunchMinutes))

  override def staffMinutesActor(clazz: Class[_]): ActorRef = system.actorOf(Props(clazz, now, queuesByTerminal.keys, primaryStaffLookup, secondaryStaffLookup, updateStaffMinutes))
}
