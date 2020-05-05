package actors

import actors.DrtStaticParameters.expireAfterMillis
import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import actors.daily.ReadJournalTypes.ReadJournalWithEvents
import actors.daily.TerminalDay.TerminalDayBookmarks
import actors.daily.{StartUpdatesStream, TerminalDayQueuesUpdatesActor, TerminalDayStaffUpdatesActor, UpdatesSupervisor}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.persistence.jdbc.query.scaladsl.JdbcReadJournal
import akka.persistence.query.journal.leveldb.scaladsl.LeveldbReadJournal
import akka.util.Timeout
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{FlightsWithSplits, FlightsWithSplitsDiff}
import drt.shared.Terminals.Terminal
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.crunch.deskrecs.GetFlights

import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.postfixOps

object PartitionedPortStateActor {
  def apply(now: () => SDateLike, airportConfig: AirportConfig, journalType: StreamingJournalLike)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    val lookups: MinuteLookups = MinuteLookups(system, now, MilliTimes.oneDayMillis, airportConfig.queuesByTerminal)
    val flightsActor: ActorRef = system.actorOf(Props(new FlightsStateActor(None, Sizes.oneMegaByte, "crunch-live-state-actor", airportConfig.queuesByTerminal, now, expireAfterMillis)))
    val queuesActor: ActorRef = lookups.queueMinutesActor(classOf[QueueMinutesActor])
    val staffActor: ActorRef = lookups.staffMinutesActor(classOf[StaffMinutesActor])
    system.actorOf(Props(new PartitionedPortStateActor(flightsActor, queuesActor, staffActor, now, airportConfig.terminals.toList, journalType)))
  }
}

trait StreamingJournalLike {
  type ReadJournalType <: ReadJournalWithEvents
  val id: String
}

object ProdStreamingJournal extends StreamingJournalLike {
  override type ReadJournalType = JdbcReadJournal
  override val id: String = JdbcReadJournal.Identifier
}

object TestStreamingJournal extends StreamingJournalLike {
  override type ReadJournalType = LeveldbReadJournal
  override val id: String = LeveldbReadJournal.Identifier
}

class PartitionedPortStateActor(flightsActor: ActorRef,
                                queuesActor: ActorRef,
                                staffActor: ActorRef,
                                now: () => SDateLike,
                                terminals: List[Terminal],
                                journalType: StreamingJournalLike) extends Actor {
  val log: Logger = LoggerFactory.getLogger(getClass)

  implicit val ec: ExecutionContextExecutor = context.dispatcher
  implicit val timeout: Timeout = new Timeout(10 seconds)

  val queueUpdatesProps: (Terminal, SDateLike, MillisSinceEpoch) => Props =
    (terminal: Terminal, day: SDateLike, startingSequenceNr: MillisSinceEpoch) => {
      Props(new TerminalDayQueuesUpdatesActor(day.getFullYear(), day.getMonth(), day.getDate(), terminal, now, journalType, startingSequenceNr))
    }

  val staffUpdatesProps: (Terminal, SDateLike, MillisSinceEpoch) => Props =
    (terminal: Terminal, day: SDateLike, startingSequenceNr: MillisSinceEpoch) => {
      Props(new TerminalDayStaffUpdatesActor(day.getFullYear(), day.getMonth(), day.getDate(), terminal, now, journalType, startingSequenceNr))
    }

  val queueUpdatesSupervisor: ActorRef = context.system.actorOf(Props(new UpdatesSupervisor[CrunchMinute, TQM](now, terminals, queueUpdatesProps)))
  val staffUpdatesSupervisor: ActorRef = context.system.actorOf(Props(new UpdatesSupervisor[StaffMinute, TM](now, terminals, staffUpdatesProps)))

  def processMessage: Receive = {
    case msg: SetCrunchActor =>
      log.info(s"Received crunchSourceActor")
      flightsActor.ask(msg)

    case msg: SetSimulationActor =>
      log.info(s"Received simulationSourceActor")
      queuesActor.ask(msg)

    case StreamInitialized => sender() ! Ack

    case StreamCompleted => log.info(s"Stream completed")

    case StreamFailure(t) => log.error(s"Stream failed", t)

    case flightsWithSplits: FlightsWithSplitsDiff =>
      val replyTo = sender()
      askThenAck(flightsWithSplits, replyTo, flightsActor)

    case noUpdates: PortStateMinutes[_, _] if noUpdates.isEmpty =>
      sender() ! Ack

    case someQueueUpdates: PortStateQueueMinutes =>
      val replyTo = sender()
      askThenAck(someQueueUpdates.asContainer, replyTo, queuesActor)

    case someStaffUpdates: PortStateStaffMinutes =>
      val replyTo = sender()
      askThenAck(someStaffUpdates.asContainer, replyTo, staffActor)

    case GetState =>
      log.warn("Ignoring GetState request (for entire state)")

    case GetPortState(start, end) =>
      log.debug(s"Received GetPortState request from ${SDate(start).toISOString()} to ${SDate(end).toISOString()}")
      replyWithPortState(start, end, sender())

    case StartUpdatesStream(terminal, day, seqNr) =>
      val replyTo = sender()
      Future.sequence(startUpdateStreams(Map((terminal, day.millisSinceEpoch) -> seqNr), queueUpdatesSupervisor)).foreach(_ => replyTo ! Ack)

    case GetPortStateForTerminal(start, end, terminal) =>
      log.debug(s"Received GetPortStateForTerminal request from ${SDate(start).toISOString()} to ${SDate(end).toISOString()} for $terminal")
      replyWithTerminalState(start, end, terminal, sender())

    case GetUpdatesSince(since, start, end) =>
      log.debug(s"Received GetUpdatesSince request since ${SDate(since).toISOString()} from ${SDate(start).toISOString()} to ${SDate(end).toISOString()}")
      replyWithUpdates(since, start, end, sender())

    case getFlights: GetFlights =>
      log.debug(s"Received GetFlights request from ${SDate(getFlights.from).toISOString()} to ${SDate(getFlights.to).toISOString()}")
      flightsActor forward getFlights
  }

  override def receive: Receive = processMessage orElse {
    case unexpected => log.warn(s"Got unexpected: ${unexpected.getClass}")
  }

  def replyWithPortState(start: MillisSinceEpoch,
                         end: MillisSinceEpoch,
                         replyTo: ActorRef): Future[Option[PortState]] = {
    val eventualFlights = flightsActor.ask(GetFlights(start, end)).mapTo[FlightsWithSplits]
    val eventualQueueMinutes = queuesActor.ask(GetPortState(start, end)).mapTo[MinutesWithBookmarks[CrunchMinute, TQM]]
    val eventualStaffMinutes = staffActor.ask(GetPortState(start, end)).mapTo[MinutesWithBookmarks[StaffMinute, TM]]
    val eventualPortState = combineToPortStateAndBookmarks(eventualFlights, eventualQueueMinutes, eventualStaffMinutes)
    eventualPortState.map {
      case (ps, (queueBookmarks, staffBookmarks)) =>
        startUpdateStreams(queueBookmarks, queueUpdatesSupervisor)
        startUpdateStreams(staffBookmarks, staffUpdatesSupervisor)
        Option(ps)
    }.pipeTo(replyTo)
  }

  def startUpdateStreams(bookmarks: TerminalDayBookmarks, supervisor: ActorRef): immutable.Iterable[Future[Any]] = bookmarks.map {
    case ((t, d), bookmarkSeqNr) => supervisor.ask(StartUpdatesStream(t, SDate(d), bookmarkSeqNr))
  }

  def replyWithUpdates(since: MillisSinceEpoch,
                       start: MillisSinceEpoch,
                       end: MillisSinceEpoch,
                       replyTo: ActorRef): Future[Option[PortStateUpdates]] = {
    val updatesRequest = GetUpdatesSince(since, start, end)
    val eventualFlights = flightsActor.ask(updatesRequest).mapTo[FlightsWithSplits]
    val eventualQueueMinutes = queueUpdatesSupervisor.ask(updatesRequest).mapTo[MinutesContainer[CrunchMinute, TQM]]
    val eventualStaffMinutes = staffUpdatesSupervisor.ask(updatesRequest).mapTo[MinutesContainer[StaffMinute, TM]]
    val eventualPortState = combineToPortStateUpdates(eventualFlights, eventualQueueMinutes, eventualStaffMinutes)
    eventualPortState.pipeTo(replyTo)
  }

  def replyWithTerminalState(start: MillisSinceEpoch,
                             end: MillisSinceEpoch,
                             terminal: Terminal,
                             replyTo: ActorRef): Future[Option[PortState]] = {
    val eventualFlights = flightsActor.ask(GetFlightsForTerminal(start, end, terminal)).mapTo[FlightsWithSplits]
    val eventualQueueMinutes = queuesActor.ask(GetStateByTerminalDateRange(terminal, SDate(start), SDate(end))).mapTo[MinutesContainer[CrunchMinute, TQM]]
    val eventualStaffMinutes = staffActor.ask(GetStateByTerminalDateRange(terminal, SDate(start), SDate(end))).mapTo[MinutesContainer[StaffMinute, TM]]
    val eventualPortState = combineToPortState(eventualFlights, eventualQueueMinutes, eventualStaffMinutes)
    eventualPortState.map(Option(_)).pipeTo(replyTo)
  }

  def stateAsTuple(eventualFlights: Future[FlightsWithSplits],
                   eventualQueueMinutes: Future[MinutesContainer[CrunchMinute, TQM]],
                   eventualStaffMinutes: Future[MinutesContainer[StaffMinute, TM]]): Future[(Iterable[ApiFlightWithSplits], Iterable[CrunchMinute], Iterable[StaffMinute])] =
    for {
      flights <- eventualFlights
      queueMinutes <- eventualQueueMinutes
      staffMinutes <- eventualStaffMinutes
    } yield {
      val fs = flights.flights.toMap.values
      val cms = queueMinutes.minutes.map(_.toMinute)
      val sms = staffMinutes.minutes.map(_.toMinute)
      (fs, cms, sms)
    }

  def stateWithBookmarksAsTuple(eventualFlights: Future[FlightsWithSplits],
                                eventualQueueMinutes: Future[MinutesWithBookmarks[CrunchMinute, TQM]],
                                eventualStaffMinutes: Future[MinutesWithBookmarks[StaffMinute, TM]]): Future[(Iterable[ApiFlightWithSplits], Iterable[CrunchMinute], Iterable[StaffMinute], TerminalDayBookmarks, TerminalDayBookmarks)] =
    for {
      flights <- eventualFlights
      queueMinutesWithBookmarks <- eventualQueueMinutes
      staffMinutesWithBookmarks <- eventualStaffMinutes
    } yield {
      val fs = flights.flights.toMap.values
      val cms = queueMinutesWithBookmarks.container.minutes.map(_.toMinute)
      val cmsBookmarks = queueMinutesWithBookmarks.terminalDayBookmarks
      val sms = staffMinutesWithBookmarks.container.minutes.map(_.toMinute)
      val smsBookmarks = staffMinutesWithBookmarks.terminalDayBookmarks
      (fs, cms, sms, cmsBookmarks, smsBookmarks)
    }

  def combineToPortState(eventualFlights: Future[FlightsWithSplits],
                         eventualQueueMinutes: Future[MinutesContainer[CrunchMinute, TQM]],
                         eventualStaffMinutes: Future[MinutesContainer[StaffMinute, TM]]): Future[PortState] =
    stateAsTuple(eventualFlights, eventualQueueMinutes, eventualStaffMinutes).map {
      case (fs, cms, sms) => PortState(fs, cms, sms)
    }

  def combineToPortStateAndBookmarks(eventualFlights: Future[FlightsWithSplits],
                                     eventualQueueMinutes: Future[MinutesWithBookmarks[CrunchMinute, TQM]],
                                     eventualStaffMinutes: Future[MinutesWithBookmarks[StaffMinute, TM]]): Future[(PortState, (TerminalDayBookmarks, TerminalDayBookmarks))] =
    stateWithBookmarksAsTuple(eventualFlights, eventualQueueMinutes, eventualStaffMinutes).map {
      case (fs, cms, sms, cmsBookmarks, smsBookmarks) => (PortState(fs, cms, sms), (cmsBookmarks, smsBookmarks))
    }

  def combineToPortStateUpdates(eventualFlights: Future[FlightsWithSplits],
                                eventualQueueMinutes: Future[MinutesContainer[CrunchMinute, TQM]],
                                eventualStaffMinutes: Future[MinutesContainer[StaffMinute, TM]]): Future[Option[PortStateUpdates]] =
    stateAsTuple(eventualFlights, eventualQueueMinutes, eventualStaffMinutes).map {
      case (fs, cms, sms) =>
        fs.map(_.lastUpdated.getOrElse(0L)) ++ cms.map(_.lastUpdated.getOrElse(0L)) ++ sms.map(_.lastUpdated.getOrElse(0L)) match {
          case noUpdates if noUpdates.isEmpty =>
            None
          case millis =>
            Option(PortStateUpdates(millis.max, fs.toSet, cms.toSet, sms.toSet))
        }
    }

  def askThenAck(message: Any, replyTo: ActorRef, actor: ActorRef): Unit =
    actor.ask(message).foreach(_ => replyTo ! Ack)
}