package actors.migration

import actors.InMemoryStreamingJournal
import actors.acking.AckingReceiver.Ack
import actors.daily.RequestAndTerminateActor
import actors.minutes.MinutesActorLike.{CrunchMinutesMigrationUpdate, FlightsMigrationUpdate}
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.persistence.PersistentActor
import akka.testkit.{ImplicitSender, TestProbe}
import drt.shared.{SDateLike, UtcDate}
import scalapb.GeneratedMessage
import server.protobuf.messages.CrunchState._
import server.protobuf.messages.FlightsMessage.{FlightMessage, UniqueArrivalMessage}
import services.SDate
import services.crunch.CrunchTestLike

import scala.concurrent.Await
import scala.concurrent.duration._

object PersistMessageForIdActor {
  def props(idToPersist: String): Props = Props(new PersistMessageForIdActor(idToPersist))
}

class PersistMessageForIdActor(idToPersist: String) extends PersistentActor {
  override def receiveRecover: Receive = {
    case _ => Unit
  }

  override def receiveCommand: Receive = {
    case message: GeneratedMessage =>
      val replyTo = sender()
      persist(message)((message) => {
        context.system.eventStream.publish(message)
        replyTo ! Ack
      })

  }

  override def persistenceId: String = idToPersist
}

class DummyActor(probe: ActorRef, terminal: String, date: SDateLike) extends Actor {
  override def receive: Receive = {
    case message =>
      probe ! (terminal, date, message)
      sender() ! Ack
  }
}

object DummyActor {
  def props(probe: ActorRef)(terminal: String, date: UtcDate) =
    Props(new DummyActor(probe, terminal, SDate(date)))
}

class CrunchStateMigrationSpec extends CrunchTestLike with ImplicitSender {

  val legacyCutoffDate: SDateLike = SDate("2020-12-31T10:00:00Z")

  /**
   * Considerations
   * 1) We need to retain the `createdAt` part so that the non-legacy actors use it rather than using now()
   *   - maybe send the protobuf messages to and actor that overrides the FlightsRouter & TerminalMinuteLike actors?
   *     they could use the same logic, ie grouping by terminal & day before passing them on to the relevant persisting
   *     actors
   *
   * 2) We need to update the timestamp field of the snapshot table to match that of the original data
   *   - maybe the persisting actors can simply use the max createdAt field from the message that triggered the snapshot
   *     to update the timestamp using a raw slick query
   *     3) We can handle all three data type migrations from the same stream of persisted CrunchStateActor data
   *     4) We additionally have to handle the FlightsStateActor data once we've finished the CrunchStateActor data
   */
  "Given a stream of EventEnvelopes containing legacy CrunchDiffMessages with flights to remove and flight updates" >> {
    "When I ask for them to be re-persisted as non-legacy data" >> {
      "I should see each type of data sent as a protobuf message to the TerminalDayFlightMigrationActor" >> {
        val createdAt = SDate("2020-10-01T00:00").millisSinceEpoch
        val scheduled = SDate("2020-10-02T12:10")

        val removalMessage = UniqueArrivalMessage(Option(1), Option("T1"), Option(scheduled.millisSinceEpoch))

        val flightMessage = FlightMessage(terminal = Option("T1"), scheduled = Option(scheduled.millisSinceEpoch))
        val fwsMsg = FlightWithSplitsMessage(Option(flightMessage))

        val crunchDiffMessage = CrunchDiffMessage(Option(createdAt), None, Seq(removalMessage), Seq(fwsMsg))

        setMigrationData(crunchDiffMessage)

        val flightsTestProbe = TestProbe()
        val updateFlightsFn: FlightsMigrationUpdate = flightsUpdateFn(flightsTestProbe)

        val migrator = LegacyMigrator(
          updateFlightsFn,
          crunchMinutesUpdateFn(TestProbe(), legacyCutoffDate),
          staffMinutesUpdateFn(TestProbe(), legacyCutoffDate),
          InMemoryStreamingJournal,
          LegacyStreamingJournalMigrationActor.legacy1PersistenceId,
          0L
        )
        migrator.start()
        val expectedMessage = FlightsWithSplitsDiffMessage(Some(createdAt), Vector(removalMessage), Vector(fwsMsg))

        flightsTestProbe.expectMsg(("T1", SDate(2020, 10, 2), expectedMessage))
        success
      }
    }
  }

  "Given a FlightsWithSplitsDiffMessage containing removals for flights scheduled on or before the message's creation date day" >> {
    "When I ask to remove any invalid removals" >> {
      "I should only see the valid removal messages remaining" >> {
        val createdAt = SDate("2020-10-10T12:00").millisSinceEpoch
        val scheduledInPast = SDate("2020-10-09T11:10")
        val scheduledSameDay = SDate("2020-10-10T13:10")
        val scheduledInFuture = SDate("2020-10-11T12:10")

        val validRemovalMessage = UniqueArrivalMessage(Option(1), Option("T1"), Option(scheduledInFuture.millisSinceEpoch))
        val invalidRemovalMessage1 = UniqueArrivalMessage(Option(1), Option("T1"), Option(scheduledSameDay.millisSinceEpoch))
        val invalidRemovalMessage2 = UniqueArrivalMessage(Option(1), Option("T1"), Option(scheduledInPast.millisSinceEpoch))

        val diffMessage = FlightsWithSplitsDiffMessage(Option(createdAt), Seq(validRemovalMessage, invalidRemovalMessage1, invalidRemovalMessage2), Seq())
        val result = FlightsRouterMigrationActor.removeInvalidRemovals(diffMessage)

        result === FlightsWithSplitsDiffMessage(Option(createdAt), Seq(validRemovalMessage))
      }
    }
  }

  "Given a CrunchDiffMessage with flights to remove where some they're scheduled on the same day or earlier than the message's createdAt day" >> {
    "When I ask for them to be re-persisted as non-legacy data" >> {
      "I should see each type of data sent as a protobuf message to the TerminalDayFlightMigrationActor" >> {
        val createdAt = SDate("2020-10-10T12:00").millisSinceEpoch
        val scheduledInPast = SDate("2020-10-09T11:10")
        val scheduledSameDay = SDate("2020-10-10T13:10")
        val scheduledInFuture = SDate("2020-10-11T12:10")

        val validRemovalMessage = UniqueArrivalMessage(Option(1), Option("T1"), Option(scheduledInFuture.millisSinceEpoch))
        val invalidRemovalMessage1 = UniqueArrivalMessage(Option(1), Option("T1"), Option(scheduledSameDay.millisSinceEpoch))
        val invalidRemovalMessage2 = UniqueArrivalMessage(Option(1), Option("T1"), Option(scheduledInPast.millisSinceEpoch))

        val crunchDiffMessage = CrunchDiffMessage(Option(createdAt), None, Seq(validRemovalMessage, invalidRemovalMessage1, invalidRemovalMessage2), Seq())

        setMigrationData(crunchDiffMessage)

        val flightsTestProbe = TestProbe()
        val updateFlightsFn: FlightsMigrationUpdate = flightsUpdateFn(flightsTestProbe)

        val migrator = LegacyMigrator(
          updateFlightsFn,
          crunchMinutesUpdateFn(TestProbe(), legacyCutoffDate),
          staffMinutesUpdateFn(TestProbe(), legacyCutoffDate),
          InMemoryStreamingJournal,
          LegacyStreamingJournalMigrationActor.legacy1PersistenceId,
          0L
        )
        migrator.start()

        flightsTestProbe.expectMsg(("T1", SDate(2020, 10, 9), FlightsWithSplitsDiffMessage(Some(createdAt), Vector(), Vector())))
        flightsTestProbe.expectMsg(("T1", SDate(2020, 10, 10), FlightsWithSplitsDiffMessage(Some(createdAt), Vector(), Vector())))
        flightsTestProbe.expectMsg(("T1", SDate(2020, 10, 11), FlightsWithSplitsDiffMessage(Some(createdAt), Vector(validRemovalMessage), Vector())))
        success
      }
    }
  }

    "Given a stream of EventEnvelopes containing legacy CrunchDiffMessages with crunch minutes" >> {
      "When I ask for them to be re-persisted as non-legacy data" >> {
        "I should see them sent as a protobuf message to the terminal day actor" >> {

          val createdAt = SDate("2020-10-01T00:00").millisSinceEpoch

          val minuteTime = SDate("2020-09-10T00:00Z")
          val crunchMinute = CrunchMinuteMessage(Option("T1"), Option("Eea"), Option(minuteTime.millisSinceEpoch))

          val crunchDiffMessage = CrunchDiffMessage(Option(createdAt), None, crunchMinutesToUpdate = Seq(crunchMinute))

          setMigrationData(crunchDiffMessage)

          val minutesTestProbe = TestProbe()
          val updateMinutesFn: CrunchMinutesMigrationUpdate = crunchMinutesUpdateFn(minutesTestProbe, legacyCutoffDate)

          val migrator = LegacyMigrator(
            flightsUpdateFn(TestProbe()),
            updateMinutesFn,
            staffMinutesUpdateFn(TestProbe(), legacyCutoffDate),
            InMemoryStreamingJournal,
            LegacyStreamingJournalMigrationActor.legacy1PersistenceId,
            0L
          )

          migrator.start()

          val expectedMessage = CrunchMinutesMessageMigration(createdAt, Vector(crunchMinute))

          minutesTestProbe.expectMsg(("T1", minuteTime, expectedMessage))
          success
        }
      }
    }

    "Given a stream of EventEnvelopes containing legacy CrunchDiffMessages with staff minutes" >> {
      "When I ask for them to be re-persisted as non-legacy data" >> {
        "I should see them sent as a protobuf message to the terminal day actor" >> {

          val createdAt = SDate("2020-10-01T00:00").millisSinceEpoch

          val minuteTime = SDate("2020-09-10T00:00Z")
          val staffMinuteMessage = StaffMinuteMessage(Option("T1"), Option(minuteTime.millisSinceEpoch), Option(1))

          val crunchDiffMessage = CrunchDiffMessage(Option(createdAt), None, Seq(), Seq(), Seq(), Seq(staffMinuteMessage))

          setMigrationData(crunchDiffMessage)

          val minutesTestProbe = TestProbe()

          val migrator = LegacyMigrator(
            flightsUpdateFn(TestProbe()),
            crunchMinutesUpdateFn(TestProbe(), legacyCutoffDate),
            staffMinutesUpdateFn(minutesTestProbe, legacyCutoffDate),
            InMemoryStreamingJournal,
            LegacyStreamingJournalMigrationActor.legacy1PersistenceId,
            0L
          )

          migrator.start()

          val expectedMessage = StaffMinutesMessageMigration(createdAt, Vector(staffMinuteMessage))

          minutesTestProbe.expectMsg(("T1", minuteTime, expectedMessage))
          success
        }
      }
    }

    "Given legacy data for a date on or since the new storage mechanism began" >> {
      "When I ask for them to be re-persisted as non-legacy data" >> {
        "Those days should be ignored for Crunch and Staff minutes but not for flights" >> {
          val legacyCutoffDate = SDate("2020-10-10T10:00:00Z")
          val createdAt = SDate("2020-10-01T00:00").millisSinceEpoch
          val scheduled = SDate("2020-10-10T09:00:00Z")

          val removalMessage = UniqueArrivalMessage(Option(1), Option("T1"), Option(scheduled.millisSinceEpoch))

          val flightMessage = FlightMessage(terminal = Option("T1"), scheduled = Option(scheduled.millisSinceEpoch))
          val fwsMsg = FlightWithSplitsMessage(Option(flightMessage))

          val staffMinuteMessage = StaffMinuteMessage(Option("T1"), Option(scheduled.millisSinceEpoch), Option(1))

          val crunchMinute = CrunchMinuteMessage(Option("T1"), Option("Eea"), Option(scheduled.millisSinceEpoch))

          val crunchDiffMessage = CrunchDiffMessage(Option(createdAt),
            None,
            Seq(removalMessage),
            Seq(fwsMsg),
            Seq(crunchMinute),
            Seq(staffMinuteMessage)
          )

          setMigrationData(crunchDiffMessage)

          val flightsTestProbe = TestProbe()
          val crunchMinutesTestProbe = TestProbe()
          val staffMinutesTestProbe = TestProbe()
          val updateFlightsFn: FlightsMigrationUpdate = flightsUpdateFn(flightsTestProbe)

          val migrator = LegacyMigrator(
            updateFlightsFn,
            crunchMinutesUpdateFn(crunchMinutesTestProbe, legacyCutoffDate),
            staffMinutesUpdateFn(staffMinutesTestProbe, legacyCutoffDate),
            InMemoryStreamingJournal,
            LegacyStreamingJournalMigrationActor.legacy1PersistenceId,
            0L
          )
          migrator.start()
          val expectedMessage = FlightsWithSplitsDiffMessage(Some(createdAt), Vector(removalMessage), Vector(fwsMsg))

          staffMinutesTestProbe.expectNoMessage(500 millis)
          crunchMinutesTestProbe.expectNoMessage(500 millis)
          flightsTestProbe.expectMsg(("T1", SDate(2020, 10, 10), expectedMessage))
          success
        }
      }
    }

  private def flightsUpdateFn(flightsTestProbe: TestProbe) = {
    val requestAndTerminateActor = system.actorOf(Props(new RequestAndTerminateActor))
    val updateFlightsFn = FlightsRouterMigrationActor
      .updateFlights(requestAndTerminateActor, DummyActor.props(flightsTestProbe.ref))
    updateFlightsFn
  }

  private def crunchMinutesUpdateFn(minutesTestProbe: TestProbe, legacyDate: SDateLike) = {
    val requestAndTerminateActor = system.actorOf(Props(new RequestAndTerminateActor))
    val updateMinutesFn = FlightsRouterMigrationActor
      .updateCrunchMinutes(legacyDate, requestAndTerminateActor, DummyActor.props(minutesTestProbe.ref))
    updateMinutesFn
  }

  private def staffMinutesUpdateFn(minutesTestProbe: TestProbe, legacyDate: SDateLike) = {
    val requestAndTerminateActor = system.actorOf(Props(new RequestAndTerminateActor))
    val updateMinutesFn = FlightsRouterMigrationActor
      .updateStaffMinutes(legacyDate, requestAndTerminateActor, DummyActor.props(minutesTestProbe.ref))
    updateMinutesFn
  }

  private def setMigrationData(message: CrunchDiffMessage) = {
    val persistActor = system.actorOf(PersistMessageForIdActor.props(LegacyStreamingJournalMigrationActor.legacy1PersistenceId))
    Await.result(persistActor ? message, 1 second)
  }
}

