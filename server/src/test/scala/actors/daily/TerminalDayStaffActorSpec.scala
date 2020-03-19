package actors.daily

import actors.{ClearState, GetState}
import akka.actor.Props
import akka.pattern.AskableActorRef
import akka.util.Timeout
import drt.shared.CrunchApi.{CrunchMinute, MinutesContainer, StaffMinute}
import drt.shared.Queues.EeaDesk
import drt.shared.Terminals.{T1, Terminal}
import drt.shared.{SDateLike, TM, TQM}
import services.SDate
import services.crunch.CrunchTestLike

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object MockTerminalDayStaffActor {
  def props(day: SDateLike, terminal: Terminal, initialState: Map[TM, StaffMinute]): Props =
    Props(new MockTerminalDayStaffActor(day, terminal, initialState))
}

class MockTerminalDayStaffActor(day: SDateLike,
                                 terminal: Terminal,
                                 initialState: Map[TM, StaffMinute]) extends TerminalDayStaffActor(day.getFullYear(), day.getMonth(), day.getDate(), terminal, () => day) {
  state = initialState
}

class TerminalDayStaffActorSpec extends CrunchTestLike {
  val terminal: Terminal = T1

  implicit val timeout: Timeout = new Timeout(5 seconds)

  val date: SDateLike = SDate("2020-01-01")
  val myNow: () => SDateLike = () => date

  "Given a terminal-day queues actor for a day which does not any data" >> {
    val terminalSummariesActor: AskableActorRef = actorForTerminalAndDate(terminal, date)

    "When I ask for the state for that day" >> {
      "I should get back an empty map of staff minutes" >> {
        val result = Await.result(terminalSummariesActor.ask(GetState).asInstanceOf[Future[Option[Map[TM, StaffMinute]]]], 1 second)

        result === None
      }
    }

    "When I send minutes to persist which lie within the day, and then ask for its state I should see the minutes sent" >> {
      val staffMinutes = MinutesContainer(Set(staffMinuteForDate(date)))
      val terminalSummariesActor: AskableActorRef = actorForTerminalAndDate(terminal, date)

      val eventual = sendMinuteQueryAndClear(staffMinutes, terminalSummariesActor)
      val result = Await.result(eventual, 1 second)

      result === Option(staffMinutes)
    }

    "When I send minutes to persist which lie outside the day, and then ask for its state I should see None" >> {
      val otherDate = SDate("2020-01-02T00:00")
      val staffMinutes = MinutesContainer(Set(staffMinuteForDate(otherDate)))
      val terminalSummariesActor: AskableActorRef = actorForTerminalAndDate(terminal, date)

      val eventual = sendMinuteQueryAndClear(staffMinutes, terminalSummariesActor)
      val result = Await.result(eventual, 1 second)

      result === None
    }

    "When I send minutes to persist which lie both inside and outside the day, and then ask for its state I should see only the minutes inside the actor's day" >> {
      val otherDate = SDate("2020-01-02T00:00")
      val inside = staffMinuteForDate(date)
      val outside = staffMinuteForDate(otherDate)
      val staffMinutes = MinutesContainer(Set(inside, outside))
      val terminalSummariesActor: AskableActorRef = actorForTerminalAndDate(terminal, date)

      val eventual = sendMinuteQueryAndClear(staffMinutes, terminalSummariesActor)
      val result = Await.result(eventual, 1 second)

      result === Option(MinutesContainer(Set(inside)))
    }
  }

  private def sendMinuteQueryAndClear(minutesContainer: MinutesContainer,
                                      terminalSummariesActor: AskableActorRef): Future[Option[MinutesContainer]] = {
    terminalSummariesActor.ask(minutesContainer).flatMap { _ =>
      terminalSummariesActor.ask(GetState).asInstanceOf[Future[Option[MinutesContainer]]].flatMap { r =>
        terminalSummariesActor.ask(ClearState).map { _ => r }
      }
    }
  }

  private def staffMinuteForDate(date: SDateLike): StaffMinute = {
    StaffMinute(terminal, date.millisSinceEpoch, 1, 2, 3)
  }

  private def actorForTerminalAndDate(terminal: Terminal, date: SDateLike): AskableActorRef = {
    system.actorOf(TerminalDayStaffActor.props(date, terminal, () => date))
  }
}
