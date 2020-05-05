package actors.daily

import java.io.File
import java.util.UUID

import actors.{StreamingJournalLike, TestStreamingJournal}
import actors.daily.ReadJournalTypes.ReadJournalWithEvents
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.persistence.query.journal.leveldb.scaladsl.LeveldbReadJournal
import akka.stream.ActorMaterializer
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import drt.shared.CrunchApi.{CrunchMinute, MinutesContainer}
import drt.shared.Terminals.{T1, Terminal}
import drt.shared.{Queues, SDateLike}
import org.specs2.mutable.SpecificationLike
import org.specs2.specification.BeforeEach
import server.protobuf.messages.CrunchState.CrunchMinuteMessage
import services.SDate
import test.TestActors.{ResetData, TestTerminalDayQueuesActor}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.reflect.io.Directory

object LevelDbConfig {
  val tempStoreDir: String = s"/tmp/drt-${UUID.randomUUID().toString}"

  def config(uuid: String): Config = ConfigFactory.load("leveldb").withValue("akka.persistence.journal.leveldb.dir", ConfigValueFactory.fromAnyRef(tempStoreDir + uuid))

  def clearData(uuid: String): Boolean = {
    val path = tempStoreDir + uuid
    println(s"Attempting to delete $path")
    val directory = new Directory(new File(path))
    if (directory.exists) {
      directory.deleteRecursively()
    } else true
  }
}

class TerminalDayQueuesUpdatesActorSpec
  extends TestKit(ActorSystem("drt", LevelDbConfig.config("TerminalDayQueuesUpdatesActorSpec")))
    with SpecificationLike {

  implicit val mat: ActorMaterializer = ActorMaterializer()
  implicit val timeout: Timeout = new Timeout(1 second)

  val terminal: Terminal = T1
  val queue: Queues.Queue = Queues.EeaDesk
  val date: String = "2020-01-01"
  val day: SDateLike = SDate(s"${date}T00:00")

  val crunchMinute: CrunchMinute = CrunchMinute(terminal, queue, day.millisSinceEpoch, 1, 2, 3, 4)
  val crunchMinuteMessage: CrunchMinuteMessage = CrunchMinuteMessage(Option(terminal.toString), Option(queue.toString), Option(day.millisSinceEpoch), Option(1.0), Option(2.0), Option(3), Option(4), None, None, None, None, Option(day.millisSinceEpoch))

  "Given a TerminalDayQueueMinuteUpdatesActor" >> {
    implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global
    val queuesActor = system.actorOf(Props(new TestTerminalDayQueuesActor(day.getFullYear(), day.getMonth(), day.getDate(), terminal, () => day)))
    Await.ready(queuesActor.ask(ResetData), 1 second)
    val probe = TestProbe()
    system.actorOf(Props(new TestTerminalDayQueuesUpdatesActor[LeveldbReadJournal](day.getFullYear(), day.getMonth(), day.getDate(), terminal, () => day, TestStreamingJournal, 0L, probe.ref)))
    val minute2 = day.addMinutes(1).millisSinceEpoch

    "When I send it a crunch minute" >> {
      val eventualAcks = Future.sequence(Seq(
        queuesActor.ask(MinutesContainer(Iterable(crunchMinute))),
        queuesActor.ask(MinutesContainer(Iterable(crunchMinute.copy(minute = minute2))))))
      Await.ready(eventualAcks, 1 second)

      "I should see it received as an update" >> {
        val expected = List(
          crunchMinute.copy(lastUpdated = Option(day.millisSinceEpoch)),
          crunchMinute.copy(minute = minute2, lastUpdated = Option(day.millisSinceEpoch)))
          .map(cm => (cm.key, cm)).toMap

        probe.fishForMessage(5 seconds) {
          case updates => updates == expected
        }

        success
      }
    }
  }
}

class TestTerminalDayQueuesUpdatesActor[T <: ReadJournalWithEvents](year: Int,
                                                                    month: Int,
                                                                    day: Int,
                                                                    terminal: Terminal,
                                                                    now: () => SDateLike,
                                                                    journalType: StreamingJournalLike,
                                                                    startingSequenceNr: Long,
                                                                    probe: ActorRef) extends TerminalDayQueuesUpdatesActor(year, month, day, terminal, now, journalType, startingSequenceNr) {
  override def updateState(minuteMessages: Seq[CrunchMinuteMessage]): Unit = {
    super.updateState(minuteMessages)
    probe ! updates
  }
}
