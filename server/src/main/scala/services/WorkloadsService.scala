package services

import org.slf4j.LoggerFactory
import services.workloadcalculator.PaxLoadCalculator
import spatutorial.shared.FlightsApi._
import spatutorial.shared._

import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


trait FlightsService extends FlightsApi {
  def getFlights(st: Long, end: Long): Future[List[ApiFlight]]

  def flights(startTimeEpoch: Long, endTimeEpoch: Long): Flights = {
    val fsFuture = getFlights(startTimeEpoch, endTimeEpoch)
    Flights(Await.result(fsFuture, Duration.Inf))
  }
}

trait WorkloadsService extends WorkloadsApi with WorkloadsCalculator {
  self: FlightsService =>
  type WorkloadByTerminalQueue = Map[TerminalName, Map[QueueName, (Seq[WL], Seq[Pax])]]

  override def getWorkloads(): Future[WorkloadByTerminalQueue] = getWorkloadsByTerminal(getFlights(0, 0))
}

trait WorkloadsCalculator {
  private val log = LoggerFactory.getLogger(getClass)

  type TerminalQueueWorkloads = Map[TerminalName, Map[QueueName, (Seq[WL], Seq[Pax])]]

  def numberOf15Mins = (24 * 4 * 15)

  def maxLoadPerSlot: Int = 20

  def splitRatioProvider: (ApiFlight) => Option[List[SplitRatio]]

  def procTimesProvider(terminalName: TerminalName)(paxTypeAndQueue: PaxTypeAndQueue): Double

//  def procTimesProvider(paxTypeAndQueue: PaxTypeAndQueue): Double = paxTypeAndQueue match {
//    case PaxTypeAndQueue(PaxTypes.eeaMachineReadable, Queues.eeaDesk) => 20d / 60d
//    case PaxTypeAndQueue(PaxTypes.eeaMachineReadable, Queues.eGate) => 35d / 60d
//    case PaxTypeAndQueue(PaxTypes.eeaNonMachineReadable, Queues.eeaDesk) => 50d / 60d
//    case PaxTypeAndQueue(PaxTypes.visaNational, Queues.nonEeaDesk) => 90d / 60d
//    case PaxTypeAndQueue(PaxTypes.nonVisaNational, Queues.nonEeaDesk) => 78d / 60d
//  }

  def getWorkloadsByTerminal(flights: Future[List[ApiFlight]]): Future[TerminalQueueWorkloads] = {
    val flightsByTerminalFut: Future[Map[TerminalName, List[ApiFlight]]] = flights.map(fs => {
      val flightsByTerminal = fs.filterNot(freightOrEngineering).groupBy(_.Terminal)
      flightsByTerminal
    })
    val plc = PaxLoadCalculator.queueWorkloadCalculator(splitRatioProvider, procTimesProvider("T1")) _

    val workloadByTerminal: Future[Map[String, Map[QueueName, (Seq[WL], Seq[Pax])]]] = flightsByTerminalFut.map((flightsByTerminal: Map[TerminalName, List[ApiFlight]]) =>
      flightsByTerminal.map((fbt: (TerminalName, List[ApiFlight])) => {
      log.info(s"Got flights by terminal ${fbt}")
      val terminal = fbt._1
      val flights = fbt._2
      (terminal -> plc(flights))
    }))

    workloadByTerminal
  }

  def freightOrEngineering(flight: ApiFlight): Boolean = Set("FRT", "ENG").contains(flight.Terminal)
}

