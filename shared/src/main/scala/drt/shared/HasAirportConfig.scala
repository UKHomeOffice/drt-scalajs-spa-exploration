package drt.shared

import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.PaxTypes._
import drt.shared.QueueStatusProviders.QueueStatusProvider
import drt.shared.Queues.{Queue, QueueStatus, _}
import drt.shared.SplitRatiosNs.{SplitRatio, SplitRatios, SplitSources}
import drt.shared.Terminals.Terminal
import ujson.Js.Value
import uk.gov.homeoffice.drt.auth.Roles.Role
import upickle.default
import upickle.default._

import scala.collection.immutable.SortedMap

trait ClassNameForToString {
  override val toString: String = getClass.toString.split("\\$").last
}

object Terminals {

  sealed trait Terminal extends ClassNameForToString with Ordered[Terminal] {
    override def compare(that: Terminal): Int = toString.compare(that.toString)
  }

  object Terminal {
    implicit val rw: ReadWriter[Terminal] = macroRW

    def apply(terminalName: String): Terminal = terminalName.toLowerCase match {
      case "t1" => T1
      case "t2" => T2
      case "t3" => T3
      case "t4" => T4
      case "t5" => T5
      case "a1" => A1
      case "a2" => A2
      case "1i" => T1
      case "2i" => T2
      case "1d" => T1
      case "2d" => T2
      case "5d" => T5
      case "3i" => T3
      case "4i" => T4
      case "5i" => T5
      case "ter" => T1
      case "n" => N
      case "s" => S
      case "mt" => T1
      case "cta" => CTA
      case "mainapron" => MainApron
      case _ => InvalidTerminal
    }
  }

  case object InvalidTerminal extends Terminal {
    override val toString: String = ""
  }

  case object T1 extends Terminal

  case object T2 extends Terminal

  case object T3 extends Terminal

  case object T4 extends Terminal

  case object T5 extends Terminal

  case object A1 extends Terminal

  case object A2 extends Terminal

  case object ACLTER extends Terminal

  case object N extends Terminal

  case object S extends Terminal

  case object MainApron extends Terminal

  case object CTA extends Terminal

}

object Queues {

  sealed trait QueueStatus

  object QueueStatus {
    implicit val rw: ReadWriter[QueueStatus] = macroRW
  }

  case object Open extends QueueStatus

  case object Closed extends QueueStatus

  case class QueueFallbacks(queues: Map[Terminal, Seq[Queue]]) {
    val fallbacks: PartialFunction[(Queue, PaxType), Seq[Queue]] = {
      case (EGate, _: EeaPaxType) => Seq(EeaDesk, QueueDesk, NonEeaDesk)
      case (EGate, _: NonEeaPaxType) => Seq(NonEeaDesk, QueueDesk, EeaDesk)
      case (EeaDesk, _: PaxType) => Seq(EeaDesk, QueueDesk)
      case (NonEeaDesk, _: PaxType) => Seq(EeaDesk, QueueDesk)
      case (_, _) => Seq()
    }

    def availableFallbacks(terminal: Terminal, queue: Queue, paxType: PaxType): Iterable[Queue] = {
      val availableQueues: List[Queue] = queues.get(terminal).toList.flatten
      fallbacks((queue, paxType)).filter(availableQueues.contains)
    }
  }

  sealed trait Queue extends ClassNameForToString with Ordered[Queue] {
    override def compare(that: Queue): Int = toString.compareTo(that.toString)
  }

  object Queue {
    implicit val rw: ReadWriter[Queue] = macroRW

    def apply(queueName: String): Queue = queueName.toLowerCase match {
      case "eeadesk" => EeaDesk
      case "egate" => EGate
      case "noneeadesk" => NonEeaDesk
      case "fasttrack" => FastTrack
      case "transfer" => Transfer
      case "queuedesk" => QueueDesk
      case _ => InvalidQueue
    }
  }

  case object InvalidQueue extends Queue {
    override val toString: String = ""
  }

  case object EeaDesk extends Queue

  case object EGate extends Queue

  case object NonEeaDesk extends Queue

  case object FastTrack extends Queue

  case object Transfer extends Queue

  case object QueueDesk extends Queue

  val queueOrder = List(QueueDesk, EGate, EeaDesk, NonEeaDesk, FastTrack)

  def inOrder(queuesToSort: Seq[Queue]): Seq[Queue] = queueOrder.filter(q => queuesToSort.contains(q))

  val queueDisplayNames: Map[Queue, String] = Map(
    EeaDesk -> "EEA",
    NonEeaDesk -> "Non-EEA",
    EGate -> "e-Gates",
    FastTrack -> "Fast Track",
    Transfer -> "Tx",
    QueueDesk -> "Desk"
  )

  val forecastExportQueueOrderSansFastTrack = List(EeaDesk, NonEeaDesk, EGate)
  val forecastExportQueueOrderWithFastTrack = List(EeaDesk, NonEeaDesk, EGate, FastTrack)

  val deskExportQueueOrderSansFastTrack = List(EeaDesk, EGate, NonEeaDesk)
  val deskExportQueueOrderWithFastTrack = List(EeaDesk, EGate, NonEeaDesk, FastTrack)
  val exportQueueDisplayNames: Map[Queue, String] = Map(
    EeaDesk -> "EEA",
    NonEeaDesk -> "NON-EEA",
    EGate -> "E-GATES",
    FastTrack -> "FAST TRACK"
  )
}

sealed trait PaxType {
  def name: String = getClass.getSimpleName

  def cleanName: String = getClass.getSimpleName.dropRight(1)
}

sealed trait EeaPaxType extends PaxType

sealed trait NonEeaPaxType extends PaxType

object PaxType {
  def apply(paxTypeString: String): PaxType = paxTypeString match {
    case "EeaMachineReadable$" => EeaMachineReadable
    case "EeaNonMachineReadable$" => EeaNonMachineReadable
    case "EeaBelowEGateAge$" => EeaBelowEGateAge
    case "VisaNational$" => VisaNational
    case "NonVisaNational$" => NonVisaNational
    case "B5JPlusNational$" => B5JPlusNational
    case "B5JPlusNationalBelowEGateAge$" => B5JPlusNationalBelowEGateAge
    case "Transit$" => Transit
    case _ => UndefinedPaxType
  }

  implicit val paxTypeReaderWriter: ReadWriter[PaxType] =
    readwriter[Value].bimap[PaxType](paxType => paxType.cleanName, (s: Value) => PaxType(s"${s.str}$$"))
}

object PaxTypes {

  case object EeaMachineReadable extends EeaPaxType

  case object EeaNonMachineReadable extends EeaPaxType

  case object EeaBelowEGateAge extends EeaPaxType

  case object VisaNational extends NonEeaPaxType

  case object NonVisaNational extends NonEeaPaxType

  case object B5JPlusNational extends NonEeaPaxType

  case object B5JPlusNationalBelowEGateAge extends NonEeaPaxType

  case object Transit extends PaxType

  case object UndefinedPaxType extends PaxType

  def displayName(pt: PaxType): String = pt match {
    case EeaMachineReadable => "EEA Machine Readable"
    case EeaNonMachineReadable => "EEA Non-Machine Readable"
    case EeaBelowEGateAge => "EEA Child"
    case VisaNational => "Visa National"
    case NonVisaNational => "Non-Visa National"
    case B5JPlusNational => "B5J+ National"
    case B5JPlusNationalBelowEGateAge => "B5J+ Child"
    case Transit => "Transit"
    case other => other.name
  }

  def displayNameShort(pt: PaxType): String = pt match {
    case EeaMachineReadable => "EEA MRTD"
    case EeaNonMachineReadable => "EEA NMR"
    case EeaBelowEGateAge => "EEA U12"
    case VisaNational => "VN"
    case NonVisaNational => "NVN"
    case B5JPlusNational => "B5J+"
    case B5JPlusNationalBelowEGateAge => "B5J+ U12"
    case Transit => "Transit"
    case other => other.name
  }
}

case class PaxTypeAndQueue(passengerType: PaxType, queueType: Queue) {
  def key = s"${passengerType}_${queueType}"
}

object PaxTypeAndQueue {
  def apply(split: ApiPaxTypeAndQueueCount): PaxTypeAndQueue = PaxTypeAndQueue(split.passengerType, split.queueType)

  implicit val rw: ReadWriter[PaxTypeAndQueue] = macroRW
}

object ProcessingTimes {
  val nationalityProcessingTimes: Map[String, Double] = Map(
    "AUT" -> 22.7, "BEL" -> 22.7, "BGR" -> 22.7, "HRV" -> 22.7, "CYP" -> 22.7, "CZE" -> 22.7, "DNK" -> 22.7,
    "EST" -> 22.7, "FIN" -> 22.7, "FRA" -> 22.7, "DEU" -> 22.7, "HUN" -> 22.7, "IRL" -> 22.7, "LVA" -> 22.7,
    "LTU" -> 22.7, "LUX" -> 22.7, "MLT" -> 22.7, "NLD" -> 22.7, "POL" -> 22.7, "PRT" -> 22.7, "ROU" -> 22.7,
    "SVK" -> 22.7, "SVN" -> 22.7, "ESP" -> 22.7, "SWE" -> 22.7, "GBR" -> 22.7, "GRC" -> 64.0, "ITA" -> 50.5,
    "USA" -> 69.6, "CHN" -> 75.7, "IND" -> 79.0, "AUS" -> 69.5, "CAN" -> 66.6, "SAU" -> 76.3, "JPN" -> 69.5,
    "NGA" -> 79.2, "KOR" -> 70.1, "NZL" -> 69.5, "RUS" -> 79.5, "BRA" -> 86.0, "PAK" -> 82.4, "KWT" -> 80.8,
    "TUR" -> 77.5, "ISR" -> 66.3, "ZAF" -> 78.3, "MYS" -> 69.8, "MEX" -> 82.9, "PHL" -> 86.2, "QAT" -> 79.0,
    "UKR" -> 82.2, "ARG" -> 80.7, "ARE" -> 81.0, "THA" -> 77.8, "TWN" -> 75.2, "SGP" -> 72.0, "EGY" -> 79.8,
    "LKA" -> 72.2, "GHA" -> 87.8, "IRN" -> 77.0, "BGD" -> 80.0, "IDN" -> 82.1, "COL" -> 81.8, "CHL" -> 84.2,
    "KEN" -> 87.5, "BHR" -> 79.9, "XXB" -> 71.9, "LBN" -> 66.2, "MUS" -> 78.3, "OMN" -> 82.9, "DZA" -> 83.7,
    "JAM" -> 84.0, "NPL" -> 77.8, "MAR" -> 83.2, "ALB" -> 69.7, "JOR" -> 77.3, "TTO" -> 84.7, "VNM" -> 87.7,
    "ZWE" -> 75.5, "IRQ" -> 81.3, "SRB" -> 77.2, "BLR" -> 78.3, "KAZ" -> 80.9, "SYR" -> 85.4, "ZIM" -> 77.2,
    "AFG" -> 82.1, "GBN" -> 75.2, "VEN" -> 75.7, "PER" -> 83.2, "UGA" -> 88.8, "TUN" -> 85.3, "SDN" -> 85.1,
    "AZE" -> 80.3, "BRB" -> 85.8, "TZA" -> 82.9, "SLE" -> 93.1, "HKG" -> 72.3, "ERI" -> 92.8, "CMR" -> 85.2,
    "ECU" -> 78.6, "LBY" -> 82.2, "URY" -> 94.5, "CRI" -> 89.1, "ZMB" -> 85.4, "BIH" -> 72.3, "COD" -> 90.2,
    "ISL" -> 28.3, "None" -> 30.0, "MKD" -> 72.6, "GEO" -> 83.4, "AGO" -> 94.8, "GMB" -> 81.3, "UZB" -> 72.6,
    "KNA" -> 83.8, "SOM" -> 90.6, "LCA" -> 89.3, "GRD" -> 105.9
  )
}

object QueueStatusProviders {

  sealed trait QueueStatusProvider {
    def statusAt(terminal: Terminal, queue: Queue, hour: Int): QueueStatus
  }

  object QueueStatusProvider {
    implicit val rw: default.ReadWriter[QueueStatusProvider] = macroRW
  }

  case object QueuesAlwaysOpen extends QueueStatusProvider {
    implicit val rw: ReadWriter[QueueStatusProvider] = macroRW

    override def statusAt(terminal: Terminal, queue: Queue, hour: Int): QueueStatus = Open
  }

  case class HourlyStatuses(statusByTerminalQueueHour: Map[Terminal, Map[Queue, IndexedSeq[QueueStatus]]]) extends QueueStatusProvider {
    override def statusAt(terminal: Terminal, queue: Queue, hour: Int): QueueStatus =
      statusByTerminalQueueHour.get(terminal)
        .flatMap(_.get(queue).flatMap { statuses => statuses.lift(hour % 24) }).getOrElse(Closed)
  }

  object HourlyStatuses {
    implicit val rw: ReadWriter[HourlyStatuses] = macroRW
  }

  case class FlexibleEGatesForSimulation(eGateOpenHours: Seq[Int]) extends QueueStatusProvider {
    override def statusAt(t: Terminal, queue: Queue, hour: Int): QueueStatus =
      (queue, hour) match {
        case (EGate, hour) if !eGateOpenHours.contains(hour) => Closed
        case _ => Open
      }
  }

  object FlexibleEGatesForSimulation {
    implicit val rw: ReadWriter[FlexibleEGatesForSimulation] = macroRW
  }

}

case class AirportConfig(portCode: PortCode,
                         queuesByTerminal: SortedMap[Terminal, Seq[Queue]],
                         divertedQueues: Map[Queue, Queue] = Map(),
                         flexedQueues: Set[Queue] = Set(),
                         slaByQueue: Map[Queue, Int],
                         timeToChoxMillis: Long = 300000L,
                         firstPaxOffMillis: Long = 180000L,
                         defaultWalkTimeMillis: Map[Terminal, Long],
                         terminalPaxSplits: Map[Terminal, SplitRatios],
                         terminalProcessingTimes: Map[Terminal, Map[PaxTypeAndQueue, Double]],
                         minMaxDesksByTerminalQueue24Hrs: Map[Terminal, Map[Queue, (List[Int], List[Int])]],
                         queueStatusProvider: QueueStatusProvider = QueueStatusProviders.QueuesAlwaysOpen,
                         fixedPointExamples: Seq[String] = Seq(),
                         hasActualDeskStats: Boolean = false,
                         eGateBankSizes: Map[Terminal, Iterable[Int]],
                         minutesToCrunch: Int = 1440,
                         crunchOffsetMinutes: Int = 0,
                         hasEstChox: Boolean = true,
                         forecastExportQueueOrder: List[Queue] = Queues.forecastExportQueueOrderSansFastTrack,
                         desksExportQueueOrder: List[Queue] = Queues.deskExportQueueOrderSansFastTrack,
                         contactEmail: Option[String] = None,
                         outOfHoursContactPhone: Option[String] = None,
                         nationalityBasedProcTimes: Map[String, Double] = ProcessingTimes.nationalityProcessingTimes,
                         role: Role,
                         cloneOfPortCode: Option[PortCode] = None,
                         terminalPaxTypeQueueAllocation: Map[Terminal, Map[PaxType, Seq[(Queue, Double)]]],
                         hasTransfer: Boolean = false,
                         maybeCiriumEstThresholdHours: Option[Int] = None,
                         maybeCiriumTaxiThresholdMinutes: Option[Int] = Option(20),
                         feedSources: Seq[FeedSource],
                         feedSourceMonitorExemptions: Seq[FeedSource] = Seq(),
                         desksByTerminal: Map[Terminal, Int],
                         queuePriority: List[Queue] = List(EeaDesk, NonEeaDesk, QueueDesk, FastTrack, EGate),
                         assumedAdultsPerChild: Double = 1.0,
                        ) {
  def assertValid(): Unit = {
    queuesByTerminal.values.flatten.toSet
      .filterNot(_ == Transfer)
      .foreach { queue: Queue =>
        assert(slaByQueue.contains(queue), s"Missing sla for $queue @ $portCode")
      }
    queuesByTerminal.foreach { case (terminal, tQueues) =>
      assert(minMaxDesksByTerminalQueue24Hrs.contains(terminal), s"Missing min/max desks for terminal $terminal @ $portCode")
      tQueues
        .filterNot(_ == Transfer)
        .foreach { tQueue =>
          assert(minMaxDesksByTerminalQueue24Hrs(terminal).contains(tQueue), s"Missing min/max desks for $tQueue for terminal $terminal @ $portCode")
        }
    }
  }

  def minDesksByTerminalAndQueue24Hrs: Map[Terminal, Map[Queue, IndexedSeq[Int]]] = minMaxDesksByTerminalQueue24Hrs.mapValues(_.mapValues(_._1.toIndexedSeq))

  def maxDesksByTerminalAndQueue24Hrs: Map[Terminal, Map[Queue, IndexedSeq[Int]]] = minMaxDesksByTerminalQueue24Hrs.mapValues(_.mapValues(_._2.toIndexedSeq))

  def minDesksForTerminal24Hrs(tn: Terminal): Map[Queue, IndexedSeq[Int]] = minMaxDesksByTerminalQueue24Hrs.getOrElse(tn, Map()).mapValues(_._1.toIndexedSeq)

  def maxDesksForTerminal24Hrs(tn: Terminal): Map[Queue, IndexedSeq[Int]] = minMaxDesksByTerminalQueue24Hrs.getOrElse(tn, Map()).mapValues(_._2.toIndexedSeq)

  val terminals: Iterable[Terminal] = queuesByTerminal.keys

  val terminalSplitQueueTypes: Map[Terminal, Set[Queue]] = terminalPaxSplits.map {
    case (terminal, splitRatios) =>
      (terminal, splitRatios.splits.map(_.paxType.queueType).toSet)
  }

  def queueTypeSplitOrder(terminal: Terminal): List[Queue] = Queues.queueOrder.filter { q =>
    terminalSplitQueueTypes.getOrElse(terminal, Set()).contains(q)
  }

  def feedPortCode: PortCode = cloneOfPortCode.getOrElse(portCode)

  def nonTransferQueues(terminalName: Terminal): Seq[Queue] = queuesByTerminal(terminalName).collect {
    case queue if queue != Queues.Transfer => queue
  }
}

object AirportConfig {
  implicit val rwQueues: ReadWriter[SortedMap[Terminal, Seq[Queue]]] = readwriter[Map[Terminal, Seq[Queue]]].bimap[SortedMap[Terminal, Seq[Queue]]](
    sm => Map[Terminal, Seq[Queue]]() ++ sm,
    m => SortedMap[Terminal, Seq[Queue]]() ++ m
  )

  implicit val rw: ReadWriter[AirportConfig] = macroRW

  def desksByTerminalDefault(minMaxDesksByTerminalQueue: Map[Terminal, Map[Queue, (List[Int], List[Int])]])
                            (terminal: Terminal): List[Int] = minMaxDesksByTerminalQueue.getOrElse(terminal, Map())
    .filterKeys(_ != EGate)
    .map { case (_, (_, max)) => max }
    .reduce[List[Int]] {
      case (max1, max2) => max1.zip(max2).map { case (m1, m2) => m1 + m2 }
    }

  val defaultQueueStatusProvider: (Terminal, Queue, MillisSinceEpoch) => QueueStatus = (_, _, _) => Open
}

case class ContactDetails(supportEmail: Option[String], oohPhone: Option[String])

object ContactDetails {
  implicit val rw: ReadWriter[ContactDetails] = macroRW
}

case class OutOfHoursStatus(localTime: String, isOoh: Boolean)

object OutOfHoursStatus {
  implicit val rw: ReadWriter[OutOfHoursStatus] = macroRW
}

trait HasAirportConfig {
  val airportConfig: AirportConfig
}

object PaxTypesAndQueues {
  val eeaMachineReadableToDesk = PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk)
  val eeaChildToDesk = PaxTypeAndQueue(PaxTypes.EeaBelowEGateAge, Queues.EeaDesk)
  val eeaMachineReadableToEGate = PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EGate)
  val eeaNonMachineReadableToDesk = PaxTypeAndQueue(PaxTypes.EeaNonMachineReadable, Queues.EeaDesk)
  val b5jsskToDesk = PaxTypeAndQueue(PaxTypes.B5JPlusNational, Queues.EeaDesk)
  val b5jsskChildToDesk = PaxTypeAndQueue(PaxTypes.B5JPlusNationalBelowEGateAge, Queues.EeaDesk)
  val b5jsskToEGate = PaxTypeAndQueue(PaxTypes.B5JPlusNational, Queues.EGate)
  val visaNationalToDesk = PaxTypeAndQueue(PaxTypes.VisaNational, Queues.NonEeaDesk)
  val nonVisaNationalToDesk = PaxTypeAndQueue(PaxTypes.NonVisaNational, Queues.NonEeaDesk)
  val visaNationalToFastTrack = PaxTypeAndQueue(PaxTypes.VisaNational, Queues.FastTrack)
  val transitToTransfer = PaxTypeAndQueue(PaxTypes.Transit, Queues.Transfer)
  val nonVisaNationalToFastTrack = PaxTypeAndQueue(PaxTypes.NonVisaNational, Queues.FastTrack)
  val undefinedToEea = PaxTypeAndQueue(PaxTypes.UndefinedPaxType, Queues.EeaDesk)
  val undefinedToNonEea = PaxTypeAndQueue(PaxTypes.UndefinedPaxType, Queues.NonEeaDesk)
  val undefinedToEgate = PaxTypeAndQueue(PaxTypes.UndefinedPaxType, Queues.EGate)
  val undefinedToFastTrack = PaxTypeAndQueue(PaxTypes.UndefinedPaxType, Queues.FastTrack)

  def displayName: Map[PaxTypeAndQueue, String] = Map(
    eeaMachineReadableToEGate -> "eGates",
    eeaMachineReadableToDesk -> "EEA (Machine Readable)",
    eeaChildToDesk -> "EEA child to Desk",
    eeaNonMachineReadableToDesk -> "EEA (Non Machine Readable)",
    b5jsskToDesk -> "B5JSSK to Desk",
    b5jsskChildToDesk -> "B5JSSK child to Desk",
    b5jsskToEGate -> "B5JSSK to eGates",
    visaNationalToDesk -> "Non EEA (Visa)",
    nonVisaNationalToDesk -> "Non EEA (Non Visa)",
    visaNationalToFastTrack -> "Fast Track (Visa)",
    nonVisaNationalToFastTrack -> "Fast Track (Non Visa)",
    transitToTransfer -> "Transfer",
    undefinedToEea -> "EEA (Undefined)",
    undefinedToNonEea -> "Non EEA (Undefined)",
    undefinedToEgate -> "eGates (Undefined)",
    undefinedToEgate -> "eGates (Undefined)",
  )

  val inOrder = List(
    eeaMachineReadableToEGate, eeaMachineReadableToDesk, eeaNonMachineReadableToDesk, visaNationalToDesk, nonVisaNationalToDesk, visaNationalToFastTrack, nonVisaNationalToFastTrack)
}

case class PortCode(iata: String) extends Ordered[PortCode] {
  override def toString: String = iata

  override def compare(that: PortCode): Int = iata.compareTo(that.iata)

  def nonEmpty: Boolean = iata.nonEmpty

  lazy val isDomestic: Boolean = Ports.isDomestic(this)
  lazy val isCta: Boolean = Ports.isCta(this)
  lazy val isDomesticOrCta: Boolean = Ports.isDomesticOrCta(this)
}

object PortCode {
  implicit val rw: ReadWriter[PortCode] = macroRW
}

case class CarrierCode(code: String) {
  override def toString: String = code
}

object CarrierCode {
  implicit val rw: ReadWriter[CarrierCode] = macroRW
}

object AirportConfigs {

  import drt.shared.airportconfig._

  val allPorts: List[AirportConfigLike] = List(Bfs, Bhd, Bhx, Brs, Edi, Ema, Gla, Lcy, Lgw, Lhr, Lpl, Ltn, Man, Ncl, Stn, Test, Test2)
  val testPorts: List[AirportConfigLike] = List(Test, Test2)

  val allPortConfigs: List[AirportConfig] = allPorts.map(_.config)
  val testPortConfigs: List[AirportConfig] = testPorts.map(_.config)

  def portGroups: List[String] = allPortConfigs.filterNot(testPorts.contains).map(_.portCode.toString.toUpperCase).sorted

  val confByPort: Map[PortCode, AirportConfig] = allPortConfigs.map(c => (c.portCode, c)).toMap
}

trait AirportConfigLike {
  val config: AirportConfig
}

object AirportConfigDefaults {
  val defaultSlas: Map[Queue, Int] = Map(
    EeaDesk -> 20,
    EGate -> 25,
    NonEeaDesk -> 45
  )

  import PaxTypesAndQueues._

  val defaultPaxSplits = SplitRatios(
    SplitSources.TerminalAverage,
    SplitRatio(eeaMachineReadableToDesk, 0.1625),
    SplitRatio(eeaMachineReadableToEGate, 0.4875),
    SplitRatio(eeaNonMachineReadableToDesk, 0.1625),
    SplitRatio(visaNationalToDesk, 0.05),
    SplitRatio(nonVisaNationalToDesk, 0.05)
  )

  val defaultQueueRatios: Map[PaxType, Seq[(Queue, Double)]] = Map(
    EeaMachineReadable -> List(Queues.EGate -> 0.8, Queues.EeaDesk -> 0.2),
    EeaBelowEGateAge -> List(Queues.EeaDesk -> 1.0),
    EeaNonMachineReadable -> List(Queues.EeaDesk -> 1.0),
    NonVisaNational -> List(Queues.NonEeaDesk -> 1.0),
    VisaNational -> List(Queues.NonEeaDesk -> 1.0),
    B5JPlusNational -> List(Queues.EGate -> 0.6, Queues.EeaDesk -> 0.4),
    B5JPlusNationalBelowEGateAge -> List(Queues.EeaDesk -> 1)
  )

  val defaultProcessingTimes: Map[PaxTypeAndQueue, Double] = Map(
    eeaMachineReadableToDesk -> 20d / 60,
    eeaMachineReadableToEGate -> 35d / 60,
    eeaNonMachineReadableToDesk -> 50d / 60,
    visaNationalToDesk -> 90d / 60,
    nonVisaNationalToDesk -> 78d / 60
  )
}
