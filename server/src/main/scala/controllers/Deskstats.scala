package controllers

import java.security.SecureRandom
import java.security.cert.X509Certificate
import javax.net.ssl._

import akka.actor.{Actor, ActorLogging}
import drt.shared.{ActualDeskStats, DeskStat, Queues}
import org.slf4j.LoggerFactory
import services.PcpArrival.getClass
import services.SDate

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

case class GetActualDeskStats()

class DeskstatsActor extends Actor with ActorLogging {
  var actualDeskStats = Map[String, Map[String, Map[Long, DeskStat]]]()

  override def receive: Receive = {
    case ActualDeskStats(deskStats) =>
      log.info(s"Received ActualDeskStats")
      actualDeskStats = deskStats
    case GetActualDeskStats() =>
      log.info(s"Sending ActualDeskStats($actualDeskStats) to sender")
      sender ! ActualDeskStats(actualDeskStats)
  }
}

object Deskstats {
  val log = LoggerFactory.getLogger(getClass)

  class NaiveTrustManager extends X509TrustManager {
    override def checkClientTrusted(cert: Array[X509Certificate], authType: String) {}
    override def checkServerTrusted(cert: Array[X509Certificate], authType: String) {}
    override def getAcceptedIssuers = null
  }

  object NaiveTrustManager {
    def getSocketFactory: SSLSocketFactory = {
      val tm = Array[TrustManager](new NaiveTrustManager())
      val context = SSLContext.getInstance("SSL")
      context.init(new Array[KeyManager](0), tm, new SecureRandom())
      context.getSocketFactory
    }
  }

  def blackjackDeskstats(blackjackUrl: String, parseSinceMillis: Long): Map[String, Map[String, Map[Long, DeskStat]]] = {
    val sc = SSLContext.getInstance("SSL")
    sc.init(null, Array(new NaiveTrustManager), new java.security.SecureRandom())
    HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory())
    val backupSslSocketFactory = HttpsURLConnection.getDefaultSSLSocketFactory

    val bufferedCsvContent: BufferedSource = Source.fromURL(blackjackUrl)

    HttpsURLConnection.setDefaultSSLSocketFactory(backupSslSocketFactory)

    val relevantData = csvLinesUntil(bufferedCsvContent, parseSinceMillis)
    csvData(relevantData)
  }

  def csvLinesUntil(csvContent: BufferedSource, until: Long): String = {
    csvContent.getLines().takeWhile(line => {
      val cells: Seq[String] = parseCsvLine(line)
      cells(0) match {
        case "device" => true
        case _ =>
          val (date, time) = (cells(1), cells(2).take(5))
          val Array(day, month, year) = date.split("/")
          val statsDate = SDate(s"$year-$month-${day}T$time:00Z")
          statsDate.millisSinceEpoch > until
      }
    }).mkString("\n")
  }

  def csvHeadings(deskstatsContent: String): Seq[String] = {
    parseCsvLine(deskstatsContent.split("\n").head)
  }

  def desksForQueueByMillis(queueName: String, dateIndex: Int, timeIndex: Int, deskIndex: Int, waitTimeIndex: Int, rows: Seq[Seq[String]]): Map[Long, DeskStat] = {
    rows.map {
      case columnData: Seq[String] =>
        val desksOption = Try {
          columnData(deskIndex).toInt
        } match {
          case Success(d) => Option(d)
          case Failure(f) => None
        }
        val waitTimeOption = Try {
          log.info(s"deskStats waitTime: ${columnData(waitTimeIndex)}, from columnData: ${columnData}")
          val Array(hours, minutes) = columnData(waitTimeIndex).split(":").map(_.toInt)
          (hours * 60) + minutes
        } match {
          case Success(d) => Option(d)
          case Failure(f) => None
        }
        val timeString = columnData(timeIndex).take(5)
        val dateString = {
          val Array(day, month, year) = columnData(dateIndex).split("/")
          s"$year-$month-$day"
        }
        val millis = SDate(s"${dateString}T$timeString").millisSinceEpoch
        millis -> DeskStat(desksOption, waitTimeOption)
    }.toMap
  }


  def csvData(deskstatsContent: String): Map[String, Map[String, Map[Long, DeskStat]]] = {
    val headings = csvHeadings(deskstatsContent)
    val columnIndices = Map(
      "terminal" -> headings.indexOf("device"),
      "date" -> headings.indexOf("Date"),
      "time" -> headings.indexOf("Time")
    )
    val queueColumns = queueColumnIndexes(headings)
    val rows = deskstatsContent.split("\n").drop(1).toList
    val parsedRows = rows.map(parseCsvLine).filter(_.length == 11)
    val dataByTerminal = parsedRows.groupBy(_ (columnIndices("terminal")))
    val dataByTerminalAndQueue =
      dataByTerminal.map {
        case (terminal, rows) =>
          terminal -> queueColumns.map {
            case (queueName, desksAndWaitIndexes) =>
              queueName -> desksForQueueByMillis(queueName, columnIndices("date"), columnIndices("time"), desksAndWaitIndexes("desks"), desksAndWaitIndexes("wait"), rows)
          }
      }

    dataByTerminalAndQueue
  }

  def queueColumnIndexes(headings: Seq[String]) = {
    Map(
      Queues.EeaDesk -> Map(
        "desks" -> headings.indexOf("EEA desks open"),
        "wait" -> headings.indexOf("Queue time EEA")
      ),
      Queues.NonEeaDesk -> Map(
        "desks" -> headings.indexOf("Non EEA desks open"),
        "wait" -> headings.indexOf("Queue time Non EEA")
      ),
      Queues.FastTrack -> Map(
        "desks" -> headings.indexOf("Fast Track desks open"),
        "wait" -> headings.indexOf("Queue time Fast Track")
      )
    )
  }

  def parseCsvLine(line: String): Seq[String] = {
    line.drop(1).dropRight(1).split("\",\"").toList
  }
}
