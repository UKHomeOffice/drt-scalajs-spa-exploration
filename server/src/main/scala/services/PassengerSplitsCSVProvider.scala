package services

import java.net.URL

import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import services.workloadcalculator.PassengerQueueTypes.{PaxTypes, Queues}
import services.workloadcalculator.PaxLoadAt.PaxTypeAndQueue
import services.workloadcalculator.SplitRatio
import spatutorial.shared.ApiFlight

import scala.io.Codec

trait PassengerSplitsCSVProvider extends DefaultPassengerSplitRatioProvider {

  private val log = LoggerFactory.getLogger(getClass)

  def csvSplitUrl = ConfigFactory.load.getString("passenger_splits_csv_url")
  val url = new URL(csvSplitUrl)
  lazy val splitCSVRows = PassengerSplitsCSVReader.parseCSV(url)

  override def splitRatioProvider(flight: ApiFlight): List[SplitRatio] = {
    val flightDate = DateTime.parse(flight.SchDT)
    flightDate.monthOfYear().getAsText

    val foundFlights = splitCSVRows.filter(row =>
      row.flightCode == flight.IATA &&
      row.dayOfWeek == flightDate.dayOfWeek().getAsText &&
      row.month == flightDate.monthOfYear().getAsText
    ).toList

    foundFlights match {
      case head :: Nil =>
        log.info(s"Found split for $flight")
        PassengerSplitsCSVReader.parseRow(foundFlights.head)
      case _ =>
        log.info(s"Failed to find split for $flight in CSV - using default")
        super.splitRatioProvider(flight)
    }
  }
}

object PassengerSplitsCSVReader {
  def parseRow(row: SplitCSVRow): List[SplitRatio] = {

    def calcQueueRatio(categoryPercentage: Int, queuePercentage: Int) = (categoryPercentage.toDouble / 100.0) * (queuePercentage.toDouble / 100.0)

    List(
      SplitRatio(
        PaxTypeAndQueue(PaxTypes.eeaMachineReadable, Queues.eeaDesk),
        calcQueueRatio(row.eeaMachineReadable, row.eeaMachineReadaleToDesk)),
      SplitRatio(
        PaxTypeAndQueue(PaxTypes.eeaMachineReadable, Queues.eGate),
        calcQueueRatio(row.eeaMachineReadable, row.eeaMachineReadableToEgate)),
      SplitRatio(
        PaxTypeAndQueue(PaxTypes.eeaNonMachineReadable, Queues.eeaDesk),
        calcQueueRatio(row.eeaNonMachineReadable, row.eeaNonMachineReadableToDesk)),
      SplitRatio(
        PaxTypeAndQueue(PaxTypes.visaNational, Queues.nonEeaDesk),
        calcQueueRatio(row.visaNationals, row.visaToNonEEA)),
      SplitRatio(
        PaxTypeAndQueue(PaxTypes.nonVisaNational, Queues.nonEeaDesk),
        calcQueueRatio(row.nonVisaNationals, row.nonVisaToNonEEA))
    )
  }

  case class SplitCSVRow(
                          flightCode: String,
                          originPort: String,
                          eeaMachineReadable: Int,
                          eeaNonMachineReadable: Int,
                          nonVisaNationals: Int,
                          visaNationals: Int,
                          eeaMachineReadableToEgate: Int,
                          eeaMachineReadaleToDesk: Int,
                          eeaNonMachineReadableToDesk: Int,
                          nonVisaToFastTrack: Int,
                          nonVisaToNonEEA: Int,
                          visaToFastTrack: Int,
                          visaToNonEEA: Int,
                          transfers: Int,
                          dayOfWeek: String,
                          month: String,
                          port: String,
                          terminal: String,
                          originCountryCode: String
                        )

  def parseCSV(pathToFile: URL): Seq[SplitCSVRow] = {

    val bufferedSource = scala.io.Source.fromURL(pathToFile)(Codec.UTF8)
    val lines = bufferedSource.getLines()
    lines.drop(1).map { l =>
      val splitRow: Array[String] = l.split(",", -1)
      SplitCSVRow(
        splitRow(0),
        splitRow(1),
        splitRow(2).toInt,
        splitRow(3).toInt,
        splitRow(4).toInt,
        splitRow(5).toInt,
        splitRow(6).toInt,
        splitRow(7).toInt,
        splitRow(8).toInt,
        splitRow(9).toInt,
        splitRow(10).toInt,
        splitRow(11).toInt,
        splitRow(12).toInt,
        splitRow(13).toInt,
        splitRow(14),
        splitRow(15),
        splitRow(16),
        splitRow(17),
        splitRow(18)
      )
    }.toSeq
  }
}
