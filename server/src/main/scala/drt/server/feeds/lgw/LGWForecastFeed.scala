package drt.server.feeds.lgw

import java.io.{ByteArrayOutputStream, File, FileReader}
import java.nio.file.FileSystems
import akka.NotUsed
import akka.actor.{ActorSystem, Cancellable}
import akka.stream.scaladsl.Source
import akka.stream.{ActorAttributes, Supervision}
import com.box.sdk.{BoxFile, BoxFolder, _}
import drt.server.feeds.lgw.LGWFeed.log
import drt.shared.Arrival
import org.apache.commons.lang3.StringUtils
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter, ISODateTimeFormat}
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import scala.collection.JavaConversions._
import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class LGWForecastFeed(boxConfigFilePath: String, userId: String, ukBfGalForecastFolderId: String) extends BoxFileConstants {
  val log: Logger = LoggerFactory.getLogger(getClass)
  val filePattern = "^[0-9]{8}-LGWArrForecast.csv"
  val LGW = "LGW"
  val PORT_FORECAST = "Port Forecast"
  val regex: Regex = """(([^,^\"])*(\".*\")*([^,^\"])*)(,|$)""".r
  val MAX_CACHE_ENTRIES = 100
  val accessTokenCache = new InMemoryLRUAccessTokenCache(MAX_CACHE_ENTRIES)
  val ddMMYYYHHMMFormat : DateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm")

  val boxConfig: BoxConfig = getBoxConfig

  getApiConnection


  def getBoxConfig: BoxConfig = {
    val boxFile: File = FileSystems.getDefault.getPath(boxConfigFilePath).toFile
    if (!boxFile.canRead) {
      throw new Exception(s"Could not read Gatwick box config file from $boxConfigFilePath")
    }
    BoxConfig.readFrom(new FileReader(boxFile))
  }

  def getArrivals: Try[List[Arrival]] =
    for {
      client <- getApiConnection
      galFileToDownload <- getTheLatestFileInfo(client)
      theData <- downloadTheData(client, galFileToDownload)
    } yield getArrivalsFromData(galFileToDownload.getName, theData)

  def getArrivalsFromData(fileName: String, theData: String): List[Arrival] = {
    val rows = theData.split("\n")
    if (rows.length <= 1) throw new Exception(s"The latest forecast file '$fileName' has no data.")
    val header = rows.head
    log.debug(s"The header of the CSV file $fileName is: '$header'.")
    if (header.split(",").size != TOTAL_COLUMNS) {
      log.warn(s"The CSV file header has does not have $TOTAL_COLUMNS, This is the header [$header].")
    }
    val body = rows.tail.filterNot(row => StringUtils.isBlank(row.replaceAll(",", "")))
    log.debug(s"The latest forecast file has ${body.length} rows.")
    body.flatMap(toArrival).toList
  }

  private def toArrival(row: String): Option[Arrival] = Try {

    val fields = regex.findAllIn(row).map(field => StringUtils.removeEnd(StringUtils.removeStart(StringUtils.removeEnd(field, ","), "\"" ), "\"")).toList

    def scheduledDateAsIsoString = Try {
      val dateTimeField = StringUtils.trimToEmpty(fields(DATE_TIME))
      ddMMYYYHHMMFormat.parseDateTime(dateTimeField).toString(ISODateTimeFormat.dateTime)
    } match {
      case Success(value) => value
      case Failure(exception) => throw new Exception(s"""Cannot get the scheduled date from "$row".""", exception)
    }

    val scheduledDate = scheduledDateAsIsoString

    new Arrival(Operator = "",
      Status = PORT_FORECAST,
      EstDT = "",
      ActDT = "",
      EstChoxDT = "",
      ActChoxDT = "",
      Gate = "",
      Stand = "",
      MaxPax = fields(MAX_PAX).toInt,
      ActPax = fields(ACTUAL_PAX).toInt,
      TranPax = fields(TRANSFER_PAX).toInt,
      RunwayID = "",
      BaggageReclaimId = "",
      FlightID = 0,
      AirportID = LGW,
      Terminal = fields(5) match {
        case "South" => "S"
        case "North" => "N"
        case unknown => throw new Exception(s"Unknown Terminal in Gatwick Forecast feed $unknown.")
      },
      rawICAO = fields(FLIGHT_NUMBER),
      rawIATA = fields(FLIGHT_NUMBER),
      Origin = fields(AIRPORT_CODE),
      SchDT = scheduledDate,
      Scheduled = SDate.parseString(scheduledDate).millisSinceEpoch,
      PcpTime = 0,
      None)
  } match {
    case Success(arrival) => Some(arrival)
    case Failure(error) =>
      log.error(s"""Cannot parse arrival from "$row".""", error)
      None

  }

  private def downloadTheData(boxAPIConnection: BoxAPIConnection, latestFile: BoxFile#Info): Try[String] =
    Try {
      val file = new BoxFile(boxAPIConnection, latestFile.getID)
      val stream = new ByteArrayOutputStream()
      file.download(stream)
      stream.flush()
      stream.close()
      new String(stream.toByteArray, "UTF-8")
    }

  private def getTheLatestFileInfo(boxAPIConnection: BoxAPIConnection): Try[BoxFile#Info] =
    Try {
      val folder = new BoxFolder(boxAPIConnection, ukBfGalForecastFolderId)

      var csvFiles = ListBuffer[BoxFile#Info]()
      for (itemInfo <- folder) {
        itemInfo match {
          case fileInfo: BoxFile#Info =>
            if (fileInfo.getName.matches(filePattern)) {
              csvFiles.add(fileInfo)
            }
          case _ =>
        }
      }
      csvFiles.sortBy(f => f.getName).reverse.headOption.getOrElse {
        log.error("Cannot find the latest Forecast CSV file")
        throw new Exception("Cannot find the latest Forecast CSV file.")
      }
    }

  def getApiConnection: Try[BoxDeveloperEditionAPIConnection] =
    Try {
      BoxDeveloperEditionAPIConnection.getAppUserConnection(userId, boxConfig, accessTokenCache)
    }
}

trait BoxFileConstants {
  val MAX_PAX = 3
  val ACTUAL_PAX = 10
  val TRANSFER_PAX = 11
  val FLIGHT_NUMBER = 1
  val AIRPORT_CODE = 8
  val DATE_TIME = 17

  val TOTAL_COLUMNS = 17
}

object LGWForecastFeed {

  def apply()(implicit actorSystem: ActorSystem): Source[Seq[Arrival], Cancellable] = {
    val config = actorSystem.settings.config
    val boxConfigFilePath = config.getString("feeds.gatwick.forecast.boxConfigFile")
    val userId = config.getString("feeds.gatwick.forecast.userId")
    val folderId = config.getString("feeds.gatwick.forecast.folderId")
    val initialDelayImmediately = 100 milliseconds
    val pollInterval = 1 hours
    val feed = new LGWForecastFeed(boxConfigFilePath, userId = userId, ukBfGalForecastFolderId = folderId)
    val tickingSource: Source[List[Arrival], Cancellable] = Source.tick(initialDelayImmediately, pollInterval, NotUsed)
      .withAttributes(ActorAttributes.supervisionStrategy(Supervision.restartingDecider))
      .map(_ => {
         feed.getArrivals match {
          case Success(arrivals) =>
            log.info(s"Got forecast Arrivals ${arrivals.size}.")
            arrivals
          case Failure(e: BoxAPIResponseException) =>
            log.error(e.getResponse, e)
            List.empty[Arrival]
          case Failure(t) =>
            log.info(s"Failed to fetch LGW forecast arrivals. $t")
            List.empty[Arrival]
        }
      })

    tickingSource
  }
}
