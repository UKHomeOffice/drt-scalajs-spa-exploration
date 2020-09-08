package passengersplits.parsing

import drt.shared.EventTypes.InvalidEventType
import drt.shared._
import org.joda.time.DateTime
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import services.SDate.JodaSDate
import spray.json.{DefaultJsonProtocol, JsNumber, JsString, JsValue, RootJsonFormat}

import scala.util.{Failure, Success, Try}

object VoyageManifestParser {
  def parseVoyagePassengerInfo(content: String): Try[VoyageManifest] = {
    import FlightPassengerInfoProtocol._
    import spray.json._
    Try(content.parseJson.convertTo[VoyageManifest])
  }

  case class PassengerInfo(DocumentType: Option[DocumentType],
                           DocumentIssuingCountryCode: Nationality,
                           Age: Option[PaxAge] = None)

  case class InTransit(isInTransit: Boolean) {
    override def toString: String = if (isInTransit) "Y" else "N"
  }

  object InTransit {
    def apply(inTransitString: String): InTransit = InTransit(inTransitString == "Y")
  }

  case class EeaFlag(value: String)


  case class PassengerInfoJson(DocumentType: Option[DocumentType],
                               DocumentIssuingCountryCode: Nationality,
                               EEAFlag: EeaFlag,
                               Age: Option[PaxAge] = None,
                               DisembarkationPortCode: Option[PortCode],
                               InTransitFlag: InTransit = InTransit(false),
                               DisembarkationPortCountryCode: Option[Nationality] = None,
                               NationalityCountryCode: Option[Nationality] = None,
                               PassengerIdentifier: Option[String]
                              ) {
    def toPassengerInfo = PassengerInfo(DocumentType, DocumentIssuingCountryCode, Age)
  }

  case class VoyageManifests(manifests: Set[VoyageManifest])

  case class ManifestDateOfArrival(date: String) {
    override def toString: String = date
  }

  case class ManifestTimeOfArrival(time: String) {
    override def toString: String = time
  }

  case class VoyageManifest(EventCode: EventType,
                            ArrivalPortCode: PortCode,
                            DeparturePortCode: PortCode,
                            VoyageNumber: VoyageNumberLike,
                            CarrierCode: CarrierCode,
                            ScheduledDateOfArrival: ManifestDateOfArrival,
                            ScheduledTimeOfArrival: ManifestTimeOfArrival,
                            PassengerList: List[PassengerInfoJson]) {
    def flightCode: String = CarrierCode.code + VoyageNumber

    def scheduleArrivalDateTime: Option[SDateLike] = Try(DateTime.parse(scheduleDateTimeString)).toOption.map(JodaSDate)

    def passengerInfos: Seq[PassengerInfo] = PassengerList.map(_.toPassengerInfo)

    def scheduleDateTimeString: String = s"${ScheduledDateOfArrival}T${ScheduledTimeOfArrival}Z"
  }

  object FlightPassengerInfoProtocol extends DefaultJsonProtocol {

    implicit object DocumentTypeJsonFormat extends RootJsonFormat[DocumentType] {
      def write(c: DocumentType) = JsString(c.toString)

      def read(value: JsValue): DocumentType = value match {
        case str: JsString => DocumentType(str.value)
      }
    }

    implicit object NationalityJsonFormat extends RootJsonFormat[Nationality] {
      def write(c: Nationality) = JsString(c.toString)

      def read(value: JsValue): Nationality = value match {
        case str: JsString => Nationality(str.value)
      }
    }

    implicit object PortCodeJsonFormat extends RootJsonFormat[PortCode] {
      def write(c: PortCode) = JsString(c.toString)

      def read(value: JsValue): PortCode = value match {
        case str: JsString => PortCode(str.value)
        case _ => PortCode("")
      }
    }

    implicit object EeaFlagJsonFormat extends RootJsonFormat[EeaFlag] {
      def write(c: EeaFlag) = JsString(c.toString)

      def read(value: JsValue): EeaFlag = value match {
        case str: JsString => EeaFlag(str.value)
        case _ => EeaFlag("")
      }
    }

    implicit object PaxAgeJsonFormat extends RootJsonFormat[Option[PaxAge]] {
      def write(c: Option[PaxAge]): JsNumber = c match {
        case Some(PaxAge(years)) => JsNumber(years)
        case None => JsNumber(-1)
      }

      def read(value: JsValue): Option[PaxAge] = Try(value.convertTo[String].toInt) match {
        case Success(num) if num > 0 => Option(PaxAge(num))
        case _ => None
      }
    }

    implicit object InTransitJsonFormat extends RootJsonFormat[InTransit] {
      def write(c: InTransit) = JsString(if (c.isInTransit) "Y" else "N")

      def read(value: JsValue): InTransit = value match {
        case str: JsString => InTransit(str.value)
        case _ => InTransit(false)
      }
    }

    implicit val passengerInfoConverter: RootJsonFormat[PassengerInfoJson] = jsonFormat(PassengerInfoJson,
      "DocumentType",
      "DocumentIssuingCountryCode",
      "NationalityCountryEEAFlag",
      "Age",
      "DisembarkationPortCode",
      "InTransitFlag",
      "DisembarkationPortCountryCode",
      "NationalityCountryCode",
      "PassengerIdentifier"
    )

    implicit object EventTypeJsonFormat extends RootJsonFormat[EventType] {
      def write(c: EventType) = JsString(c.toString)

      def read(value: JsValue): EventType = value match {
        case str: JsString => EventType(str.value)
        case _ => InvalidEventType
      }
    }

    implicit object CarrierCodeJsonFormat extends RootJsonFormat[CarrierCode] {
      def write(c: CarrierCode) = JsString(c.toString)

      def read(value: JsValue): CarrierCode = value match {
        case str: JsString => CarrierCode(str.value)
      }
    }

    implicit object VoyageNumberJsonFormat extends RootJsonFormat[VoyageNumberLike] {
      def write(c: VoyageNumberLike) = JsString(c.toString)

      def read(value: JsValue): VoyageNumberLike = value match {
        case str: JsString => VoyageNumber(str.value)
        case unexpected => InvalidVoyageNumber(new Exception(s"unexpected json value: $unexpected"))
      }
    }

    implicit object ManifestDateOfArrivalJsonFormat extends RootJsonFormat[ManifestDateOfArrival] {
      def write(c: ManifestDateOfArrival) = JsString(c.toString)

      def read(value: JsValue): ManifestDateOfArrival = value match {
        case str: JsString => ManifestDateOfArrival(str.value)
      }
    }

    implicit object ManifestTimeOfArrivalJsonFormat extends RootJsonFormat[ManifestTimeOfArrival] {
      def write(c: ManifestTimeOfArrival) = JsString(c.toString)

      def read(value: JsValue): ManifestTimeOfArrival = value match {
        case str: JsString => ManifestTimeOfArrival(str.value)
      }
    }

    implicit val passengerInfoResponseConverter: RootJsonFormat[VoyageManifest] = jsonFormat8(VoyageManifest)
  }

}
