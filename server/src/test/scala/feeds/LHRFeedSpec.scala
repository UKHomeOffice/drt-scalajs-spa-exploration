package feeds

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import drt.server.feeds.lhr.{LHRFlightFeed, LHRLiveFlight}
import drt.shared.{Arrival, LiveFeedSource}
import org.apache.commons.csv.{CSVFormat, CSVParser, CSVRecord}
import org.specs2.mutable.SpecificationLike
import services.SDate

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class LHRFeedSpec extends TestKit(ActorSystem("testActorSystem", ConfigFactory.empty())) with SpecificationLike {

  "lhrCsvToApiFlights" should {
    "Produce an Arrival source with one flight based on a line from the LHR csv" in {
      //
      val csvString =
        """|Term","Flight No","Operator","From","Airport name","Scheduled","Estimated","Touchdown","Est Chocks","Act Chocks","Stand","Max pax","Act Pax","Conn pax"
           |"4","QR005","Qatar Airways","DOH","Doha","22:00 09/03/2017","21:32 09/03/2017","21:33 09/03/2017","21:43 09/03/2017","21:45 09/03/2017","10","795","142","1""""
          .stripMargin
      import akka.pattern.pipe
      import system.dispatcher

      implicit val materializer = ActorMaterializer()
      val csvGetters: Iterator[(Int) => String] = LHRFlightFeed.csvParserAsIteratorOfColumnGetter(csvString)
      val lhrFeed = LHRFlightFeed(csvGetters)

      val probe = TestProbe()

      val flightsSource: Source[List[Arrival], NotUsed] = Source(List(lhrFeed.copiedToApiFlights))

      val futureFlightsSeq: Future[Seq[List[Arrival]]] = flightsSource.runWith(Sink.seq).pipeTo(probe.ref)

      val flights = Await.result(futureFlightsSeq, 3 seconds).asInstanceOf[Vector[Arrival]]

      flights.toList === List(
        List(
          Arrival(
            Operator = Some("Qatar Airways"),
            Status = "UNK",
            Estimated = Some(SDate("2017-03-09T21:32:00.000Z").millisSinceEpoch),
            Actual = Some(SDate("2017-03-09T21:33:00.000Z").millisSinceEpoch),
            EstimatedChox = Some(SDate("2017-03-09T21:43:00.000Z").millisSinceEpoch),
            ActualChox = Some(SDate("2017-03-09T21:45:00.000Z").millisSinceEpoch),
            Gate = None, Stand = Some("10"), MaxPax = Some(795), ActPax = Some(142), TranPax = Some(1), RunwayID = None, BaggageReclaimId = None,
            AirportID = "LHR", Terminal = "T4", rawICAO = "QR005", rawIATA = "QR005", Origin = "DOH",
            Scheduled = SDate("2017-03-09T22:00:00.000Z").millisSinceEpoch,
            PcpTime = Some(SDate("2017-03-09T22:04:00.000Z").millisSinceEpoch), FeedSources = Set(LiveFeedSource)
          )
        )
      )
    }

    "Produce an Arrival source with one flight based on a line with missing values from the LHR csv" in {
      val csvString =
        """|Term","Flight No","Operator","From","Airport name","Scheduled","Estimated","Touchdown","Est Chocks","Act Chocks","Stand","Max pax","Act Pax","Conn pax"
           |"4","KL1033","KLM Royal Dutch Airlines","AMS","Amsterdam","20:50 09/03/2017","20:50 09/03/2017","","","","","","","""""
          .stripMargin
      import akka.pattern.pipe
      import system.dispatcher

      implicit val materializer = ActorMaterializer()

      val csv: CSVParser = CSVParser.parse(csvString, CSVFormat.DEFAULT)
      val csvGetters: Iterator[(Int) => String] = csv.iterator().asScala.map((l: CSVRecord) => (i: Int) => l.get(i))
      val lhrFeed = LHRFlightFeed(csvGetters)


      val probe = TestProbe()
      val flightsSource: Source[List[Arrival], NotUsed] = Source(List(lhrFeed.copiedToApiFlights))
      val futureFlightsSeq: Future[Seq[List[Arrival]]] = flightsSource.runWith(Sink.seq).pipeTo(probe.ref)

      val flights = Await.result(futureFlightsSeq, 3 seconds)

      flights match {
        case Vector(List(flight: Arrival)) =>
          true
        case _ =>
          false
      }
    }

    "should consistently return the same flightid for the same flight" in {
      val flightv1 = LHRLiveFlight("T1", "SA123", "SAA", "JHB", "LHR", org.joda.time.DateTime.parse("2017-01-01T20:00:00z"), None, None, None, None, None, None, None, None)
      val flightv2 = LHRLiveFlight("T1", "SA123", "SAA", "JHB", "LHR", org.joda.time.DateTime.parse("2017-01-01T20:00:00z"), None, None, None, None, None, None, None, None)

      flightv1.flightId() === flightv2.flightId()
    }

    "should not return the same flightid for different flights" in {
      val flightv1 = LHRLiveFlight("T1", "SA324", "SAA", "JHB", "LHR", org.joda.time.DateTime.parse("2017-01-01T20:00:00z"), None, None, None, None, None, None, None, None)
      val flightv2 = LHRLiveFlight("T1", "SA123", "SAA", "JHB", "LHR", org.joda.time.DateTime.parse("2017-01-01T20:00:00z"), None, None, None, None, None, None, None, None)

      flightv1.flightId() !== flightv2.flightId()
    }
  }
}

