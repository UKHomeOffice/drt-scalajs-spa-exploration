package controllers

import controllers.ArrivalGenerator.apiFlight
import drt.shared._
import services.SDate
import utest._

object FlightStateTests extends TestSuite {
  import services.inputfeeds.CrunchTests.{withContext, TestContext}

  def tests = TestSuite {
    "given a flight arriving after the start threshold, " +
      "when we look at the FlightState, " +
      "then we should see that flight" - {
      val startThreshold = SDate("2016-01-01T12:00")
      val newFlights = List(apiFlight(flightId = 1, schDt = "2016-01-01T12:30", estDt = "2016-01-01T12:30", origin = "JFK"))

      withContext() { context =>
        val result = getFlightStateFlightsListFromUpdate(context, startThreshold, newFlights)

        assert(result == newFlights)
      }
    }

    "given one flight arriving after the start threshold and one before, " +
      "when we look at the FlightState, " +
      "then we should only see the one arriving after" - {
      val startThreshold = SDate("2016-01-01T12:00")

      val invalidFlights = List(apiFlight(flightId = 1, schDt = "2016-01-01T11:30", estDt = "2016-01-01T11:30", origin = "JFK"))
      val validFlights = List(apiFlight(flightId = 2, schDt = "2016-01-01T12:30", estDt = "2016-01-01T12:30", origin = "JFK"))
      val newFlights = validFlights ::: invalidFlights

      withContext() { context =>
        val result = getFlightStateFlightsListFromUpdate(context, startThreshold, newFlights)

        assert(result == validFlights)
      }
    }

    "given one flight scheduled after the threshold but with no estimated time, " +
      "when we look at the FlightState, " +
      "then we should see that one flight" - {
      val startThreshold = SDate("2016-01-01T12:00")
      val newFlights = List(
        apiFlight(flightId = 1, schDt = "2016-01-01T12:30", estDt = "", origin = "JFK")
      )

      withContext() { context =>
        val result = getFlightStateFlightsListFromUpdate(context, startThreshold, newFlights)

        assert(result == newFlights)
      }
    }

    "given existing flights before the threshold, " +
      "when new flights arrive, " +
      "then the flight flights should contain no flights before the threshold" - {
      val startThreshold = SDate("2016-01-01T12:00")
      val existingFlights: List[(Int, Arrival)] = List((1, apiFlight(1, "2016-01-01T11:00", "2016-01-01T11:00", origin = "JFK")))
      val newFlights = List(
        apiFlight(flightId = 2, schDt = "2016-01-01T12:30", estDt = "2016-01-01T12:30", origin = "JFK")
      )

      withContext() { context =>
        val flightState = new FlightState {
          override def bestPax(f: Arrival): Int = BestPax.bestPax(f)
          def log = context.system.log
        }
        flightState.setFlights(flightState.flightState ++ existingFlights)

        flightState.onFlightUpdates(newFlights, startThreshold, Seq())

        val result = flightState.flightState.toList.map(_._2)

        assert(result == newFlights)
      }
    }

    "given existing flights after the threshold, " +
      "when new flights arrive, " +
      "then the flight flights should contain old flights arriving after the threshold" - {
      val startThreshold = SDate("2016-01-01T12:00")

      val existingFlightAfterThreshold: Arrival = apiFlight(flightId = 1, schDt = "2016-01-01T13:00", estDt = "2016-01-01T13:00", origin = "JFK")
      val existingFlights: List[(Int, Arrival)] = List((1, existingFlightAfterThreshold))

      val newFlightAfterThreshold = apiFlight(flightId = 2, schDt = "2016-01-01T12:30", estDt = "2016-01-01T12:30", origin = "JFK")
      val newFlights = List(newFlightAfterThreshold)

      withContext() { context =>
        val flightState = new FlightState {
          override def bestPax(f: Arrival): Int = BestPax.bestPax(f)
          def log = context.system.log
        }
        flightState.setFlights(flightState.flightState ++ existingFlights)

        flightState.onFlightUpdates(newFlights, startThreshold, Seq())

        val result = flightState.flightState.toList.map(_._2)

        val expected = newFlightAfterThreshold :: existingFlightAfterThreshold :: Nil

        assert(result.toSet == expected.toSet)
      }
    }

    "given no existing flights, " +
      "when 2 flights arrive - one domestic and one international, " +
      "then the flights should only contain the international flight" - {
      val startThreshold = SDate("1970-01-01T00:00")

      val newDomesticFlight = apiFlight(flightId = 1, schDt = "2016-01-01T12:30", origin = "DUB")
      val newInternationalFlight = apiFlight(flightId = 2, schDt = "2016-01-01T12:30", origin = "JFK")
      val newFlights = List(newDomesticFlight, newInternationalFlight)

      val existingFlights: List[(Int, Arrival)] = List()

      withContext() { context =>
        val flightState = new FlightState {
          override def bestPax(f: Arrival): Int = BestPax.bestPax(f)
          def log = context.system.log
        }

        flightState.setFlights(flightState.flightState ++ existingFlights)
        flightState.onFlightUpdates(newFlights, startThreshold, Seq("DUB"))

        val result = flightState.flightState.toList.map(_._2)

        val expected = newInternationalFlight :: Nil

        assert(result.toSet == expected.toSet)
      }
    }
  }

  def getFlightStateFlightsListFromUpdate(context: TestContext, startThreshold: SDateLike, newFlights: List[Arrival]): List[Arrival] = {
    val flightState = new FlightState {
      override def bestPax(f: Arrival): Int = BestPax.bestPax(f)
      def log = context.system.log
    }

    flightState.onFlightUpdates(newFlights, startThreshold, Seq())

    val result = flightState.flightState.toList.map(_._2)
    result
  }
}
