package services.crunch

import drt.shared.CrunchApi.{CrunchMinute, StaffMinute}
import drt.shared.Queues.{EGate, EeaDesk, NonEeaDesk}
import drt.shared.Terminals.T1
import drt.shared.{PortState, Queues, SDateLike}
import org.specs2.matcher.Scope
import org.specs2.mutable.Specification
import services.SDate
import services.exports.Exports

class CrunchMinutesToCSVDataTest extends Specification {

  trait Context extends Scope {
    val header =  """|Date,,Q1,Q1,Q1,Q1,Q1,Q2,Q2,Q2,Q2,Q2,Q3,Q3,Q3,Q3,Q3,Misc,Moves,PCP Staff,PCP Staff
         |,Start,Pax,Wait,Desks req,Act. wait time,Act. desks,Pax,Wait,Desks req,Act. wait time,Act. desks,Pax,Wait,Desks req,Act. wait time,Act. desks,Staff req,Staff movements,Avail,Req"""
  }

  "Given a set of crunch minutes for a terminal, we should receive a CSV for that terminals data" in new Context {
    val startDateTime: SDateLike = SDate("2017-11-10T00:00:00Z")
    val endDateTime: SDateLike = SDate("2017-11-10T00:15:00Z")
    val cms = Iterable(
      CrunchMinute(T1, Queues.EeaDesk, startDateTime.millisSinceEpoch, 1.1, 1, deskRec = 1, 1),
      CrunchMinute(T1, Queues.NonEeaDesk, startDateTime.millisSinceEpoch, 1.3, 1, deskRec = 1, 1),
      CrunchMinute(T1, Queues.EGate, startDateTime.millisSinceEpoch, 1.2, 1, deskRec = 1, 1)
    )

    val sms = Iterable(
      StaffMinute(T1, startDateTime.millisSinceEpoch, 5, fixedPoints = 1, movements = -1)
    )

    val summary: String = Exports.queueSummariesFromPortStateLegacy(Seq(EeaDesk, NonEeaDesk, EGate), 15)(startDateTime, endDateTime, PortState(Iterable(), cms, sms)).get.toCsv

    val expected =
      s"""2017-11-10,00:00,1,1,1,,,1,1,1,,,1,1,1,,,1,-1,4,4
         |""".stripMargin

    summary === expected
  }

  "Given a set of crunch minutes for a terminal, we should get the result back in 15 minute increments" in new Context {
    val startDateTime = SDate("2017-11-10T00:00:00Z")
    val endDateTime = SDate("2017-11-10T00:30:00Z")

    val t15mins = startDateTime.addMinutes(15).millisSinceEpoch
    val t14mins = startDateTime.addMinutes(14).millisSinceEpoch
    val t13mins = startDateTime.addMinutes(13).millisSinceEpoch

    val cms: Seq[CrunchMinute] = (0 until 16).flatMap((min: Int) => {
      List(
        CrunchMinute(T1, Queues.EeaDesk, startDateTime.addMinutes(min).millisSinceEpoch, 1.0, 1, deskRec = 1, 1),
        CrunchMinute(T1, Queues.EGate, startDateTime.addMinutes(min).millisSinceEpoch, 1.0, 1, deskRec = 1, 1),
        CrunchMinute(T1, Queues.NonEeaDesk, startDateTime.addMinutes(min).millisSinceEpoch, 1.0, 1, deskRec = 1, 1)
      )
    })

    val sms = (0 until 16).map((min: Int) => {
      StaffMinute(T1, startDateTime.addMinutes(min).millisSinceEpoch, 5, fixedPoints = 1, movements = -1)
    })

    val summary: String = Exports.queueSummariesFromPortStateLegacy(Seq(EeaDesk, NonEeaDesk, EGate), 15)(startDateTime, endDateTime, PortState(Iterable(), cms, sms)).get.toCsv

    val expected =
      s"""2017-11-10,00:00,15,1,1,,,15,1,1,,,15,1,1,,,1,-1,4,4
         |2017-11-10,00:15,1,1,1,,,1,1,1,,,1,1,1,,,1,-1,4,4
         |""".stripMargin

    summary === expected
  }

  "When exporting data, column names and order must exactly match V1" in {
    val startDateTime = SDate("2017-11-10T00:00:00Z")
    val endDateTime = SDate("2017-11-10T00:15:00Z")
    val cms = List(
      CrunchMinute(T1, Queues.EeaDesk, startDateTime.millisSinceEpoch, 1.0, 2.0, deskRec = 1, 100, Option(2), Option(100), Option(2), Option(100)),
      CrunchMinute(T1, Queues.EGate, startDateTime.millisSinceEpoch, 1.0, 2.0, deskRec = 1, 100, Option(2), Option(100), Option(2), Option(100)),
      CrunchMinute(T1, Queues.NonEeaDesk, startDateTime.millisSinceEpoch, 1.0, 2.0, deskRec = 1, 100, Option(2), Option(100), Option(2), Option(100))
    )

    val sms = List(
      StaffMinute(T1, startDateTime.millisSinceEpoch, 5, fixedPoints = 1, movements = -1)
    )

    val expected =
      """2017-11-10,00:00,1,100,1,100,2,1,100,1,100,2,1,100,1,100,2,1,-1,4,4
        |""".stripMargin

    val summary: String = Exports.queueSummariesFromPortStateLegacy(Seq(EeaDesk, NonEeaDesk, EGate), 15)(startDateTime, endDateTime, PortState(Iterable(), cms, sms)).get.toCsv

    summary === expected
  }

  "When exporting data without headings, then we should not get headings back" >> {
    val startDateTime = SDate("2017-11-10T00:00:00Z")
    val endDateTime = SDate("2017-11-10T00:15:00Z")
    val cms = List(
      CrunchMinute(T1, Queues.EeaDesk, startDateTime.millisSinceEpoch, 1.0, 2.0, deskRec = 1, 100, Option(2), Option(100), Option(2), Option(100)),
      CrunchMinute(T1, Queues.EGate, startDateTime.millisSinceEpoch, 1.0, 2.0, deskRec = 1, 100, Option(2), Option(100), Option(2), Option(100)),
      CrunchMinute(T1, Queues.NonEeaDesk, startDateTime.millisSinceEpoch, 1.0, 2.0, deskRec = 1, 100, Option(2), Option(100), Option(2), Option(100))
    )

    val sms = List(
      StaffMinute(T1, startDateTime.millisSinceEpoch, 5, fixedPoints = 1, movements = -1)
    )

    val expected =
      """2017-11-10,00:00,1,100,1,100,2,1,100,1,100,2,1,100,1,100,2,1,-1,4,4
        |""".stripMargin

    val summary: String = Exports.queueSummariesFromPortStateLegacy(Seq(EeaDesk, NonEeaDesk, EGate), 15)(startDateTime, endDateTime, PortState(Iterable(), cms, sms)).get.toCsv

    summary === expected
  }
}
