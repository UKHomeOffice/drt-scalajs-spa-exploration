package drt.client.services

import java.util.UUID

import drt.client.services.JSDateConversions.SDate
import drt.shared.FlightsApi.TerminalName
import drt.shared._

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}


object StaffAssignmentHelper {

  import JSDateConversions._

  def tryStaffAssignment(name: String, terminalName: TerminalName, startDate: String, startTime: String, endTime: String, numberOfStaff: String = "1", createdBy: Option[String] = None): Try[StaffAssignment] = {
    val staffDeltaTry = Try(numberOfStaff.toInt)
    val ymd = startDate.split("/").toVector

    val tryDMY: Try[(Int, Int, Int)] = Try((ymd(0).toInt, ymd(1).toInt, ymd(2).toInt + 2000))

    for {
      dmy <- tryDMY
      (d, m, y) = dmy

      startDtTry: Try[SDateLike] = parseTimeWithStartTime(startTime, d, m, y)
      endDtTry: Try[SDateLike] = parseTimeWithStartTime(endTime, d, m, y)
      startDt <- startDtTry
      endDt <- endDtTry
      staffDelta: Int <- staffDeltaTry
    } yield {
      StaffAssignment(name, terminalName, startDt, adjustEndDateIfEndTimeIsBeforeStartTime(d, m, y, startDt, endDt), staffDelta, createdBy = createdBy)
    }
  }

  def toCsv(assignment: StaffAssignment): String = {
    val startDate: SDateLike = SDate(assignment.startDt)
    val endDate: SDateLike = SDate(assignment.endDt)
    val startDateString = f"${startDate.getDate()}%02d/${startDate.getMonth()}%02d/${startDate.getFullYear - 2000}%02d"
    val startTimeString = f"${startDate.getHours()}%02d:${startDate.getMinutes()}%02d"
    val endTimeString = f"${endDate.getHours()}%02d:${endDate.getMinutes()}%02d"

    s"${assignment.name},${assignment.terminalName},$startDateString,$startTimeString,$endTimeString,${assignment.numberOfStaff}"
  }

  def fixedPointsFormat(staffAssignments: StaffAssignments): String = staffAssignments.assignments.map(fixedPointFormat).mkString("\n")

  def fixedPointFormat(assignment: StaffAssignment): String = {
    val startDate: SDateLike = SDate(assignment.startDt)
    val endDate: SDateLike = SDate(assignment.endDt)
    val startTimeString = f"${startDate.getHours()}%02d:${startDate.getMinutes()}%02d"
    val endTimeString = f"${endDate.getHours()}%02d:${endDate.getMinutes()}%02d"

    s"${assignment.name}, $startTimeString, $endTimeString, ${assignment.numberOfStaff}"
  }

  private def adjustEndDateIfEndTimeIsBeforeStartTime(d: Int, m: Int, y: Int, startDt: SDateLike, endDt: SDateLike): SDateLike = {
    if (endDt.millisSinceEpoch < startDt.millisSinceEpoch) {
      SDate(y, m, d, endDt.getHours(), endDt.getMinutes()).addDays(1)
    }
    else {
      endDt
    }
  }

  private def parseTimeWithStartTime(startTime: String, d: Int, m: Int, y: Int): Try[SDateLike] = {
    Try {
      val startT = startTime.split(":").toVector
      val (startHour, startMinute) = (startT(0).toInt, startT(1).toInt)
      val startDt = SDate(y, m, d, startHour, startMinute)
      startDt
    }
  }
}

case class StaffAssignmentParser(rawStaffAssignments: String) {
  val lines: Array[TerminalName] = rawStaffAssignments.split("\n")
  val parsedAssignments: Array[Try[StaffAssignment]] = lines.map(l => {
    l.replaceAll("([^\\\\]),", "$1\",\"").split("\",\"").toList.map(_.trim)
  })
    .filter(parts => parts.length == 5 || parts.length == 6)
    .map {
      case List(description, terminalName, startDay, startTime, endTime) =>
        StaffAssignmentHelper.tryStaffAssignment(description, terminalName, startDay, startTime, endTime)
      case List(description, terminalName, startDay, startTime, endTime, staffNumberDelta) =>
        StaffAssignmentHelper.tryStaffAssignment(description, terminalName, startDay, startTime, endTime, staffNumberDelta)
    }
}

trait StaffAssignmentService {

  def terminalStaffAt(terminalName: TerminalName, date: MilliDate): Int
}

case class StaffAssignmentServiceWithoutDates(assignments: Seq[StaffAssignment]) extends StaffAssignmentService {

  def terminalStaffAt(terminalName: TerminalName, date: MilliDate): Int = assignments.filter(assignment => {
    assignment.terminalName == terminalName &&
      SDate(date).toHoursAndMinutes() >= SDate(assignment.startDt).toHoursAndMinutes() &&
      SDate(date).toHoursAndMinutes() <= SDate(assignment.endDt).toHoursAndMinutes()
  }).map(_.numberOfStaff).sum
}

case class StaffAssignmentServiceWithDates(assignments: Seq[StaffAssignment]) extends StaffAssignmentService {

  def terminalStaffAt(terminalName: TerminalName, date: MilliDate): Int = assignments.filter(assignment => {
    assignment.startDt <= date && date <= assignment.endDt && assignment.terminalName == terminalName
  }).map(_.numberOfStaff).sum
}

object StaffAssignmentServiceWithoutDates {
  def apply(assignments: Seq[Try[StaffAssignment]]): Try[StaffAssignmentServiceWithoutDates] = {
    if (assignments.exists(_.isFailure))
      Failure(new Exception("Couldn't parse assignments"))
    else {
      Success(StaffAssignmentServiceWithoutDates(assignments.collect { case Success(s) => s }))
    }
  }
}

object StaffAssignmentServiceWithDates {
  def apply(assignments: Seq[Try[StaffAssignment]]): Try[StaffAssignmentServiceWithDates] = {
    if (assignments.exists(_.isFailure))
      Failure(new Exception("Couldn't parse assignments"))
    else {
      Success(StaffAssignmentServiceWithDates(assignments.collect { case Success(s) => s }))
    }
  }
}

object StaffMovements {
  def assignmentsToMovements(staffAssignments: Seq[StaffAssignment]): Seq[StaffMovement] = {
    staffAssignments.flatMap(assignment => {
      val uuid: UUID = UUID.randomUUID()
      StaffMovement(assignment.terminalName, assignment.name + " start", time = assignment.startDt, assignment.numberOfStaff, uuid, createdBy = assignment.createdBy) ::
        StaffMovement(assignment.terminalName, assignment.name + " end", time = assignment.endDt, -assignment.numberOfStaff, uuid, createdBy = assignment.createdBy) :: Nil
    }).sortBy(_.time)
  }

  def adjustmentsAt(movements: Seq[StaffMovement])(dateTime: MilliDate): Int = movements.sortBy(_.time).takeWhile(_.time <= dateTime).map(_.delta).sum

  def terminalStaffAt(assignmentService: StaffAssignmentService)(movements: Seq[StaffMovement])(terminalName: TerminalName, dateTime: MilliDate): Int = {
    val baseStaff = assignmentService.terminalStaffAt(terminalName, dateTime)

    val movementAdjustments = adjustmentsAt(movements.filter(_.terminalName == terminalName))(dateTime)
    baseStaff + movementAdjustments
  }
}
