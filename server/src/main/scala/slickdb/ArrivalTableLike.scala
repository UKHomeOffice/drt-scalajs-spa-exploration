package slickdb

import java.sql.Timestamp

import drt.shared
import drt.shared.Terminals.Terminal

import scala.concurrent.Future

trait ArrivalTableLike {
  def selectAll: AggregatedArrivals

  def removeArrival(number: Int, terminalName: Terminal, scheduledTs: Timestamp): Future[Int]

  def insertOrUpdateArrival(f: shared.api.Arrival): Future[Int]
}
