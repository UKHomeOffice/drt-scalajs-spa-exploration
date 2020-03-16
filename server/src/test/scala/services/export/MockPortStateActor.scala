package services.`export`

import actors.GetPortStateForTerminal
import akka.actor.Actor
import drt.shared.PortState

class MockPortStateActor(optionalPortState: Option[PortState]) extends Actor {
  override def receive: Receive = {
    case GetPortStateForTerminal(_, _, _) => sender() ! optionalPortState
  }
}
