package drt.shared

import drt.shared.SplitRatiosNs.SplitRatios
import org.specs2.mutable.Specification

class AirportConfigsSpec extends Specification {

  "AirportConfigs" should {

    "have a list size of 24 of min and max desks by terminal and queue for all ports" in {
      for {
        port <- AirportConfigs.allPorts
        terminalName <- port.minMaxDesksByTerminalQueue.keySet
        queueName <- port.minMaxDesksByTerminalQueue(terminalName).keySet
        (minDesks, maxDesks) = port.minMaxDesksByTerminalQueue(terminalName)(queueName)
      } yield {
        minDesks.size.aka(s"minDesk-> ${port.portCode} -> $terminalName -> $queueName") mustEqual 24
        maxDesks.size.aka(s"maxDesk-> ${port.portCode} -> $terminalName -> $queueName") mustEqual 24
      }
    }

    "Queue names in min max desks by terminal and queues should be defined in Queues" in {
      for {
        port <- AirportConfigs.allPorts
        terminalName <- port.minMaxDesksByTerminalQueue.keySet
        queueName <- port.minMaxDesksByTerminalQueue(terminalName).keySet
      } yield {
        Queues.queueDisplayNames.get(queueName).aka(s"$queueName not found in Queues") mustNotEqual None
      }
    }

    "All Airport config queues must be defined in Queues" in {
      for {
        port <- AirportConfigs.allPorts
        queueName <- port.queues.values.flatten
      } yield {
        Queues.queueDisplayNames.get(queueName).aka(s"$queueName not found in Queues") mustNotEqual None
      }
    }

    "A cloned Airport config should return the portcode of the port it is cloned from when calling feedPortCode" in {
      val clonedConfig = AirportConfig(
        portCode = "LHR_Clone",
        cloneOfPortCode = Option("LHR"),
        queues = Map(),
        slaByQueue = Map(),
        terminalNames = Seq(),
        timeToChoxMillis = 0L,
        firstPaxOffMillis = 0L,
        defaultWalkTimeMillis = Map(),
        defaultPaxSplits = SplitRatios("queue", Nil),
        defaultProcessingTimes = Map(),
        minMaxDesksByTerminalQueue = Map()
      )

      val result = clonedConfig.feedPortCode
      val expected = "LHR"
      result === expected
    }

  }

}