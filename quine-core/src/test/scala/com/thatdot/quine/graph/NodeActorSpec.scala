package com.thatdot.quine.graph

import org.scalatest.funsuite.AnyFunSuite

import com.thatdot.quine.graph.NodeChangeEvent.EdgeAdded

class NodeActorSpec extends AnyFunSuite {}

object DedupEventsPerformance extends App {

  val maxEventsSize = 1000
  val iterations = 10000

  val eventGen = HalfEdgeGen.halfEdgeGen

  val events: List[NodeChangeEvent] = (for {
    _ <- 1 to maxEventsSize
    sample = EdgeAdded(eventGen.sample.get)
  } yield sample).toList

  def eventTime() = EventTime.MaxValue
  def hasEffect(x: NodeChangeEvent) = true

  def timeWithCopyThreshold(eventsSize: Int, n: Int): Long = {
    val start = System.currentTimeMillis()
    val eventSample = events.take(eventsSize)
    for (_ <- 0 until iterations) NodeActor.dedupEvents(eventSample, eventTime, hasEffect, n)
    val end = System.currentTimeMillis()
    end - start
  }

  timeWithCopyThreshold(100, n = 0)
  timeWithCopyThreshold(100, n = Int.MaxValue)

  for {
    eventSize <- 1 to 20
    withCopyThreshold0 = timeWithCopyThreshold(eventSize, n = 0)
    withCopyThresholdMax = timeWithCopyThreshold(eventSize, n = Int.MaxValue)
  } println(s"$eventSize,$withCopyThreshold0,$withCopyThresholdMax")

}
