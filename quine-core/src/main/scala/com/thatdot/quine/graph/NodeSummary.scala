package com.thatdot.quine.graph

import scala.util.hashing.MurmurHash3
import com.thatdot.quine.model.{PropertyValue, QuineId}
import NodeSummary._

case class NodeSummary(count: Long,
                       ids: IdSummary,
                       labels: LabelSummary,
                       properties: PropertySummary)

object NodeSummary {

  type IdSummary = Int
  val zeroIdSummary = 0

  type LabelSummary = Int
  val zeroLabelSummary = 0

  type PropertySummary = Map[Symbol, Int]
  val zeroPropertySummary: Map[Symbol, Int] = Map.empty[Symbol, Int]

  val zero: NodeSummary = NodeSummary(
    count = 0,
    ids = zeroIdSummary,
    labels = zeroLabelSummary,
    properties = zeroPropertySummary)

  def asPropertySummary(properties: Map[Symbol, PropertyValue]): PropertySummary =
    properties.mapValues(propertyValue => MurmurHash3.bytesHash(propertyValue.serialized))

  def asLabelSummary(labels: Option[Set[Symbol]]): LabelSummary =
    labels.fold(0) { symbols =>
      symbols.foldLeft(0) { (z, symbol) =>
        z ^ MurmurHash3.stringHash(symbol.name)
      }
    }

  def apply(id: QuineId, properties: Map[Symbol, PropertyValue], labels: Option[Set[Symbol]]): NodeSummary =
    NodeSummary(
      count = 1,
      ids = MurmurHash3.bytesHash(id.array),
      labels = asLabelSummary(labels),
      properties = asPropertySummary(properties))

  def combine(a: PropertySummary, b: PropertySummary): PropertySummary =
    (a.keySet ++ b.keySet).toSeq.map { key =>
      val combinedHash = (a.get(key), b.get(key)) match {
        case (Some(hash1), Some(hash2)) => hash1 ^ hash2
        case (Some(hash1), None) => hash1
        case (None, Some(hash2)) => hash2
      }
      key -> combinedHash
    }.toMap

  def combine(a: NodeSummary, b: NodeSummary): NodeSummary =
    NodeSummary(
      count = a.count + b.count,
      ids = a.ids ^ b.ids,
      labels = a.labels ^ b.labels,
      properties = combine(a.properties, b.properties))
}
