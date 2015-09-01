package edu.arizona.sista.reach.reach.brat

import edu.arizona.sista.struct.Interval

sealed trait Annotation {
  def id: String
  def label: String
}

case class TextBound(id: String, label: String, spans: Seq[Interval], text: String)
extends Annotation {
  def totalSpan: Interval = Interval(spans.map(_.start).min, spans.map(_.end).max)
}

case class Event(id: String, label: String, trigger: String, arguments: Map[String, Seq[String]])
extends Annotation

case class Relation(id: String, label: String, arguments: Map[String, Seq[String]])
extends Annotation

case class Equivalence(id: String, label: String, annotations: Seq[String])
extends Annotation

case class BinaryAttribute(id: String, label: String, annotation: String)
extends Annotation

case class MultiValueAttribute(id: String, label: String, annotation: String, value: String)
extends Annotation

case class Normalization(id: String, label: String, annotation: String, resource: String, entry: String, text: String)
extends Annotation
