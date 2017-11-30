package org.clulab.reach.mentions

import org.clulab.odin.{ Mention, Modification }

trait ReachModifications {
  this: Mention =>

  def isModified: Boolean = modifications.nonEmpty

  def mutants: Set[Mutant] = modifications.filter(_.isInstanceOf[Mutant]).asInstanceOf[Set[Mutant]]
}

sealed trait LabeledModification extends Modification {
  // reach modifications should at least have a label that explains
  // what kind of modification they are
  def label: String

  def matches(query: String): Boolean = this.label == query
}

case class PTM(
  label: String,
  evidence: Option[Mention] = None,
  site: Option[Mention] = None,
  negated: Boolean = false
) extends Modification {
  override def toString: String = {
    val b = new StringBuilder()
    b.append(label)
    if (site.isDefined)
      b.append(" @ " + site.get.text)
    b.toString()
  }
}

case class Mutant(evidence: Mention, foundBy: String) extends LabeledModification {
  val label: String = evidence.label
  val text: String = evidence.text

  def isGeneric: Boolean = evidence.toCorefMention.isGeneric

  override def hashCode: Int = evidence.hashCode() * 42 + label.hashCode()
}

case class EventSite(site: Mention) extends LabeledModification {
  val label = "EventSite"
}

case class Negation(evidence: Mention) extends LabeledModification {
  val label = "Negation"
}

case class Hypothesis(evidence: Mention) extends LabeledModification {
  val label = "Hypothesis"
}
