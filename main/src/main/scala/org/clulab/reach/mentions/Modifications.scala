package org.clulab.reach.mentions

import org.clulab.odin.Mention

trait Modifications {
  this: Mention =>

  var modifications: Set[Modification] = Set.empty

  def isModified: Boolean = modifications.nonEmpty

  def mutants: Set[Mutant] = modifications.filter(_.isInstanceOf[Mutant]).asInstanceOf[Set[Mutant]]
}

sealed trait Modification {
  // modifications should at least have a label that explains
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

case class Mutant(evidence: Mention, foundBy: String) extends Modification{
  val label = evidence.label
  val text = evidence.text

  def isGeneric: Boolean = evidence.toCorefMention.isGeneric

  override def hashCode: Int = evidence.hashCode() * 42 + label.hashCode()
}

case class EventSite(site: Mention) extends Modification {
  val label = "EventSite"
}

case class Negation(evidence: Mention) extends Modification {
  val label = "Negation"
}

case class Hypothesis(evidence: Mention) extends Modification{
  val label = "Hypothesis"
}

case class KDtrigger(evidence: Mention) extends Modification{
  val label = "KDtrigger"
}

case class KOtrigger(evidence: Mention) extends Modification{
  val label = "KOtrigger"
}

case class DNtrigger(evidence: Mention) extends Modification{
  val label = "DNtrigger"
}

case class OEtrigger(evidence: Mention) extends Modification{
  val label = "OEtrigger"
}

case class CHEMtrigger(evidence: Mention) extends Modification{
  val label = "CHEMtrigger"
}

case class UnassignedTrigger(evidence: Mention) extends Modification{
  val label = "UnassignedTrigger"
}

case class DummyMod(text: String) extends Modification {
  val label  = text
}
