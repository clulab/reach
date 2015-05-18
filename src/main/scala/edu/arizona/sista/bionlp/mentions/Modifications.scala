package edu.arizona.sista.bionlp.mentions

import edu.arizona.sista.odin.Mention

trait Modifications {
  this: Mention =>
  var modifications = Set.empty[Modification]

  def isModified: Boolean = modifications.nonEmpty
}

sealed trait Modification {
  // modifications should at least have a label that explains
  // what kind of modification they are
  def label: String
}

case class PTM(
  label: String,
  evidence: Option[Mention] = None,
  site: Option[Mention] = None
) extends Modification {
  override def toString: String = {
    val b = new StringBuilder()
    b.append(label)
    if (site.isDefined)
      b.append(" @ " + site.get.text)
    b.toString()
  }
}

case class EventSite(site: Mention) extends Modification {
  val label = "EventSite"
}

case class Negation(evidence: Mention) extends Modification {
  val label = "Negation"
}
