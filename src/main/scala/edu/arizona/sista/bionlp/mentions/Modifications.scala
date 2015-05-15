package edu.arizona.sista.bionlp.mentions

import scala.collection.mutable
import edu.arizona.sista.odin.Mention

trait Modifications {
  this: Mention =>
  val modifications = new mutable.HashSet[Modification]

  def isModified:Boolean = modifications.size > 0
}

trait Modification {
  // modifications should at least have a label that explains
  // what kind of modification they are
  def label: String
}

case class PTM(
  label: String,
  evidence: Option[Mention] = None,
  site: Option[Mention] = None
) extends Modification {
  override def toString:String = {
    val b = new StringBuilder()
    b.append(label)
    if(site.isDefined)
      b.append(" @ " + site.get.text)
    b.toString()
  }
}

case class EventSite(
  label: String = "UnknownEvent",
  site: Mention) extends Modification {
  override def toString:String =
    s"$label@${site.text}"
}
