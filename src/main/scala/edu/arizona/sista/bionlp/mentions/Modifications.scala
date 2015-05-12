package edu.arizona.sista.bionlp.mentions

import scala.collection.mutable
import edu.arizona.sista.odin.Mention

trait Modifications {
  this: Mention =>
  val modifications = new mutable.HashSet[Modification]
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
) extends Modification
