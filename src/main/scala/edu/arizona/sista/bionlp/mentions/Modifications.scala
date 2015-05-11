package edu.arizona.sista.bionlp.mentions

import scala.collection.mutable
import edu.arizona.sista.odin.Mention

trait Modifications {
  this: Mention =>
  val modifications = new mutable.HashSet[Modification]
}

// different modification types
trait Modification
case class PTM(name: String) extends Modification
case class GM(name: String) extends Modification
