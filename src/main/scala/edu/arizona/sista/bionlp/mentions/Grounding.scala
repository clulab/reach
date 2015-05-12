package edu.arizona.sista.bionlp.mentions

import edu.arizona.sista.odin.Mention

trait Grounding {
  this: Mention =>

  /** points to an Xref object that represents an entry in an external database */
  var xref: Option[Grounding.Xref] = None

  /** returns true if this mention is grounded */
  def isGrounded: Boolean = xref.isDefined

  def ground(namespace: String, id: String): Unit =
    xref = Some(Grounding.Xref(namespace, id))
}

object Grounding {
  /** Represents an entry in an external database. Used for grounding mentions */
  case class Xref(namespace: String, id: String) {
    /** Return a printable string representation of this Xref. */
    def printString(): String = s"${namespace}:${id}"
  }
}
