package edu.arizona.sista.bionlp.mentions

import edu.arizona.sista.odin.Mention

trait Grounding {
  // only mentions can be grounded
  this: Mention =>

  /** points to an Xref object that represents an entry in an external database */
  // FIXME rename to xref once processors is updated
  var xref2: Option[Xref] = None

  /** returns true if this mention is grounded */
  // FIXME rename to isGrounded once processors is updated
  def isGrounded2: Boolean = xref2.isDefined

  // FIXME rename to ground once processors is updated
  def ground2(namespace: String, id: String): this.type = {
    xref2 = Some(Xref(namespace, id))
    this
  }
}

/** Represents an entry in an external database. Used for grounding mentions */
case class Xref(namespace: String, id: String) {
  /** Return a printable string representation of this Xref. */
  def printString(): String = s"${namespace}:${id}"
}
