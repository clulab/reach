package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.grounding._

/**
  * Trait which holds grounding information for a mention.
  *   Written by: Marco Valenzuela.
  *   Last Modified: Update to hold all candidate resolutions.
  */
trait Grounding {
  this: Mention =>

  /** Points to an Xref object that represents an entry in an external database */
  var xref: Option[Grounding.Xref] = None

  /** List of candidate resolutions, derived from lookups in grounding. */
  var candidates: Resolutions = None

  /** Returns true if this mention is grounded */
  def isGrounded: Boolean = xref.isDefined

  /** Nominate candidates for eventual final grounding resolution. */
  def nominate (resolutions: Resolutions): Unit = {
    if (resolutions.isDefined && !resolutions.get.isEmpty) {  // if at least one candidate
      candidates = resolutions
      val headRes = resolutions.get.head
      xref = Some(Grounding.Xref(headRes.namespace, headRes.id))
    }
  }

  /** Set the final resolution for grounding. */
  def ground (namespace: String, id: String): Unit = {
    xref = Some(Grounding.Xref(namespace, id))
    candidates = None
  }
}

object Grounding {
  /** Represents an entry in an external database. Used for grounding mentions */
  case class Xref(namespace: String, id: String) {
    /** Return a printable string representation of this Xref. */
    def printString(): String = s"${namespace}:${id}"
  }
}
