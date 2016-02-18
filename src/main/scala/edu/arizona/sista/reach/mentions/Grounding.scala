package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.grounding._

/**
  * Trait which holds grounding information for a mention.
  *   Written by: Marco Valenzuela, Tom Hicks.
  *   Last Modified: Redo to hold KB resolutions, define accessors.
  */
trait Grounding {
  this: Mention =>

  /** Points to an object that represents a resolution from an external KB. */
  private var _grounding: Option[KBResolution] = None

  /** List of candidate resolutions, derived from lookups in external KBs. */
  private var _candidates: Resolutions = None

  /** Returns true if this mention is grounded. */
  def isGrounded: Boolean = _grounding.isDefined

  /** Get the current candidate resolutions for this mention, if any. */
  def candidates (): Resolutions = _candidates

  /** Get the current grounding for this mention, if any. */
  def grounding (): Option[KBResolution] = _grounding

  /** Set the final resolution for grounding. */
  def ground (resolution: KBResolution): Unit = {
    _grounding = Some(resolution)
    _candidates = None
  }

  /** Set candidates for eventual final grounding resolution. Resets any grounding. */
  def nominate (resolutions: Resolutions): Unit = {
    if (resolutions.isDefined && !resolutions.get.isEmpty) {  // if at least one candidate
      _candidates = resolutions
      _grounding = Some(resolutions.get.head)
    }
  }

  /** Copy the values from the given Grounding object into this Grounding object. */
  def copyGroundingFrom (other: Grounding): Unit = {
    _grounding = other.grounding
    _candidates = other.candidates
  }

  /** Return a formatted string containing the grounding namespace and ID. */
  def nsId (): String = if (_grounding.isDefined) _grounding.get.nsId else ""
}
