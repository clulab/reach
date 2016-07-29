package org.clulab.reach.mentions

import org.clulab.odin.Mention
import org.clulab.reach.grounding._

/**
  * Trait which holds grounding information for a mention.
  *   Written by: Marco Valenzuela, Tom Hicks.
  *   Last Modified: Propagate ambiguity: comment out finalization methods.
  */
trait Grounding {
  this: Mention =>

  /** Points to an object that represents a resolution from an external KB. */
  private var _grounding: Option[KBResolution] = None

  /** List of candidate resolutions, derived from lookups in external KBs. */
  private var _candidates: Resolutions = None


  /** Get the current candidate resolutions for this mention, if any. */
  def candidates (): Resolutions = _candidates

  /** Copy the values from the given Grounding object into this Grounding object. */
  def copyGroundingFrom (other: Grounding): Unit = {
    _grounding = other.grounding
    _candidates = other.candidates
  }

  /** Get the current grounding for this mention, if any. */
  def grounding (): Option[KBResolution] = _grounding

  /** Set the final resolution for grounding. */
  // def ground (resolution: KBResolution): Unit = {
  //   _grounding = Some(resolution)
  //   _candidates = None                      // final grounding done: remove candidates
  // }

  /** Returns true if this mention has more than one candidate grounding. */
  def hasCandidates: Boolean = _candidates.isDefined

  /** Returns true if this mention is grounded and has more than one candidate grounding. */
  def hasMoreCandidates: Boolean = _candidates.isDefined && (_candidates.get.size > 1)

  /** Returns true if this mention is grounded. */
  def isGrounded: Boolean = _grounding.isDefined

  /** Set candidates for eventual final grounding resolution. Resets any grounding. */
  def nominate (resolutions: Resolutions): Unit = {
    if (resolutions.isDefined && resolutions.get.nonEmpty) {  // if at least one candidate
      _candidates = resolutions
      _grounding = Some(resolutions.get.head)
    }
  }

  /** Return a formatted string containing the grounding namespace and ID. */
  def nsId (): String = if (_grounding.isDefined) _grounding.get.nsId else ""

  /** Select the current grounding as the final grounding. */
  // def selectCurrentGrounding (): Unit = {
  //   assert(this.isGrounded,                 // MUST be grounded before calling this method
  //          s"Mention '${this}' MUST be grounded before selecting current grounding.")
  //   _candidates = None                      // final grounding done: remove candidates
  // }

}
