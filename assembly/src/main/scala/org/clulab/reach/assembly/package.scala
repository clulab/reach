package org.clulab.reach

import org.clulab.reach.assembly.representations.EntityEventRepresentation
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.{ Modification => MentionModification }
import org.clulab.odin.Mention


package object assembly {

  // The type used to store the grounding ID
  // TODO: Should we include species, kb source, etc as part of this ID?
  // this is all available to us from grounding
  type GroundingID = String
  // unique ID associated with each Mention
  type IDPointer = Int
  // a sieve is a function that takes a Seq[Mention] and an AssemblyManager as input and produces an AssemblyManager
  type Sieve = (Seq[Mention], AssemblyManager) => AssemblyManager
  // shorthand for EntityEventRepresentation
  type EER = EntityEventRepresentation
  // At assembly, a Mention's identity is dependent upon its modifications
  type MentionState = (Mention, Set[MentionModification])

  /**
   * Get the state of a Mention
   */
  def getMentionState(m: Mention): MentionState = (m, m.toBioMention.modifications)
}
