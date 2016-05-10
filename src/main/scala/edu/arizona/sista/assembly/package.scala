package edu.arizona.sista

import edu.arizona.sista.assembly.representations.EntityEventRepresentation
import edu.arizona.sista.odin.Mention

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
}
