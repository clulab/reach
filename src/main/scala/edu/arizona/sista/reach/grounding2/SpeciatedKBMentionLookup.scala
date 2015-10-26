package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._

/**
  * Trait for accessing species-specific information from external knowledge bases.
  *   Written by Tom Hicks. 10/22/2015.
  *   Last Modified: Update for species name set.
  */
trait SpeciatedKBMentionLookup extends SpeciatedKBLookup {

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for the entry, if any found.
    * NB: This default method ignores species and resolves to any species.
    */
  def resolveHuman (mention:Mention): Option[KBEntry] = {
    return resolveBySpecies(mention, HumanLabels)  // resolve for humans only
  }

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * for any of the species name strings in the given set.
    * Return a resolution for the entry, if any found.
    * NB: This default method ignores species argument and resolves to any species.
    */
  def resolveBySpecies (mention:Mention, species:SpeciesNameSet): Option[KBEntry] = {
    return resolve(mention.text)            // ignore species argument: any species accepted
  }

}


/** Trait Companion Object allows Mixin OR Import pattern. */
object SpeciatedKBMentionLookup extends SpeciatedKBMentionLookup
