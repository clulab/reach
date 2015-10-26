package edu.arizona.sista.reach.grounding2

/**
  * Trait for simple string lookup in external knowledge bases.
  *   Written by Tom Hicks. 10/22/2015.
  *   Last Modified: Update for species name set.
  */
trait SpeciatedKBLookup extends KBLookup with Speciated {

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the KB entry is not for humans.
    * Return a resolution for the entry, if any found.
    * NB: This default method ignores species and resolves to any species.
    */
  def resolveHuman (text:String): Option[KBEntry] = {
    return resolveBySpecies(text, HumanLabels)  // resolve for humans only
  }

  /** Resolve the given string to an optional entry in a knowledge base,
    * for any of the species names in the given set.
    * Return a resolution for the entry, if any found.
    * NB: This default method ignores species argument and resolves to any species.
    */
  def resolveBySpecies (text:String, species:SpeciesNameSet): Option[KBEntry] = {
    return resolve(text)
  }

}


/** Trait Companion Object allows Mixin OR Import pattern. */
object SpeciatedKBLookup extends SpeciatedKBLookup
