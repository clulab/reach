package edu.arizona.sista.reach.grounding2

/**
  * Trait for simple and species-specific string lookup in local knowledge bases.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Add method for single species resolution.
  */
trait KBLookup extends Speciated {

  /** Resolve the given text string to an optional entry in a knowledge base
    * Return a resolution for the entry, if any found.
    * NB: This method assumes a single resolution KB and behavior is undefined
    *     for KBs with multiple entries.
    * NB: This is a non-operative default method to be overridden by each child class.
    */
  def resolve (text:String): Option[KBEntry] = None

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the KB entry is not for humans.
    * Return a resolution for the entry, if any found.
    */
  def resolveHuman (text:String): Option[KBEntry] =
    resolveBySpecies(text, HumanLabels)     // resolve for humans only

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species. Return a resolution for the entry, if any found.
    */
  def resolveByASpecies (text:String, species:String): Option[KBEntry] =
    resolveBySpecies(text, SpeciesNameSet(species))

  /** Resolve the given string to an optional entry in a knowledge base,
    * for any of the species names in the given set.
    * Return a resolution for the entry, if any found.
    * NB: This is a non-operative default method to be overridden by each child class.
    * NB: This default method ignores species argument and calls basic resolve.
    */
  def resolveBySpecies (text:String, species:SpeciesNameSet): Option[KBEntry] =
    resolve(text)

}

/** Trait Companion Object allows Mixin OR Import pattern. */
object KBLookup extends KBLookup
