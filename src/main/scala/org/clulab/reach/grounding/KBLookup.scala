package org.clulab.reach.grounding

/**
  * Trait for simple and species-specific string lookup in local knowledge bases.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Redo to return resolution sequences.
  */
trait KBLookup extends Speciated {

  /** Resolve the given text string to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolve (text:String): Resolutions

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveByASpecies (text:String, species:String): Resolutions

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Resolutions

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveHuman (text:String): Resolutions

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveNoSpecies (text:String): Resolutions

}
