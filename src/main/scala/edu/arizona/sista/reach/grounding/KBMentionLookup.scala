package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._

/**
  * Trait for simple and species-specific Mention lookup in local knowledge bases.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Redo to return resolution sequences.
  */
trait KBMentionLookup extends KBLookup {

  /** Resolve the given Mention to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolve (mention:Mention): Resolutions

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveByASpecies (mention:Mention, species:String): Resolutions

  /** Resolve the given Mention to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveBySpecies (mention:Mention, speciesSet:SpeciesNameSet): Resolutions

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveHuman (mention:Mention): Resolutions

  /** Resolve the given Mention to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveNoSpecies (mention:Mention): Resolutions

}
