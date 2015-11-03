package edu.arizona.sista.reach.grounding2

/**
  * Trait for simple and species-specific string lookup in local knowledge bases.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Make KBLookup base trait abstract.
  */
trait KBLookup extends Speciated {

  /** Resolve the given text string to an optional entry in a knowledge base
    * Return a resolution for the entry, if any found.
    * NB: This method assumes a single resolution KB and behavior is undefined
    *     for KBs with multiple entries.
    * NB: This is a non-operative default method to be overridden by each child class.
    */
  def resolve (text:String): Option[KBResolution]

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species. Return a resolution for the entry, if any found.
    * NB: This is a non-operative default method to be overridden by each child class.
    */
  def resolveByASpecies (text:String, species:String): Option[KBResolution]

  /** Resolve the given string to an optional group of entries in a knowledge base,
    * returning a resolution for any species entry found in the KB.
    * NB: This is a non-operative default method to be overridden by each child class.
    */
  def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]]

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the KB entry is not for humans.
    * Return a resolution for a human entry, if any found.
    * NB: This is a non-operative default method to be overridden by each child class.
    */
  def resolveHuman (text:String): Option[KBResolution]

}
