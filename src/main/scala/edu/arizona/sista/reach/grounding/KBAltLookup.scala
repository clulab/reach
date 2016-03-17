package edu.arizona.sista.reach.grounding

/**
  * Trait for simple and species-specific string lookup in local knowledge bases
  * using alternate key resolutions.
  *   Written by Tom Hicks. 11/15/2015.
  *   Last Modified: Redo to return resolution sequences.
  */
trait KBAltLookup extends Speciated with KBKeyTransforms {

  /** Resolve the given text string to an optional entry in a knowledge base.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveAlt (text:String, transforms:KeyTransforms): Resolutions

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveByASpeciesAlt (text:String, species:String,
                            transforms:KeyTransforms): Resolutions

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveBySpeciesAlt (text:String, speciesSet:SpeciesNameSet,
                           transforms:KeyTransforms): Resolutions

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for a human entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveHumanAlt (text:String, transforms:KeyTransforms): Resolutions

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveNoSpeciesAlt (text:String, transforms:KeyTransforms): Resolutions
}
