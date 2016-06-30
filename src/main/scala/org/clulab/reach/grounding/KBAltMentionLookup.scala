package org.clulab.reach.grounding

import org.clulab.odin._

/**
  * Trait for simple and species-specific Mention lookup in local knowledge bases
  * using alternate key lookups.
  *   Written by Tom Hicks. 11/15/2015.
  *   Last Modified: Redo to return resolution sequences.
  */
trait KBAltMentionLookup extends KBAltLookup {

  /** Resolve the given Mention to an optional entry in a knowledge base.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveAlt (mention:Mention, transforms:KeyTransforms): Resolutions

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * for the single named species.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveByASpeciesAlt (mention:Mention, species:String,
                            transforms:KeyTransforms): Resolutions

  /** Resolve the given Mention to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveBySpeciesAlt (mention:Mention, speciesSet:SpeciesNameSet,
                           transforms:KeyTransforms): Resolutions

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for a human entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveHumanAlt (mention:Mention, transforms:KeyTransforms): Resolutions

  /** Resolve the given Mention to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    * NB: This is an abstract method, to be overridden by each child class.
    */
  def resolveNoSpeciesAlt (mention:Mention, transforms:KeyTransforms): Resolutions
}
