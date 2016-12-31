package org.clulab.reach.grounding

import org.clulab.odin._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.Speciated._

/**
  * Class implementing logic for local KB Mention lookups on top of base lookups on top of in-memory KB.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Refactor for consistent selfless traits and extension vs imports.
  */
class IMKBMentionLookup (

  /** The in-memory knowledge base that all lookups will work against. */
  memoryKB: InMemoryKB = new InMemoryKB()

) extends IMKBLookup (memoryKB) with KBMentionLookup with KBAltMentionLookup {

  /** Resolve the given Mention to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (mention:Mention): Resolutions = resolve(mention.text)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpecies (mention:Mention, species:String): Resolutions =
    resolveByASpecies(mention.text, species)

  /** Resolve the given Mention to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    */
  override def resolveBySpecies (mention:Mention, speciesSet:SpeciesNameSet): Resolutions =
    resolveBySpecies(mention.text, speciesSet)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (mention:Mention): Resolutions =
    resolveHuman(mention.text)

  /** Resolve the given Mention to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpecies (mention:Mention): Resolutions =
    resolveNoSpecies(mention.text)


  //
  // Alternate Key Lookups
  //

  /** Resolve the given Mention to an optional entry in a knowledge base.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveAlt (mention:Mention, transforms:KeyTransforms): Resolutions =
    resolveAlt(mention.text, transforms)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * for the single named species.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpeciesAlt (mention:Mention, species:String,
                                     transforms:KeyTransforms): Resolutions =
    resolveByASpeciesAlt(mention.text, species, transforms)


  /** Resolve the given Mention to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    */
  override def resolveBySpeciesAlt (mention:Mention, speciesSet:SpeciesNameSet,
                                    transforms:KeyTransforms): Resolutions =
    resolveBySpeciesAlt(mention.text, speciesSet, transforms)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHumanAlt (mention:Mention, transforms:KeyTransforms): Resolutions =
    resolveHumanAlt(mention.text, transforms)

  /** Resolve the given Mention to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpeciesAlt (mention:Mention,
                                    transforms:KeyTransforms): Resolutions =
    resolveNoSpeciesAlt(mention.text, transforms)

}
