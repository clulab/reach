package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._

/**
  * Trait implementing common logic for local Knowledge Base Mention lookup classes.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Group trait implementations to flatten hierarchy.
  */
trait IMKBMentionLookup extends IMKBLookup with KBMentionLookup with KBAltMentionLookup {

  /** Resolve the given Mention to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (mention:Mention): Option[KBResolution] = resolve(mention.text)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpecies (mention:Mention, species:String): Option[KBResolution] =
    resolveByASpecies(mention.text, species)

  /** Resolve the given Mention to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    */
  override def resolveBySpecies (mention:Mention, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] =
    resolveBySpecies(mention.text, speciesSet)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (mention:Mention): Option[KBResolution] =
    resolveHuman(mention.text)

  /** Resolve the given Mention to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpecies (mention:Mention): Option[KBResolution] =
    resolveNoSpecies(mention.text)


  //
  // Alternate Key Lookups
  //

  /** Resolve the given Mention to an optional entry in a knowledge base.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveAlt (mention:Mention, transforms:KeyTransforms): Option[KBResolution] =
    resolveAlt(mention.text, transforms)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * for the single named species.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpeciesAlt (mention:Mention, species:String,
                                     transforms:KeyTransforms): Option[KBResolution] =
    resolveByASpeciesAlt(mention.text, species, transforms)


  /** Resolve the given Mention to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    */
  override def resolveBySpeciesAlt (mention:Mention, speciesSet:SpeciesNameSet,
                                    transforms:KeyTransforms): Option[Iterable[KBResolution]] =
    resolveBySpeciesAlt(mention.text, speciesSet, transforms)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHumanAlt (mention:Mention, transforms:KeyTransforms): Option[KBResolution] =
    resolveHumanAlt(mention.text, transforms)

  /** Resolve the given Mention to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * If the mention is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpeciesAlt (mention:Mention,
                                    transforms:KeyTransforms): Option[KBResolution] =
    resolveNoSpeciesAlt(mention.text, transforms)

}
