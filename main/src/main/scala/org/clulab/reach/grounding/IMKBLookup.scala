package org.clulab.reach.grounding

/**
  * Base class merging logic for local Knowledge Base lookups on top of in-memory KB.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Update for refactor of KB meta info.
  */
class IMKBLookup (

  /** The in-memory knowledge base that all lookups will work against. */
  var memoryKB: InMemoryKB = new InMemoryKB()

) extends KBLookup with KBAltLookup with ReachKBKeyTransforms {

  /** Return a sequence over the entries in this KB. */
  def entries = memoryKB.entries

  /** Tell whether this KB contains species information or not. */
  // def hasSpeciesInfo: Boolean = memoryKB.hasSpeciesInfo

  /** Return meta information about the external KB from which this KB was created. */
  def metaInfo: IMKBMetaInfo = memoryKB.metaInfo


  /** Resolve the given text string to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Resolutions = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    memoryKB.lookupAll(key)                 // find all matching entries in memory KB
  }

  /** Resolve the given text string to optional group of entries in a knowledge base,
    * returning resolutions for all entries found in the KB, for the given species.
    */
  override def resolveByASpecies (text:String, species:String): Resolutions = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    memoryKB.lookupByASpecies(key, species) // find matching entries in memory KB
  }

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all entries found in the KB, with any of the given species.
    */
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Resolutions = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    memoryKB.lookupBySpecies(key, speciesSet) // find matching entries in memory KB
  }

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (text:String): Resolutions = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    memoryKB.lookupHuman(key)               // find matching entries in memory KB
  }

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpecies (text:String): Resolutions = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    memoryKB.lookupNoSpecies(key)           // find matching entries in memory KB
  }

  //
  // Alternate Key Lookups
  //

  /** Resolve the given text string to an optional entry in a knowledge base.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveAlt (text:String, transforms:KeyTransforms): Resolutions = {
    val allKeys = reachAlternateKeys(text, transforms)
    memoryKB.lookupsAll(allKeys)
  }

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpeciesAlt (text:String, species:String,
                                     transforms:KeyTransforms): Resolutions =
  {
    val allKeys = reachAlternateKeys(text, transforms)
    memoryKB.lookupsByASpecies(allKeys, species)
  }

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    */
  override def resolveBySpeciesAlt (text:String, speciesSet:SpeciesNameSet,
                                    transforms:KeyTransforms): Resolutions =
  {
    val allKeys = reachAlternateKeys(text, transforms)
    memoryKB.lookupsBySpecies(allKeys, speciesSet)
  }

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHumanAlt (text:String, transforms:KeyTransforms): Resolutions = {
    val allKeys = reachAlternateKeys(text, transforms)
    memoryKB.lookupsHuman(allKeys)
  }

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  def resolveNoSpeciesAlt (text:String, transforms:KeyTransforms): Resolutions = {
    val allKeys = reachAlternateKeys(text, transforms)
    memoryKB.lookupsNoSpecies(allKeys)
  }

}
