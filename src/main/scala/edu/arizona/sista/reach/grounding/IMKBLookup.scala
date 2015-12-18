package edu.arizona.sista.reach.grounding

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Implement resolve which favors human results.
  */
trait IMKBLookup extends KBLookup with KBAltLookup with ReachKBKeyTransforms {

  /** The in-memory knowledge base that all lookups will work against. */
  def memoryKB: InMemoryKB


  /** Resolve the given text string to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Option[KBResolution] = {
    if (!memoryKB.hasSpeciesInfo)           // if KB has species information
      resolveNoSpecies(text)                // then try to resolve the text without species
    else                                    // else prefer human resolution above others
      memoryKB.newResolution(resolveHuman(text) orElse memoryKB.lookupAny(makeCanonicalKey(text)))
  }

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpecies (text:String, species:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookupByASpecies(key, species))
  }

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    */
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val entries = memoryKB.lookupBySpecies(key, speciesSet) // find matching entries in memory KB
    return entries.map(_.map(kbe => memoryKB.newResolution(kbe)))
  }

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val entries = memoryKB.lookupHuman(key) // find matching entries in memory KB
    return memoryKB.newResolution(entries.flatMap(_.headOption)) // return first entry or None
  }

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  def resolveNoSpecies (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookupNoSpecies(key))
  }

  //
  // Alternate Key Lookups
  //

  /** Resolve the given text string to an optional entry in a knowledge base.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveAlt (text:String, transforms:KeyTransforms): Option[KBResolution] = {
    if (!memoryKB.hasSpeciesInfo)           // if KB has no species information
      resolveNoSpeciesAlt(text, transforms) // then try to resolve the text without species
    else {                                  // else prefer human resolution above others
      val allKeys = reachAlternateKeys(text, transforms)
      memoryKB.newResolution(resolveHumanAlt(text, transforms) orElse memoryKB.lookupsAny(allKeys))
    }
  }

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpeciesAlt (text:String, species:String,
                                     transforms:KeyTransforms): Option[KBResolution] =
  {
    val allKeys = reachAlternateKeys(text, transforms)
    memoryKB.newResolution(memoryKB.lookupsByASpecies(allKeys, species))
  }

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    */
  override def resolveBySpeciesAlt (text:String, speciesSet:SpeciesNameSet,
                                    transforms:KeyTransforms): Option[Iterable[KBResolution]] =
  {
    val allKeys = reachAlternateKeys(text, transforms)
    val entries = memoryKB.lookupsBySpecies(allKeys, speciesSet)
    entries.map(_.map(kbe => memoryKB.newResolution(kbe)))
  }

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHumanAlt (text:String, transforms:KeyTransforms): Option[KBResolution] = {
    val allKeys = reachAlternateKeys(text, transforms)
    val entries = memoryKB.lookupsHuman(allKeys)
    memoryKB.newResolution(entries.flatMap(_.headOption)) // return first entry or None
  }

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * If the text is not found, use the given text transforming functions to create
    * and lookup alternate keys.
    * Return a resolution for the entry, if any found.
    */
  def resolveNoSpeciesAlt (text:String, transforms:KeyTransforms): Option[KBResolution] = {
    val allKeys = reachAlternateKeys(text, transforms)
    memoryKB.newResolution(memoryKB.lookupsNoSpecies(allKeys))
  }

}
