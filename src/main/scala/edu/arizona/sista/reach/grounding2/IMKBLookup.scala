package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Group trait implementations to flatten hierarchy.
  */
trait IMKBLookup extends KBLookup with KBAltLookup with ReachKBKeyTransforms {

  /** The in-memory knowledge base that all lookups will work against. */
  def memoryKB: InMemoryKB


  /** Resolve the given text string to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Option[KBResolution] = {
    // TODO: FIX IMPLEMENTATION LATER
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookup(key))
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
    // TODO: IMPLEMENT LATER
    return None
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
    // TODO: FIX IMPLEMENTATION LATER
    val key = makeCanonicalKey(text)
    val lcKey = text.toLowerCase            // transform lower cased keys
    val allKeys = key +: makeAlternateKeys(lcKey, transforms)
    memoryKB.newResolution(memoryKB.lookups(allKeys))
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
    val key = makeCanonicalKey(text)
    val lcKey = text.toLowerCase            // transform lower cased keys
    val allKeys = key +: makeAlternateKeys(lcKey, transforms)
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
    val key = makeCanonicalKey(text)
    val lcKey = text.toLowerCase            // transform lower cased keys
    val allKeys = key +: makeAlternateKeys(lcKey, transforms)
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
    val key = makeCanonicalKey(text)
    val lcKey = text.toLowerCase            // transform lower cased keys
    val allKeys = key +: makeAlternateKeys(lcKey, transforms)
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
    // TODO: IMPLEMENT LATER
    return None
  }

}
