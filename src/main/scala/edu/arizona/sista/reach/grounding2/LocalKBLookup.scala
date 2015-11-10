package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Update for key transforms class rename.
  */
trait LocalKBLookup extends KBLookup with LocalKBKeyTransforms {

  /** The in-memory knowledge base that all lookups will work against. */
  def memoryKB: InMemoryKB

  /** Resolve the given text string to an optional entry in the in-memory knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookup(key))
  }

  /** Resolve the given text string to an optional entry in the in-memory knowledge base,
    * for the single named species. Return a resolution for the entry, if any found.
    */
  override def resolveByASpecies (text:String, species:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookupByASpecies(key, species))
  }

  /** Resolve the given string to an optional group of entries in a knowledge base,
    * returning resolutions for any species entries found in the KB.
    */
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val entries = memoryKB.lookupBySpecies(key, speciesSet) // find matching entries in memory KB
    return entries.map(_.map(kbe => memoryKB.newResolution(kbe)))
  }

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the KB entry is not for humans.
    * Return a resolution for the first human entry, if any found.
    */
  override def resolveHuman (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val entries = memoryKB.lookupHuman(key) // find matching entries in memory KB
    return memoryKB.newResolution(entries.flatMap(_.headOption)) // return first entry or None
  }

}
