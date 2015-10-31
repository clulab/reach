package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Update to return KB resolutions. Implement lookups in memory KB.
  */
trait LocalKBLookup extends KBLookup with KBKeyTransforms {

  /** The in-memory knowledge base that all lookups will work against. */
  def memoryKB: InMemoryKB

  override def resolve (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookup(key))
  }

  override def resolveHuman (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val entries = memoryKB.lookupHuman(key) // find matching entries in memory KB
    return memoryKB.newResolution(entries.map(_.head)) // return first entry
  }

  override def resolveByASpecies (text:String, species:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookupByASpecies(key, species))
  }

  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val entries = memoryKB.lookupBySpecies(key, speciesSet) // find matching entries in memory KB
    return memoryKB.newResolution(entries.map(_.head))   // return first entry
  }

}
