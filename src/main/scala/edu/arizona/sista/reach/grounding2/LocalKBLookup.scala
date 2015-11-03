package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: ResolveHuman returns first. BySpecies return Iterable. Sort methods.
  */
trait LocalKBLookup extends KBLookup with KBKeyTransforms {

  /** The in-memory knowledge base that all lookups will work against. */
  def memoryKB: InMemoryKB

  override def resolve (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookup(key))
  }

  override def resolveByASpecies (text:String, species:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.newResolution(memoryKB.lookupByASpecies(key, species))
  }

  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val entries = memoryKB.lookupBySpecies(key, speciesSet) // find matching entries in memory KB
    return entries.map(_.map(kbe => memoryKB.newResolution(kbe)))
  }

  override def resolveHuman (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val entries = memoryKB.lookupHuman(key) // find matching entries in memory KB
    return memoryKB.newResolution(entries.flatMap(_.headOption)) // return first entry or None
  }

}
