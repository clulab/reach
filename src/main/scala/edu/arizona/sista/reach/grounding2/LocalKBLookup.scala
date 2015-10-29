package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Begin lookups in memory KB.
  */
trait LocalKBLookup extends KBLookup with KBKeyTransforms {

  /** The in-memory knowledge base that all lookups will work against. */
  def memoryKB: InMemoryKB

  override def resolve (text:String): Option[KBEntry] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.lookup(key)             // do lookup in memory KB
  }

  override def resolveHuman (text:String): Option[KBEntry] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.lookupHuman(key)        // do lookup in memory KB
  }

  override def resolveByASpecies (text:String, species:String): Option[KBEntry] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.lookupByASpecies(key, species) // do lookup in memory KB
  }

  override def resolveBySpecies (text:String, species:SpeciesNameSet): Option[KBEntry] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.lookupBySpecies(key, species)  // do lookup in memory KB
  }

}
