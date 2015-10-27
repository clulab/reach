package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Update for rename of key transform trait.
  */
trait LocalKBLookup extends SpeciatedKBLookup with KBKeyTransforms {

  /** The in-memory knowledge base that all lookups will work against. */
  def memoryKB: InMemoryKB

  override def resolve (text:String): Option[KBEntry] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.resolve(key)            // look for existing entry
  }

  override def resolveBySpecies (text:String, species:SpeciesNameSet): Option[KBEntry] = {
    val key = makeCanonicalKey(text)               // make a lookup key from the given text
    return memoryKB.resolveBySpecies(key, species) // look for existing entry and species
  }

}
