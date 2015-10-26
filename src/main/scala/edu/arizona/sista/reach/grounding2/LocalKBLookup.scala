package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.LocalKBConstants._
import edu.arizona.sista.reach.grounding2.LocalKBUtils._

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Refactor meta information to in-memory KB. Update for species name set.
  */
trait LocalKBLookup extends SpeciatedKBLookup {

  /** The in-memory knowledge base that all lookups will work against. */
  def memoryKB: InMemoryKB

  /** Canonicalize the given text string into a key for both storage and lookup. */
  def makeCanonicalKey (text:String): String = {
    return makeKBCanonKey(text)             // default: use utility fn to canonicalize
  }

  override def resolve (text:String): Option[KBEntry] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.resolve(key)            // look for existing entry
  }

  override def resolveBySpecies (text:String, species:SpeciesNameSet): Option[KBEntry] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memoryKB.resolveBySpecies(key, species) // look for existing entry and species
  }

}
