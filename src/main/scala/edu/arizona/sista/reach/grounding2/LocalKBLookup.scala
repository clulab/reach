package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.LocalKBConstants._
import edu.arizona.sista.reach.grounding2.LocalKBUtils._

/**
  * Trait implementing common logic for local Knowledge Base lookup classes.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Update for KBEntry.
  */
trait LocalKBLookup extends SpeciatedKBLookup {
  /** The meta-information for the external knowledge base. */
  def metaInfo: KBMetaInfo

  /** The in-memory knowledge base that all lookups will work against. */
  val memKB: InMemoryKB = InMemoryKB()

  /** Canonicalize the given text string into a key for both storage and lookup. */
  def makeCanonicalKey (text:String): String = {
    return makeKBCanonKey(text)             // canonicalize text for KBs
  }

  override def resolve (text:String): Option[KBEntry] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    return memKB.get(key)                   // look for existing entry
  }

  override def resolveBySpecies (text:String, species:SpeciesNames): Option[KBEntry] = {
    return None                             // TODO: IMPLEMENT LATER
  }
}
