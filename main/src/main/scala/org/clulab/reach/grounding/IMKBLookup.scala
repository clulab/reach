package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.Speciated._

/**
  * Base class merging logic for local Knowledge Base lookups on top of in-memory KB.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Refactor to use key transforms.
  */
class IMKBLookup (

  /** The in-memory knowledge base that all lookups will work against. */
  var memoryKB: InMemoryKB = new InMemoryKB()

) extends KBLookup {

  /** Return a sequence over the entries in this KB. */
  def entries = memoryKB.entries

  /** Return meta information about the external KB from which this KB was created. */
  def metaInfo: IMKBMetaInfo = memoryKB.metaInfo


  /** Resolve the given text string to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Resolutions =
    memoryKB.lookupAll(text)                // find all matching entries in memory KB

  /** Resolve the given text string to optional group of entries in a knowledge base,
    * returning resolutions for all entries found in the KB, for the given species.
    */
  override def resolveByASpecies (text:String, species:String): Resolutions =
    memoryKB.lookupByASpecies(text, species) // find matching entries in memory KB

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all entries found in the KB, with any of the given species.
    */
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Resolutions =
    memoryKB.lookupBySpecies(text, speciesSet) // find matching entries in memory KB

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (text:String): Resolutions =
    memoryKB.lookupHuman(text)              // find matching entries in memory KB

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpecies (text:String): Resolutions =
    memoryKB.lookupNoSpecies(text)          // find matching entries in memory KB
}
