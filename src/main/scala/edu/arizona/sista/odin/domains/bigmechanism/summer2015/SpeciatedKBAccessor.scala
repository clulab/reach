package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin._
import edu.arizona.sista.odin.extern.inward._

/**
  * Trait for accessing species-specific information from external knowledge bases.
  *   Written by Tom Hicks. 5/1/2015.
  *   Last Modified: Correct default action for resolve by species.
  */
trait SpeciatedKBAccessor extends ExternalKBAccessor {

  /** Resolve the given Mention to an entry in an external knowledge base,
    * failing if the entry is not for humans,
    * and return a map of keys and property values from that entry.
    * NB: Default method ignores species argument and resolves to any species.
    */
  def resolveHuman (mention:Mention): Map[String,String] = {
    return resolveBySpecies(mention, LocalKBUtils.HumanLabels) // resolve for humans only
  }

  /** Resolve the given Mention to an entry in an external knowledge base,
    * for any of the species name strings in the given set,
    * and return a map of keys and property values from that entry.
    * NB: Default method ignores species argument and resolves to any species.
    */
  def resolveBySpecies (mention:Mention, species:Set[String]): Map[String,String] = {
    return resolve(mention)                 // ignore species argument: any species accepted
  }

}
