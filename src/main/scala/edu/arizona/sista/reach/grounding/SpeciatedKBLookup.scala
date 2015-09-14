package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.extern.inward.ExternalKBAccessor

/**
  * Trait for simple string lookup in external knowledge bases.
  *   Written by Tom Hicks. 5/14/2015.
  *   Last Modified: Initial creation.
  */
trait SpeciatedKBLookup extends ExternalKBAccessor {

  /** Resolve the given string to a optional string value in an external knowledge base,
    * failing if the KB entry is not for humans,
    * NB: Default method ignores species argument and resolves to any species.
    */
  def resolveHuman (key:String): Option[String] = {
    return resolveBySpecies(key, LocalKBUtils.HumanLabels) // resolve for humans only
  }

  /** Resolve the given string to a optional string value in an external knowledge base,
    * for any of the species name strings in the given set.
    * NB: Default method ignores species argument and resolves to any species.
    */
  def resolveBySpecies (key:String, species:Set[String]): Option[String] = {
    return resolve(key)                     // ignore species argument: any species accepted
  }

  /** Resolve the given string to a optional string value in an external knowledge base.
    * Default method to be overridden by each child knowledge base accessor.
    */
  def resolve (key:String): Option[String] = {
    return None
  }

}
