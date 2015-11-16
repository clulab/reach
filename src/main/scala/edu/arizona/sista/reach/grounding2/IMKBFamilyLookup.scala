package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.ReachKBKeyTransforms._

/**
  * Trait implementing alternate key lookups for protein family knowledge bases.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Update for flattened hierarchy.
  */
trait IMKBFamilyLookup extends IMKBLookup {

  /** Resolve the given text string to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Option[KBResolution] =
    resolveAlt(text, familyKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpecies (text:String, species:String): Option[KBResolution] =
    resolveByASpeciesAlt(text, species, familyKeyTransforms)

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    */
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] =
    resolveBySpeciesAlt(text, speciesSet, familyKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (text:String): Option[KBResolution] =
    resolveHumanAlt(text, familyKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpecies (text:String): Option[KBResolution] =
    resolveNoSpeciesAlt(text, familyKeyTransforms)

}
