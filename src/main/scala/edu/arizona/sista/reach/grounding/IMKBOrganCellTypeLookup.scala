package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBKeyTransforms._

/**
  * Trait implementing alternate key lookups for cell types which can be inferred
  * from the organ name and specific contextual suffixes.
  *   Written by Tom Hicks. 12/20/2015.
  *   Last Modified: Initial creation.
  */
trait IMKBOrganCellTypeLookup extends IMKBLookup {

  /** Resolve the given text string to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Option[KBResolution] =
    resolveAlt(text, organCellTypeKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpecies (text:String, species:String): Option[KBResolution] =
    resolveByASpeciesAlt(text, species, organCellTypeKeyTransforms)

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    */
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] =
    resolveBySpeciesAlt(text, speciesSet, organCellTypeKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (text:String): Option[KBResolution] =
    resolveHumanAlt(text, organCellTypeKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpecies (text:String): Option[KBResolution] =
    resolveNoSpeciesAlt(text, organCellTypeKeyTransforms)

}
