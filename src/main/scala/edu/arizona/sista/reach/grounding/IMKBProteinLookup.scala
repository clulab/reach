package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.grounding.ReachKBKeyTransforms._

/**
  * Trait implementing alternate key lookups for protein knowledge bases.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Redo to return resolution sequences.
  */
trait IMKBProteinLookup extends IMKBLookup {

  /** Resolve the given text string to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Resolutions =
    resolveAlt(text, proteinKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpecies (text:String, species:String): Resolutions =
    resolveByASpeciesAlt(text, species, proteinKeyTransforms)

  /** Resolve the given text string to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    */
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Resolutions =
    resolveBySpeciesAlt(text, speciesSet, proteinKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (text:String): Resolutions =
    resolveHumanAlt(text, proteinKeyTransforms)

  /** Resolve the given text string to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpecies (text:String): Resolutions =
    resolveNoSpeciesAlt(text, proteinKeyTransforms)

}
