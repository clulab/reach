package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.ReachKeyTransforms._

/**
  * Trait implementing alternate key lookups for protein family knowledge bases.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Update for key transform trait rename.
  */
trait IMKBFamilyLookup extends LocalAltKBLookup {

  /** Resolve given text string to an optional entry in the IMKB using alternate
    * key lookups. Return a resolution for the entry, if any found.
    */
  override def resolve (text:String): Option[KBResolution] =
    resolveAlt(text, familyKeyTransforms)


  /** Resolve the given text string to an optional entry in the IMKB, using alternate
    * key lookups, for the single named species. Return a resolution for the entry,
    * if any found.
    */
  override def resolveByASpecies (text:String, species:String): Option[KBResolution] =
    resolveByASpeciesAlt(text, species, familyKeyTransforms)


  /** Resolve the given string to an optional group of entries in the IMKB, using alternate
    * key lookups, returning resolutions for any species entries found in the KB.
    */
  override def resolveBySpecies (text:String,
                                 speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] =
    resolveBySpeciesAlt(text, speciesSet, familyKeyTransforms)


  /** Resolve the given text string to an optional entry in the IMKB, using alternate
    * key lookups, failing if the KB entry is not for humans. Return a resolution
    * for the first human entry, if any found.
    */
  override def resolveHuman (text:String): Option[KBResolution] =
    resolveHumanAlt(text, familyKeyTransforms)

}
