package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Class holding information about a specific entry from an external Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Refactor namespace and meta info.
  */
class KBEntry (

  /** Text for this entry, loaded from the external KB. */
  val text: String,

  /** Computed key string, which indexes this entry. */
  val key: String,

  /** The external namespace for this entry (e.g., go, uniprot). */
  val namespace: String = DefaultNamespace,

  /** The reference ID, relative to the namespace for this entry (e.g., GO:0033110, P12345). */
  val id: String,

  /** The species associated with this entry, if any. Empty string represents no species. */
  val species: String = KBEntry.NoSpeciesValue,

  /** Alternate IDs which might be found in external input sources. */
  val alternateIds: Option[Set[String]] = None,

  /** Standard nomenclature: some KBs might provide a standardized name alias. */
  val standardName: Option[String] = None

) extends Speciated {

  /** Tell whether this entry has an associated species or not. */
  def hasSpecies(): Boolean = (species != KBEntry.NoSpeciesValue)
  def hasNoSpecies(): Boolean = (species == KBEntry.NoSpeciesValue)

  /** Tell whether the given ID is already associated with this entry,
      either as the primary or an alternate ID. */
  def hasId (anId:String): Boolean = {
    hasPrimaryId(anId) || hasAlternateId(anId)
  }

  /** Tell whether the given ID is one of the alternate IDs. */
  def hasAlternateId (anId:String): Boolean = alternateIds.exists(_.contains(anId))

  /** Tell whether the given ID is equal to the primary id. */
  def hasPrimaryId (anId:String): Boolean = (anId == id)

  /** Merge the contents of the given entry with this one, returning a new entry. */
  def combine (other:KBEntry, overwriteText:Boolean=false): KBEntry = {
    var altIds = (this.alternateIds ++ other.alternateIds).reduceOption(_ ++ _)
    // if primary IDs are different then add them both as alternates
    if (this.id != other.id)
      altIds = (altIds ++ Some(Set(this.id, other.id))).reduceOption(_ ++ _)
    return new KBEntry(
      if (overwriteText) other.text else this.text,
      this.key,
      if (overwriteText) other.namespace else this.namespace,
      this.id,
      if (this.hasSpecies()) this.species else other.species,
      altIds,
      this.standardName orElse other.standardName orElse None
    )
  }

  /** Override method to provide logging/debugging printout. */
  override def toString(): String =
    s"<KBEntry: ${text} | ${key} | ${namespace} | ${id} | ${species}>"
}


object KBEntry {

  /** Constant which represents the lack of a species in a KB entry. */
  val NoSpeciesValue: String = ""

  /** Tell whether the given string is the special no-species constant or not. */
  def isNoSpeciesValue (species:String): Boolean = (species == NoSpeciesValue)

}
