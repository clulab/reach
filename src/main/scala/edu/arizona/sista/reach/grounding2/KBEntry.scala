package edu.arizona.sista.reach.grounding2

/**
  * Class holding information about a specific entry from an external Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Implement alternate combine method.
  */
class KBEntry (

  /** Text for this entry, loaded from the external KB. */
  val text: String,

  /** Computed key string, which indexes this entry. */
  val key: String,

  /** The KB reference ID, loaded from the external KB. */
  val id: String,

  /** The species associated with this entry, if any. Empty string represents no species. */
  val species: String = "",

  /** Alternate IDs which might be found in external input sources. */
  val alternateIds: Option[Set[String]] = None,

  /** Standard nomenclature: some KBs might provide a standardized name alias. */
  val standardName: Option[String] = None

) extends Speciated {

  /** Tell whether this entry has an associated species or not. */
  def hasSpecies(): Boolean = (species != "")

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
      this.id,
      if (this.hasSpecies()) this.species else other.species,
      altIds,
      this.standardName orElse other.standardName orElse None
    )
  }

  /** Override method to provide logging/debugging printout. */
  override def toString(): String =
    s"<KBEntry: ${text} | ${key} | ${id} | ${species}>"
}
