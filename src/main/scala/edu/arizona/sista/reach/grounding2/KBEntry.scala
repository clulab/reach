package edu.arizona.sista.reach.grounding2

/**
  * Class holding information about a specific entry from an external Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Add toString method.
  */
class KBEntry (

  /** Text for this entry, loaded from the external KB. */
  val text: String,

  /** Computed key string, which indexes this entry. */
  val key: String,

  /** The KB reference ID, loaded from the external KB. */
  val id: String,

  /** The species associated with this entry, if any. */
  val species: Option[String] = None,

  /** Alternate IDs which might be found in external input sources. */
  val alternateIds: Option[Set[String]] = None,

  /** Standard nomenclature: some KBs might provide a standardized name alias. */
  val standardName: Option[String] = None

) extends Speciated {

  /** Tell whether this entry has an associated species or not. */
  def hasSpecies(): Boolean = species.isDefined

  /** Tell whether the given ID is already associated with this entry,
      either as the primary or an alternate ID. */
  def hasId (anId:String): Boolean = {
    hasPrimaryId(anId) || hasAlternateId(anId)
  }

  /** Tell whether the given ID is one of the alternate IDs. */
  def hasAlternateId (anId:String): Boolean = alternateIds.exists(_.contains(anId))

  /** Tell whether the given ID is equal to the primary id. */
  def hasPrimaryId (anId:String): Boolean = (anId == id)

  /** Override method to provide logging/debugging printout. */
  override def toString(): String =
    s"<KBEntry: ${text} | ${key} | ${id} | ${species.getOrElse('NONE)}>"
}
