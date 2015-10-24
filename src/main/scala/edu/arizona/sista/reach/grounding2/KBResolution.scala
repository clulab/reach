package edu.arizona.sista.reach.grounding2

/**
  * Class holding information about a specific resolution from an external Knowledge Base.
  *   Written by: Tom Hicks. 10/22/2015.
  *   Last Modified: Initial creation.
  */
class KBResolution (
  /** Meta-information for the KB which spawned this resolution. */
  val metaInfo: KBMetaInfo,

  /** Key string which actually retrieved this resolution. */
  val key: String,

  /** The KB reference ID found in the resolution process. */
  val id: String,

  /** The species associated with this resolution, if any. */
  val species: Option[String] = None,

  /** Alternate IDs which might be returned by the resolution process. */
  val alternateIds: Option[Seq[String]] = None,

  /** Standard nomenclature: some KBs might provide a standardized name alias. */
  val standardName: Option[String] = None

) extends Speciated {

  /** Tell whether this resolution has an associated species or not. */
  def hasSpecies(): Boolean = species.isDefined

}
