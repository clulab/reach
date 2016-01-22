package edu.arizona.sista.reach.grounding

/**
  * Class holding information about a specific resolution from the in-memory Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Update for removal of alternate IDs.
  */
class KBResolution (

  /** KB entry containing relevant resolution information. */
  val entry: KBEntry,

  /** Meta information about the KB from which this resolution was created. */
  val metaInfo: KBMetaInfo

) {

  // Facade functions for field access
  def namespace: String = entry.namespace
  def text: String = entry.text
  def key: String = entry.key
  def id: String = entry.id
  def species: String = entry.species
  def standardName: Option[String] = entry.standardName

  /** Override method to provide logging/debugging printout. */
  override def toString(): String =
    s"<KBResolution: ${metaInfo}  ${text} | ${key} | ${namespace} | ${id} | ${species}>"

}
