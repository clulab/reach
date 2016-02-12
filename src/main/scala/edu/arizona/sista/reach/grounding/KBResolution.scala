package edu.arizona.sista.reach.grounding

/**
  * Class holding information about a specific resolution from the in-memory Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Add NS/ID method. Remove standard name.
  */
class KBResolution (

  /** KB entry containing relevant resolution information. */
  val entry: KBEntry,

  /** Meta information about the KB from which this resolution was created. */
  val metaInfo: Option[KBMetaInfo] = None

) {

  // Facade functions for field access
  def namespace: String = entry.namespace
  def text: String = entry.text
  def key: String = entry.key
  def id: String = entry.id
  def species: String = entry.species

  /** Return a formatted string containing this resolution's namespace and ID. */
  def nsId (): String = s"${namespace}:${id}"

  /** Method to provide logging/debugging printout. */
  def logString(): String =
    s"""<KBResolution: ${text} | ${key} | ${namespace} | ${id} | ${species} | ${metaInfo.getOrElse("")}>"""

  /** Override method to provide logging/debugging printout. */
  override def toString(): String = s"KBResolution(${key}, ${namespace}, ${id}, ${species})"

}
