package org.clulab.reach.grounding

import scala.Serializable

/**
  * Class holding information about a specific resolution from the in-memory Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Make class serializable.
  */
class KBResolution (

  /** KB entry containing relevant resolution information. */
  val entry: KBEntry,

  /** Meta information about the KB from which this resolution was created. */
  val metaInfo: Option[KBMetaInfo] = None

) extends Serializable {

  // Facade functions for field access:
  def namespace: String = entry.namespace
  def text: String = entry.text
  def key: String = entry.key
  def id: String = entry.id
  def species: String = entry.species

  /** Facade function: return a formatted string containing this resolution's namespace and ID. */
  def nsId: String = entry.nsId

  /** Facade functions: tell whether this resolution has an associated species or not. */
  def hasSpecies: Boolean = entry.hasSpecies
  def hasNoSpecies: Boolean = entry.hasNoSpecies


  /** Helper method for equals redefinition. */
  def canEqual (other: Any): Boolean = other.isInstanceOf[KBResolution]

  /** Redefine instance equality based on matching of entry's fields. */
  override def equals (other: Any): Boolean = other match {
    case that: KBResolution => (that.canEqual(this) && this.entry.equals(that.entry))
    case _ => false
  }

  /** Redefine hashCode. */
  override def hashCode: Int = entry.hashCode

  /** Method to provide logging/debugging printout. */
  def logString: String =
    s"""<KBResolution:|${text}|${key}|${namespace}|${id}|${species}|${metaInfo.getOrElse("")}|>"""

  /** Override method to provide logging/debugging printout. */
  override def toString: String = s"KBResolution(${key}, ${namespace}, ${id}, ${species})"

}
