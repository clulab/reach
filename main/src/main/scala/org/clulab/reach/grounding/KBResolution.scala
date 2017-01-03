package org.clulab.reach.grounding

/**
  * Class holding information about a specific resolution from the in-memory Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Hide embedded implementation from users.
  */
class KBResolution (

  /** KB entry containing relevant resolution information. */
  private val entry: KBEntry,

  /** Meta information about the KB from which this resolution was created. */
  val metaInfo: KBMetaInfo = new KBMetaInfo()

) extends Serializable {

  /** Other constructors which do not require access to embedded KBEntry. */
  def this (text: String, key: String, namespace: String, id: String) =
    this(new KBEntry(text, key, namespace, id))

  def this (text: String, key: String, namespace: String, id: String, species: String) =
    this(new KBEntry(text, key, namespace, id, species))


  // Facade functions for field access:
  def text: String = entry.text
  def key: String = entry.key
  def namespace: String = entry.namespace
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
    s"""<KBResolution:|${text}|${key}|${namespace}|${id}|${species}|${metaInfo}|>"""

  /** Override method to provide logging/debugging printout. */
  override def toString: String = s"KBResolution(${key}, ${namespace}, ${id}, ${species})"

}
