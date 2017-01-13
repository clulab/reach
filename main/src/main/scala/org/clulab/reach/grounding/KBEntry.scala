package org.clulab.reach.grounding

import scala.Serializable
import scala.util.hashing.MurmurHash3._

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.Speciated._

/**
  * Class holding information about a specific entry from an external Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Update for refactor of KB resolution.
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
  val species: String = NoSpeciesValue

) extends Serializable {

  /** Helper method for equals redefinition. */
  def canEqual (other: Any): Boolean = other.isInstanceOf[KBEntry]

  /** Redefine instance equality based on matching some fields of this class. */
  override def equals (other: Any): Boolean = other match {
    case that: KBEntry => (
      that.canEqual(this) &&
      this.namespace == that.namespace &&
      this.id == that.id &&
      this.species == that.species
    )
    case _ => false
  }

  /** Redefine hashCode. */
  override def hashCode: Int = {
    val h0 = stringHash("org.clulab.reach.grounding.KBEntry")
    val h1 = mix(h0, namespace.hashCode)
    val h2 = mix(h1, id.hashCode)
    val h3 = mixLast(h2, species.hashCode)
    finalizeHash(h3, 4)
  }

  /** Tell whether this entry has an associated species or not. */
  def hasSpecies: Boolean = (species != NoSpeciesValue)
  def hasNoSpecies: Boolean = (species == NoSpeciesValue)

  /** Return a formatted string containing this entry's namespace and ID. */
  def nsId: String = ReachKBUtils.makeNamespaceId(namespace, id)

  /** Override method to provide logging/debugging printout. */
  override def toString: String =
    s"<KBEntry: ${text}, ${namespace}, ${id}, ${species}>"
}
