package org.clulab.reach.grounding

import scala.Serializable
import scala.util.hashing.MurmurHash3._

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.Speciated._

/**
  * Class holding information about a specific entry from an external Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Refactor for consistent selfless traits and extension vs imports.
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
  val species: String = KBEntry.NoSpeciesValue

) extends Serializable {

  /** Helper method for equals redefinition. */
  def canEqual (other: Any): Boolean = other.isInstanceOf[KBEntry]

  /** Redefine instance equality based on matching some fields of this class. */
  override def equals (other: Any): Boolean = other match {
    case that: KBEntry => (
      that.canEqual(this) &&
      this.key == that.key &&
      this.namespace == that.namespace &&
      this.id == that.id &&
      this.species == that.species
    )
    case _ => false
  }

  /** Redefine hashCode. */
  override def hashCode: Int = {
    val h0 = stringHash("org.clulab.reach.grounding.KBEntry")
    val h1 = mix(h0, key.hashCode)
    val h2 = mix(h1, namespace.hashCode)
    val h3 = mix(h2, id.hashCode)
    val h4 = mixLast(h3, species.hashCode)
    finalizeHash(h4, 4)
  }

  /** Tell whether this entry has an associated species or not. */
  def hasSpecies: Boolean = (species != KBEntry.NoSpeciesValue)
  def hasNoSpecies: Boolean = (species == KBEntry.NoSpeciesValue)

  /** Return a formatted string containing this entry's namespace and ID. */
  def nsId: String = ReachKBUtils.makeNamespaceId(namespace, id)

  /** Override method to provide logging/debugging printout. */
  override def toString: String =
    s"<KBEntry: ${text} | ${key} | ${namespace} | ${id} | ${species}>"
}


object KBEntry {

  /** Constant which represents the lack of a species in a KB entry. */
  val NoSpeciesValue: String = ""

  /** Tell whether the given string is the special no-species constant or not. */
  def isNoSpeciesValue (species:String): Boolean = (species == NoSpeciesValue)

}
