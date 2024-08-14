package org.clulab.reach.grounding

import org.clulab.utils.Hash
import org.clulab.reach.context.BoundedPaddingContext.species
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.Speciated._
import org.clulab.utils.Hash

import scala.Serializable

/**
  * Class holding information about a specific resolution from the in-memory Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Remove key field. Compare text canonicalized to lower case.
  */
class KBResolution (

  /** Text for this entry, loaded from the external KB. */
  val text: String,

  /** The external namespace for this entry (e.g., go, uniprot). */
  val namespace: String = DefaultNamespace,

  /** The reference ID, relative to the namespace for this entry (e.g., GO:0033110, P12345). */
  val id: String,

  /** The species associated with this entry, if any. Empty string represents no species. */
  val species: String = NoSpeciesValue,

  /** Meta information about the KB from which this resolution was created. */
  val metaInfo: KBMetaInfo = new KBMetaInfo()

) extends Serializable {

  /** Helper method for equals redefinition. */
  def canEqual (other: Any): Boolean = other.isInstanceOf[KBResolution]

  /** Redefine instance equality based on matching some fields of this class. */
  override def equals (other: Any): Boolean = other match {
    case that: KBResolution => (
      that.canEqual(this) &&
      this.namespace == that.namespace &&
      this.id == that.id &&
      this.text.toLowerCase == that.text.toLowerCase &&
      this.species == that.species
    )
    case _ => false
  }

  /** Redefine hashCode. */
  override def hashCode: Int = Hash.withLast(
    Hash("org.clulab.reach.grounding.KBResolution"),
    text.toLowerCase.hashCode,
    namespace.hashCode,
    id.hashCode,
    species.hashCode
  )

  /** Tell whether this entry has an associated species or not. */
  def hasSpecies: Boolean = (species != NoSpeciesValue)
  def hasNoSpecies: Boolean = (species == NoSpeciesValue)

  /** Return a formatted string containing this resolution's namespace and ID. */
  def nsId: String = ReachKBUtils.makeNamespaceId(namespace, id)

  /** Override method to provide logging/debugging printout. */
  override def toString: String =
    s"<KBResolution: ${text}, ${namespace}, ${id}, ${species}, ${metaInfo}>"

}
