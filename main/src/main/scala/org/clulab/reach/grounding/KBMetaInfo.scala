package org.clulab.reach.grounding

import scala.Serializable
import org.clulab.reach.grounding.ReachKBConstants._

/**
  * Class to hold common meta data for different types of knowledge bases.
  *   Written by Tom Hicks. 12/11/2016.
  *   Last Modified: Make this class serializable.
  */
class KBMetaInfo (

  /** The namespace string for this KB. */
  val namespace: String = DefaultNamespace, // default UAZ namespace

  /** Tell if this KB has explicit (multi) or implicit (single) species information or not. */
  val hasSpeciesInfo: Boolean = false,      // default to KBs without species info

  /** Flag identifying this KB as containing protein family entries. */
  val isFamilyKB: Boolean = false,

  /** Flag identifying this KB as containing protein or protein complex entries. */
  val isProteinKB: Boolean = false,

  /** Order number for mention resolution */
  val priority: Int = 1

)  extends Serializable {

  /** Override method to provide logging/debugging printout. */
  override def toString: String =
    s"<KBMetaInfo: $namespace, sp=$hasSpeciesInfo, f=$isFamilyKB, p=$isProteinKB>"
}
