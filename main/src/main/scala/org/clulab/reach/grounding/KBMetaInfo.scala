package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._

/**
  * Abstract base class and concrete class to hold meta data for a knowledge base.
  *   Written by Tom Hicks. 12/11/2016.
  *   Last Modified: Refactor.
  */
abstract class MetaInfo (
  val namespace: String,
  val hasSpeciesInfo: Boolean,
  val isFamilyKB: Boolean,
  val isProteinKB: Boolean
) { }


class KBMetaInfo (

  /** Filename from which the KB data is loaded. */
  val kbFilename: Option[String] = None,    // default for KBs with no file to load

  namespace: String = DefaultNamespace,     // default UAZ namespace
  hasSpeciesInfo: Boolean = false,          // default to KBs without species info
  isFamilyKB: Boolean = false,              // does KB contain protein family entries
  isProteinKB: Boolean = false              // does KB contain protein entries

) extends MetaInfo (namespace, hasSpeciesInfo, isFamilyKB, isProteinKB) {

  /** Override method to provide logging/debugging printout. */
  override def toString: String =
    s"<KBMetaInfo: $namespace, $kbFilename, sp=$hasSpeciesInfo, f=$isFamilyKB, p=$isProteinKB>"

}
