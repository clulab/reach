package org.clulab.reach.grounding

import org.clulab.reach.grounding.KBKeyTransforms._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * A record class containing key transforms used to configure an in-memory KB upon instantiation.
  *   Written by: Tom Hicks. 1/1/2017.
  *   Last Modified: Redo arguments and defaults for updated lookup/transform logic.
  */
case class KBKeyTransformsGroup (

  /** Key transforms used to create storage keys upon addition & retrieval of an entry. */
  val baseKTs: KeyTransforms = DefaultKeyTransforms,

  /** Additional lookup key transformations, applied during a query of the KB. */
  val auxKTs: KeyTransforms = NoTransforms,

  /** Normalization lookup key transformations, applied to the results of the additional
      lookup key transforms during a query of the KB. */
  val postKTs: KeyTransforms = IdentityKeyTransforms

)  {

  override def toString: String =
    s"<KBKeyTransformsGroup: $baseKTs, $auxKTs, $postKTs>"

}
