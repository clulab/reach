package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * A record class containing key transforms used to configure an in-memory KB upon instantiation.
  *   Written by: Tom Hicks. 1/1/2017.
  *   Last Modified: Reorder args by usage frequency. Rename base key transforms.
  */
case class IMKBKeyTransforms (

  /** Additional lookup key transformations applied during query of the KB. */
  val queryKTs: KeyTransforms = DefaultKeyTransforms,

  /** Key transforms used to create storage keys upon addition & retrieval of an entry. */
  val baseKTs: KeyTransforms = DefaultKeyTransforms,

  /** Key transforms used to lookup keys from Mentions, upon addition & retrieval of an entry. */
  val mentionQueryKTs: MentionKeyTransforms = DefaultMentionKeyTransforms

)  {

  override def toString: String =
    s"<IMKBKeyTransforms: $baseKTs, $queryKTs, $mentionQueryKTs>"

}
