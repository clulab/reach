package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * A record class containing key transforms used to configure an in-memory KB upon instantiation.
  *   Written by: Tom Hicks. 1/1/2017.
  *   Last Modified: Initial creation.
  */
case class IMKBKeyTransforms (

  /** Key transforms used to create storage keys upon addition of an entry. */
  val additionKTs: KeyTransforms = DefaultAddKeyTransforms,

  /** Key transforms used to lookup keys upon addition of an entry. */
  val queryKTs: KeyTransforms = DefaultQueryKeyTransforms,

  /** Key transforms used to lookup keys from Mentions, upon addition of an entry. */
  val mentionQueryKTs: MentionKeyTransforms = DefaultMentionKeyTransforms

)  {

  override def toString: String =
    s"<IMKBKeyTransforms: $additionKTs, $queryKTs, $mentionQueryKTs>"

}
