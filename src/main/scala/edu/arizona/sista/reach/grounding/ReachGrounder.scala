package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.extern.export.MentionManager

/**
  * Class which implements methods to select the final groundings for a mention sequence.
  *   Written by Tom Hicks. 2/9/2016.
  *   Last Modified: Cleanups: use bio mention, simplify print mention.
  */
class ReachGrounder {

  val mentionMgr = new MentionManager

  /** Use candidate resolutions to select and apply final grounding choice. */
  def apply (mentions: Seq[BioMention]): Seq[BioMention] = mentions map {
    case tm: BioMention => selectGrounding(tm)
    case m => m
  }

  /** Select the final grounding for the given mention. */
  def selectGrounding (mention: BioMention): BioMention = {
    if (mention.isGrounded) {
      if (mention.hasCandidates && hasSpeciesContext(mention)) {
        System.err.println(mention.context.get.get("Species")) // TODO: IMPLEMENT LATER
      }
      else mention.selectCurrentGrounding
    }

    return mention
  }

  def printMention (mention:BioMention): Unit =
    mentionMgr.mentionToStrings(mention).foreach { System.err.println(_) }

}
