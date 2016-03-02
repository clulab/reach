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
  def apply (mentions: Seq[BioMention]): Seq[BioMention] = {
    mentions.flatMap {
      case bem: BioEventMention => groundArguments(bem) // context can help event argument grounding
      case bm:  BioMention =>               // other bio mentions can be finalized
        if (bm.hasCandidates)               // if mention has any grounding candidates
          bm.selectCurrentGrounding         // finalize the grounding
        Seq(bm)
      case m =>
        Seq(m)                      // else just return the mention unchanged
    }
  }

  def groundArguments (mention: BioEventMention): Seq[BioMention] = {
    if (hasSpeciesContext(mention)) {       // for now, only using species to help grounding
      val context = mention.context.get.get("Species").get
      mention.arguments.values.flatten.toSeq.map(_.toBioMention).map {
        case arg:BioMention => selectGrounding(context, arg)
        case m => m
      }
    }
    else Seq(mention)                       // else return mention unchanged
  }

  /** Select the final grounding for the given mention, if it has grounding candidates. */
  def selectGrounding (ctx: Seq[String], mention: BioMention): BioMention = {
    if (mention.hasCandidates) {            // has candidates means it is grounded
      if (mention.hasMoreCandidates) {
        System.err.println(s"SEL-GROUNDING: ${ctx}") // REMOVE LATER
        // TODO: IMPLEMENT selection based on species
      }
      else mention.selectCurrentGrounding
    }
    return mention
  }

  def printMention (mention:BioMention): Unit =
    mentionMgr.mentionToStrings(mention).foreach { System.err.println(_) }

}
