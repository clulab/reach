package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.grounding._

/**
  * Class which implements methods to select the final groundings for a mention sequence.
  *   Written by Tom Hicks. 2/9/2016.
  *   Last Modified: Add selection frame for grounding selection.
  */
class ReachGrounder extends DarpaFlow {

  /** Local implementation of darpa flow trait: use KB resolutions and context
      to select and apply final grounding choice. */
  def apply (mentions: Seq[Mention], state: State): Seq[Mention] = mentions map {
    case tm: BioTextBoundMention => selectGrounding(tm, state)
    case m => m
  }

  /** Select the final grounding for the given mention. */
  private def selectGrounding (mention: BioMention, state: State): Mention = {
    assert(mention.isGrounded,              // SHOULD be grounded before this point
           s"Mention '${mention}' should be grounded before final grounding selection.")

    if (mention.hasCandidates && hasSpeciesContext(mention)) {
      println(mention.context.get.get("Species")) // TODO: IMPLEMENT LATER
    }
    else mention.selectCurrentGrounding

    return mention
  }

}
