package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.grounding.ReachKBUtils._
import edu.arizona.sista.reach.grounding.ReachRLKBLookups._
import edu.arizona.sista.reach.extern.export.MentionManager

/**
  * Class which implements methods to select the best groundings for a sequence of mentions.
  *   Written by Tom Hicks. 2/9/2016.
  *   Last Modified: Walk biomentions, match species from context.
  */
class ReachGrounder extends Speciated {

  val mentionMgr = new MentionManager

  /** Select and apply best grounding choice to a sequence of bio mentions. */
  def apply (mentions: Seq[BioMention]): Seq[BioMention] = {
    mentions.foreach { mention =>
      if (mention.isInstanceOf[BioMention])
        groundMention(mention, Seq.empty[String])
    }
    mentions                                // return the newly grounded sequence
  }

  /** Dispatch the given bio mention for grounding, based on candidates and given species context. */
  def groundMention (mention: BioMention, species: Seq[String]): Unit = {
    mention match {
      case bem: BioEventMention => groundArguments(bem)
      case brm: BioRelationMention => groundArguments(brm)
      case  bm: BioTextBoundMention =>
        if (bm.hasMoreCandidates) {         // only redo grounding if more than one choice
          if (species.isEmpty || containsHumanNsId(species)) // use nsId for now
            groundAsHuman(bm)               // then prioritize human grounding
          else                              // else context can influence this grounding
            groundBySpecies(bm, species)
        }
      case _ =>                             // no action needed
    }
  }

  /** Recursively process arguments of given event, possibly setting new context environment. */
  def groundArguments (event: BioMention): Unit = {
    val evargs = event.arguments.values.flatten.toSeq.map(_.toBioMention)
    if (hasSpeciesContext(event)) {         // for now, only using species to help grounding
      val species = event.context.get.get("Species").get
      evargs.foreach(groundMention(_, species))
    }
    else
      evargs.foreach(groundMention(_, Seq.empty[String]))
  }

  /** Prioritize the Grounding of the given mention as human.
    * NB: Mention must be grounded and have more than one candidate. */
  def groundAsHuman (mention: BioTextBoundMention): Unit = {
    if (mention.hasMoreCandidates) {        // sanity check
      val cands = mention.candidates.get
      val ordered = selectHuman(cands) ++ selectNoSpecies(cands) ++ selectNotHuman(cands)
      mention.nominate(Some(ordered))
    }
  }

  /** Prioritize the grounding for one of the given species.
    * NB: Mention must be grounded and have more than one candidate. */
  def groundBySpecies (mention: BioTextBoundMention, mentionNsIds: Seq[String]): Unit = {
    if (mention.hasMoreCandidates) {        // sanity check
      val cands = mention.candidates.get    // get all candidates
      val candSpecies: Set[String] = cands.map(_.species).toSet // all candidate species
      val species = nsIdToSpeciesSet(mentionNsIds.head).map(_.intersect(candSpecies))
      // if any species match then prefer candidates with those species
      if (species.isDefined && !species.get.isEmpty) {
        val ordered = selectBySpecies(cands, species.get) ++ selectByNotSpecies(cands, species.get)
        mention.nominate(Some(ordered))     // reattach reordered grounding candidates
      }
    }
  }


  /** Reverse lookup the give NsId string and return the species associated with it. */
  private def nsIdToSpeciesSet (nsId: String): Option[Set[String]] =
    ReverseSpeciesLookup.lookup(nsId)

  private def printMention (mention:BioMention): Unit =
    mentionMgr.mentionToStrings(mention).foreach { System.err.println(_) }

}
