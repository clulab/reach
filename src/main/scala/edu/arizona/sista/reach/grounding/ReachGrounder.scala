package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.grounding.ReachKBUtils._
import edu.arizona.sista.reach.extern.export.MentionManager

/**
  * Class which implements methods to select the best groundings for a sequence of mentions.
  *   Written by Tom Hicks. 2/9/2016.
  *   Last Modified: WIP: begin to match context by nsId.
  */
class ReachGrounder extends Speciated {

  val mentionMgr = new MentionManager

  /** Select and apply best grounding choice to a sequence of bio mentions. */
  def apply (mentions: Seq[BioMention]): Seq[BioMention] = mentions.flatMap {
    case bm: BioMention => groundMention(bm, Seq.empty[String])
    case m => Seq(m)                  // else just return the mention unchanged
  }

  /** Dispatch the given bio mention for grounding, based on candidates and given species context. */
  def groundMention (mention: BioMention, species: Seq[String]): Seq[BioMention] = {
    mention match {
      case bem: BioEventMention => groundArguments(bem)
      case  bm: BioTextBoundMention =>
        if (bm.hasMoreCandidates) {         // only redo grounding if more than one choice
          System.err.println(s"SPECIES=${species}") // REMOVE LATER
          // if (species.isEmpty || containsHumanSpecies(species)) // USE LATER?
          if (species.isEmpty || containsHumanNsId(species)) // use nsId for now
            groundAsHuman(bm)               // then prioritize human grounding
          else                              // else context can influence this grounding
            groundBySpecies(bm, species)
        }
        Seq(bm)                             // return the possibly modified mention
      case m => Seq(m)                      // else just return the mention unchanged
    }
  }

  /** Recursively process arguments of given event, possibly setting new context environment. */
  def groundArguments (event: BioEventMention): Seq[BioMention] = {
    val args = event.arguments.values.flatten.toSeq.map(_.toBioMention)
    if (hasSpeciesContext(event)) {       // for now, only using species to help grounding
      val species = event.context.get.get("Species").get
      args.flatMap(groundMention(_, species))
    }
    else
      args.flatMap(groundMention(_, Seq.empty[String]))
  }

  /** Prioritize the Grounding of the given mention as human.
    * NB: Mention must be grounded and have more than one candidate. */
  def groundAsHuman (mention: BioTextBoundMention): BioMention = {
    if (mention.hasMoreCandidates) {        // sanity check
      val cands = mention.candidates.get
      val ordered = selectHuman(cands) ++ selectNoSpecies(cands) ++ selectNotHuman(cands)
      mention.nominate(Some(ordered))
    }
    System.err.println("AS_HUMAN")          // REMOVE LATER
    printMention(mention)                   // REMOVE LATER
    return mention
  }

  /** Prioritize the grounding for one of the given species.
    * NB: Mention must be grounded and have more than one candidate. */
  def groundBySpecies (mention: BioTextBoundMention, species: Seq[String]): BioMention = {
    if (mention.hasMoreCandidates) {        // sanity check
      val cands = mention.candidates.get
      // val candSpecies: Set[String] = cands.map(_.species.toLowerCase).toSet  // USE LATER?
      // val firstMatch = species.find{ case sp:String => candSpecies.contains(sp.toLowerCase) }
      val candSpecies: Set[String] = cands.map(_.nsId).toSet // use nsId for now
      val firstMatch = species.find{ case nsId:String => candSpecies.contains(nsId) } // use nsId for now
      val ordered = if (firstMatch.isDefined)
        // selectASpecies(cands, firstMatch.get) ++ selectNotASpecies(cands, firstMatch.get) // USE LATER?
        selectByNsId(cands, firstMatch.get) ++ selectByNotNsId(cands, firstMatch.get)
      else cands
      mention.nominate(Some(ordered))       // reattach reordered grounding candidates
    }
    System.err.println("BY_SPECIES")        // REMOVE LATER
    printMention(mention)                   // REMOVE LATER
    return mention
  }


  def printMention (mention:BioMention): Unit =
    mentionMgr.mentionToStrings(mention).foreach { System.err.println(_) }

}
