package org.clulab.reach.grounding

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.reach._
import org.clulab.reach.context._
import org.clulab.reach.mentions._
import org.clulab.reach.grounding.ReachIMKBMentionLookups._
import org.clulab.reach.grounding.ReachKBUtils._
import org.clulab.reach.grounding.Speciated._
import org.clulab.reach.utils.MentionManager

/**
  * Class which implements methods to select the best groundings for a sequence of mentions.
  *   Written by Tom Hicks. 2/9/2016.
  *   Last Modified: Update for replacement of reverse lookups.
  */
class ReachGrounder extends LazyLogging {

  val config = ConfigFactory.load()
  val overrideSpecies = config.getBoolean("grounding.overrideSpecies")
  val contextSpecies = configuredKBML("Species").head // TODO handle this case elegantly

  val mentionMgr = new MentionManager

  /** Select and apply best grounding choice to a sequence of bio mentions. */
  def apply (mentions: Seq[BioMention]): Seq[BioMention] = {
    mentions.foreach { mention =>
      if (mention.isInstanceOf[BioMention])
        groundMention(mention, getSpeciesContext(mention))
    }
    mentions                                // return the newly grounded sequence
  }

  /** Return a possibly empty sequence of NS/ID strings for the given mentions. */
  def getSpeciesContext (mention: BioMention): Seq[String] = {
    // for now, we are using only species to help grounding:
    if (!overrideSpecies && hasSpeciesContext(mention))
      mention.context.get.get("Species").get
    else
      Seq.empty[String]
  }

  /** Dispatch the given bio mention for grounding, based on candidates and given species context. */
  def groundMention (mention: BioMention, context: Seq[String]): Unit = {
    mention match {
      case bem: BioEventMention => groundArguments(bem)
      case brm: BioRelationMention => groundArguments(brm)
      case  bm: BioTextBoundMention =>
        if (bm.hasMoreCandidates) {         // only redo grounding if more than one choice
          if (context.isEmpty || containsHumanNsId(context)) // use nsId for now
            groundAsHuman(bm)               // then prioritize human grounding
          else                              // else context can influence this grounding
            groundBySpecies(bm, context)
        }
      case _ =>                             // no action needed
    }
  }

  /** Recursively process arguments of given parent mention, possibly setting new context environment. */
  def groundArguments (parent: BioMention): Unit = {
    val children = parent.arguments.values.flatten.toSeq.map(_.toBioMention)
    children.foreach(bm => groundMention(bm, getSpeciesContext(bm)))
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

  /** Prioritize the grounding for one of the given species NS/IDs strings.
    * NB: Mention must be grounded and have more than one candidate. */
  def groundBySpecies (mention: BioTextBoundMention, context: Seq[String]): Unit = {
    if (mention.hasMoreCandidates) {        // sanity check
      val cands = mention.candidates.get    // get all candidates
      val candNames = cands.map(_.species).toSet // all candidate species names

      // reverse map set of NS/IDs to set of species name strings:
      val contextNames = contextToSpeciesNameSet(context)

      // intersect candidate species names with context species names:
      val species = contextNames.intersect(candNames)

      // if any species match then prefer candidates with those species
      if (species.nonEmpty) {
        val ordered = selectBySpecies(cands, species) ++ selectByNotSpecies(cands, species)
        mention.nominate(Some(ordered))     // reattach reordered grounding candidates
      }
    }
  }

  /** Return a possibly empty set of all species name strings for the given context of NS/IDs. */
  private def contextToSpeciesNameSet (context: Seq[String]): SpeciesNameSet = {
    val ctx:Set[String] = context.toSet
    ctx.flatMap(contextSpecies.speciesForNsId(_)).map(_.toLowerCase)
  }

  private def printMention (mention:BioMention): Unit =
    mentionMgr.mentionToStrings(mention).foreach { logger.error(_) }

}
