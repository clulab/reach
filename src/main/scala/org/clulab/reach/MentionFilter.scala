package org.clulab.reach

import org.clulab.coref.CorefUtils._
import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.reach.DarpaActions._

import scala.collection.mutable

object MentionFilter {

  // a simple, imperfect way of removing incomplete Mentions
  def pruneMentions(ms: Seq[CorefMention]): Seq[CorefMention] = {

    val (events, nonEvents) = ms.partition(_.isInstanceOf[CorefEventMention])
    // We need to remove underspecified EventMentions of near-duplicate groupings
    // (ex. same phospho, but one is missing a site)
    val mentionGroupings =
      events.map(_.asInstanceOf[CorefEventMention]).groupBy(m => (m.trigger, m.label))

    // remove incomplete mentions
    val completeEventMentions =
      for ((k, ems) <- mentionGroupings) yield {
        val maxSize: Int = ems.map(_.arguments.values.flatten.size).max
        val filteredEMs = ems.filter(m => m.arguments.values.flatten.size == maxSize)
        filteredEMs
      }
    nonEvents ++ completeEventMentions.flatten.toSeq
  }

  // Removal of incomplete Mentions with special care to Regulations
  // calls pruneMentions after attending to Regulations
  def filterRegulations(regulations: Seq[BioMention], other: Seq[BioMention], state: State): Seq[CorefMention] = {

    // Move any Negation modification on a Regulation's controlled arg to the Regulation
    def promoteNegationModifications(parent: BioMention, child: BioMention): Unit = {
      val (negs, other) = child.modifications.partition(_.isInstanceOf[Negation])
      parent.modifications ++= negs
      // remove Negation modifications from child
      child.modifications = other
    }

    // We need to keep track of what SimpleEvents to remove from the state
    // whenever another is used to replace it as a "controlled" arg in a Regulation
    var toRemove = mutable.Set[Mention]()
    // Check each Regulation to see if there are any "more complete" Mentions
    // for the controlled available in the state
    val correctedRegulations = for {
      reg <- regulations
      // it should only ever have ONE controlled
      controlled = reg
        .arguments("controlled")
        .head
        // treat it as a CorefEventMention to simplify filtering
        .toCorefMention
      // how many args does the controlled Mention have?
      argCount = controlled.antecedentOrElse(controlled).arguments.size
    } yield {
      // Are there any "more complete" SimpleEvents in the State
      // that are candidates to replace the current "controlled" arg?
      val replacementCandidates: Seq[CorefMention] = controlled match {
        case rel: CorefRelationMention => Nil
        case ev: CorefEventMention => state.mentionsFor(reg.sentence, controlled.tokenInterval, controlled.label)
            // If the label is the same, these MUST be CorefEventMentions (i.e SimpleEvents)
            .map(_.toCorefMention)
            .filter(m =>
              m.isInstanceOf[CorefEventMention] &&
              m.antecedentOrElse(m).arguments.size > argCount &&
                (m.asInstanceOf[CorefEventMention].trigger == ev.asInstanceOf[CorefEventMention].trigger))
      }
      // Do we have any "more complete" Mentions to substitute for the controlled?
      replacementCandidates match {
        // Use the current reg, since there aren't any "more complete"
        // candidate Mentions for the controlled
        case Nil => Seq(reg)
        // There are some more complete candidates for the controlled arg...
        case candidates =>
          // For each "more complete" SimpleEvent, create a new Regulation...
          for (r <- candidates) yield {
            reg.toCorefMention match {
              // Is the reg we're replacing a BioRelationMention?
              case relReg: CorefRelationMention =>
                val updatedArgs = relReg.arguments updated("controlled", Seq(r))
                val junk = relReg.arguments("controlled").head.toCorefMention
                // Keep track of what we need to get rid of...
                toRemove += junk
                // Create the "more complete" BioRelationMentions
                val moreCompleteReg =
                  new CorefRelationMention(
                    relReg.labels,
                    updatedArgs,
                    relReg.sentence,
                    relReg.document,
                    relReg.keep,
                    relReg.foundBy)
                // Move Negation modifications from controlled to Reg.
                //promoteNegationModifications(moreCompleteReg, r)
                // return the new Regulation
                moreCompleteReg
              // Is the Regulation we're replacing a BioEventMention?
              case eventReg: CorefEventMention =>
                val updatedArgs = eventReg.arguments updated("controlled", Seq(r))
                val junk = eventReg.arguments("controlled").head.toCorefMention
                // Keep track of what we need to get rid of...
                toRemove += junk
                // Create the "more complete" BioEventMentions
                val moreCompleteReg =
                  new CorefEventMention(
                    eventReg.labels,
                    eventReg.trigger,
                    updatedArgs,
                    eventReg.sentence,
                    eventReg.document,
                    eventReg.keep,
                    eventReg.foundBy,
                    eventReg.isDirect)
                // Move Negation modifications from controlled to Reg.
                //promoteNegationModifications(eventReg, r)
                // return the new Regulation
                moreCompleteReg
            }
          }
      }
    }

    def filterByController(regulations: Seq[CorefMention]): Seq[CorefMention] = {
      // collect all regulation events with a Complex as the controller
      val regulationsWithComplexAsController = regulations.filter { m =>
        m.arguments.contains("controller") && m.arguments("controller").head.matches("Complex")
      }
      // collect the rest of the regulations
      val remainingRegulations = regulationsWithComplexAsController match {
        // if there where no regulations with complex controllers
        // then all the regulations are remaining
        case Nil => regulations
        // get all mentions that have no complex controller
        // and also have no controller included in a complex
        case events => regulations diff regulationsWithComplexAsController filter { m =>
          m.arguments.contains("controller") && // maybe m doesn't even have a controller
          !regulationsWithComplexAsController.exists { reg =>
            // m's controller shouldn't be included in a complex
            val participants = reg.arguments("controller").head.arguments.get("theme")
            participants.isDefined && participants.get.contains(m.arguments("controller").head)
          }
        }
      }
      regulationsWithComplexAsController ++ remainingRegulations
    }

    def preferRegulations(regulations: Seq[BioMention]): Seq[CorefMention] = {
      val highestOrderControlled = for {
        r <- regulations.map(_.toCorefMention)
      } yield {
        val isRedundant = regulations.exists{ m =>
          val mctrld = m.arguments("controlled").head
          ((m.isInstanceOf[CorefRelationMention] && r.isInstanceOf[CorefRelationMention]) ||
            (m.isInstanceOf[CorefEventMention] && r.isInstanceOf[CorefEventMention] &&
            m.asInstanceOf[CorefEventMention].trigger == r.asInstanceOf[CorefEventMention].trigger)) &&
            m.arguments("controller") == r.arguments("controller") &&
            mctrld.matches("Regulation") &&
            mctrld.arguments("controlled") == r.arguments("controlled")
        }
        if (isRedundant) None else Some(r)
      }
      val highestOrder = for {
        r <- highestOrderControlled.flatten
      } yield {
        val isRedundant = regulations.filter(_.arguments.get("controller").isDefined).exists{ m =>
          val mctrlr = m.arguments("controller").head
          ( // Both are events w/ same trigger
            ((m.isInstanceOf[CorefRelationMention] && r.isInstanceOf[CorefRelationMention]) ||
                (m.isInstanceOf[CorefEventMention] && r.isInstanceOf[CorefEventMention] &&
                  m.asInstanceOf[CorefEventMention].trigger == r.asInstanceOf[CorefEventMention].trigger)
              ) &&
              m.arguments("controlled") == r.arguments("controlled") &&
              mctrlr.matches("Regulation") &&
              mctrlr.arguments("controller") == r.arguments("controlled")
            ) ||
            // One is a TextBoundMention
            (m.arguments("controlled") == r.arguments("controlled") &&
              mctrlr.matches("Regulation") &&
              // This is specifically for event controllers converted to PTMs
              ptmEquivalent(mctrlr.arguments("controlled").head.toCorefMention, r.arguments("controller").head.toCorefMention))
        }
        if (isRedundant) None else Some(r)
      }
      highestOrder.flatten
    }

    def ptmEquivalent(a: CorefMention, b: CorefMention): Boolean = {
      (a,b) match {
        case (exactA, exactB) if exactA == exactB => true
        case (ent: CorefTextBoundMention, ev: CorefEventMention) => convertEventToEntity(ev) == ent
        case (ev: CorefEventMention, ent: CorefTextBoundMention) => convertEventToEntity(ev) == ent
        case different => false
      }
    }

    val correctedRegs = preferRegulations(correctedRegulations
      .flatten)
      .groupBy(_.arguments("controlled"))
      .values
      .map(filterByController)
      .flatten
      .toSeq

    // Remove any "controlled" Mentions we discarded
    val nonRegs =
      corefDistinct(other
        .filterNot(m => toRemove.contains(m))
        .map(_.toCorefMention))
    // Convert Regulations to BioMentions
    val cleanRegulations =
      corefDistinct(correctedRegs
        .map(_.toCorefMention))
    // We don't want to accidentally filter out any SimpleEvents
    // that are valid arguments to the filtered Regs
    val keepThese:Seq[CorefMention] =
      corefDistinct(cleanRegulations.flatMap(_.arguments.values)
        .flatten
        .filter(m => m matches "SimpleEvent")
        .map(_.toCorefMention))
    // Return the filtered with the arguments to the Regulations
    val remainingNonRegs = corefDistinct(pruneMentions(nonRegs) ++ keepThese)

    /*
    println(s"\n${cleanRegulations.size} cleanRegulations after pruning:")
    cleanRegulations foreach display.displayMention
    println(s"\t${remainingNonRegs.size} remainingNonRegs after pruning nonRegs:")
    remainingNonRegs foreach display.displayMention
    println("#" * 40 + "\n")
    */
    corefDistinct(cleanRegulations ++ remainingNonRegs)
  }

  def keepMostCompleteMentions(ms: Seq[BioMention]): Seq[CorefMention] = {
     keepMostCompleteMentions(ms, State(ms))
  }

  // Filter out "incomplete" events
  def keepMostCompleteMentions(ms: Seq[BioMention], state: State): Seq[CorefMention] = {
    // Regulations require special attention
    val (regulations, other) = ms.partition(_ matches "Regulation")
    regulations match {
      case someRegs if someRegs.nonEmpty =>
        val moreComplete = filterRegulations(someRegs, other, state)
        // filter by controller
        moreComplete.filter(m => keepMention(m, State(moreComplete)))
          .map(_.toCorefMention)
      case Nil =>
        pruneMentions(other.map(_.toCorefMention))
    }
  }

  /** Does the mention have a controller? */
  def hasController(m: Mention): Boolean = m.arguments.keySet contains "controller"

  /** Does the mention have a Entity as its controller? */
  def hasEntityAsController(m: Mention): Boolean = m.arguments.get("controller") match {
    case Some(Seq(entity)) if entity matches "Entity" => true
    case _ => false
  }

  /** Does the mention have an Event as its controller? */
  def hasEventAsController(m: Mention): Boolean = m.arguments.get("controller") match {
    case Some(Seq(event)) if event matches "Event" => true
    case _ => false
  }

  /** Determine whether or not a mention should be dropped **/
  def keepMention(m: Mention, state: State): Boolean = m match {
    // inspect any reg with an entity as its controller
    // to see if an overlapping reg with an Event controller exists
    case rec if (rec matches "Regulation") && hasEntityAsController(m) =>
      val controlled = rec.arguments("controlled").head
      val hasValidAlternate: Boolean = state.mentionsFor(rec.sentence, rec.tokenInterval, rec.label)
        .exists(c =>
          // don't consider the rec as an alternate
          (c != rec) &&
            // only consider matches if the controlleds are equivalent
            (c.arguments("controlled").head == controlled) &&
            // candidates for replacement should have a
            hasEventAsController(c)
        )
      hasValidAlternate match {
        // there are better options, so drop this guy
        case true => false
        // no replacement candidates exist, so mention is valid
        case false => true
      }
    // assume valid otherwise
    case other => true
  }
}
