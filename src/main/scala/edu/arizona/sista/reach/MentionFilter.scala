package edu.arizona.sista.reach

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import scala.collection.mutable

object MentionFilter {

  // a simple, imperfect way of removing incomplete Mentions
  def pruneMentions(ms: Seq[BioMention]): Seq[BioMention] = {

    val (events, nonEvents) = ms.partition(_.isInstanceOf[BioEventMention])
    // We need to remove underspecified EventMentions of near-duplicate groupings
    // (ex. same phospho, but one is missing a site)
    val mentionGroupings =
      events.map(_.asInstanceOf[BioEventMention]).groupBy(m => (m.trigger, m.label))

    // remove incomplete mentions
    val completeEventMentions =
      for ((k, ems) <- mentionGroupings) yield {
        val maxSize: Int = ems.map(_.arguments.size).max
        val filteredEMs = ems.filter(m => m.arguments.size == maxSize)
        filteredEMs
      }
    nonEvents ++ completeEventMentions.flatten.toSeq
  }

  // Removal of incomplete Mentions with special care to Regulations
  // calls pruneMentions after attending to Regulations
  def filterRegulations(regulations: Seq[Mention], other: Seq[Mention], state: State): Seq[Mention] = {

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
    val correctedRegulations =
      for {reg <- regulations
           // it should only ever have ONE controlled
           controlled =
           reg.arguments("controlled")
             .head
             // treat it as a BioEventMention to simplify filtering
             .asInstanceOf[BioEventMention]
           // how many args does the controlled Mention have?
           argCount = controlled.arguments.size
      } yield {
        // Are there any "more complete" SimpleEvents in the State
        // that are candidates to replace the current "controlled" arg?
        val replacementCandidates: Seq[BioEventMention] =
          state.mentionsFor(reg.sentence, controlled.tokenInterval, controlled.label)
            // If the label is the same, these MUST be BioEventMentions (i.e SimpleEvents)
            .map(_.asInstanceOf[BioEventMention])
            .filter(m => m.arguments.size > argCount && (m.trigger == controlled.trigger))
        // Do we have any "more complete" Mentions to substitute for the controlled?
        replacementCandidates match {
          // Use the current reg, since there aren't any "more complete"
          // candidate Mentions for the controlled
          case Nil => Seq(reg)
          // There are some more complete candidates for the controlled arg...
          case candidates =>
            // For each "more complete" SimpleEvent, create a new Regulation...
            for (r <- candidates) yield {
              reg match {
                // Is the reg we're replacing a BioRelationMention?
                case relReg: BioRelationMention =>
                  val updatedArgs = relReg.arguments updated("controlled", Seq(r))
                  val junk = relReg.arguments("controlled").head.toBioMention
                  // Keep track of what we need to get rid of...
                  toRemove += junk
                  // Create the "more complete" BioRelationMentions
                  val moreCompleteReg =
                    new BioRelationMention(
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
                case eventReg: BioEventMention =>
                  val updatedArgs = eventReg.arguments updated("controlled", Seq(r))
                  val junk = eventReg.arguments("controlled").head.toBioMention
                  // Keep track of what we need to get rid of...
                  toRemove += junk
                  // Create the "more complete" BioEventMentions
                  val moreCompleteReg =
                    new BioEventMention(
                      eventReg.labels,
                      eventReg.trigger,
                      updatedArgs,
                      eventReg.sentence,
                      eventReg.document,
                      eventReg.keep,
                      eventReg.foundBy)
                  // Move Negation modifications from controlled to Reg.
                  //promoteNegationModifications(eventReg, r)
                  // return the new Regulation
                  moreCompleteReg
              }
            }
        }
      }

    def filterByController(regulations: Seq[Mention]): Seq[Mention] = {
      // collect all regulation events with a Complex controller
      val regulationsWithComplexController = regulations.filter { m =>
        m.arguments.contains("controller") && m.arguments("controller").head.matches("Complex")
      }
      // collect the rest of the regulations
      val remainingRegulations = regulationsWithComplexController match {
        // if there where no regulations with complex controllers
        // then all the regulations are remaining
        case Nil => regulations
        // get all mentions that have no complex controller
        // and also have no controller included in a complex
        case events => regulations diff regulationsWithComplexController filter { m =>
          m.arguments.contains("controller") && // maybe m doesn't even have a controller
          !regulationsWithComplexController.exists { reg =>
            // m's controller shouldn't be included in a complex
            val participants = reg.arguments("controller").head.arguments.get("theme")
            participants.isDefined && participants.get.contains(m.arguments("controller").head)
          }
        }
      }
      regulationsWithComplexController ++ remainingRegulations
    }
    val correctedRegs = correctedRegulations
      .flatten
      .groupBy(_.arguments("controlled"))
      .values
      .map(filterByController)
      .flatten
      .toSeq

    // Remove any "controlled" Mentions we discarded
    val nonRegs =
      other
        .filterNot(m => toRemove.contains(m))
        .map(_.toBioMention)
        .distinct
    // Convert Regulations to BioMentions
    val cleanRegulations =
      correctedRegs
        .map(_.toBioMention)
        .distinct
    // We don't want to accidentally filter out any SimpleEvents
    // that are valid arguments to the filtered Regs
    val keepThese:Seq[Mention] =
      cleanRegulations.flatMap(_.arguments.values)
        .flatten
        .filter(m => m matches "SimpleEvent")
        .map(_.toBioMention)
        .distinct
    // Return the filtered with the arguments to the Regulations
    val remainingNonRegs = (pruneMentions(nonRegs) ++ keepThese).distinct

    /*
    println(s"\n${cleanRegulations.size} cleanRegulations after pruning:")
    cleanRegulations foreach display.displayMention
    println(s"\t${remainingNonRegs.size} remainingNonRegs after pruning nonRegs:")
    remainingNonRegs foreach display.displayMention
    println("#" * 40 + "\n")
    */
    (cleanRegulations ++ remainingNonRegs).distinct
  }

  // Filter out "incomplete" events
  def keepMostCompleteMentions(ms: Seq[Mention], state: State): Seq[Mention] = {
    // Regulations require special attention
    val (regulations, other) = ms.partition(_ matches "Regulation")
    regulations match {
      case someRegs if someRegs.nonEmpty =>
        filterRegulations(someRegs, other, state)
      case Nil =>
        pruneMentions(other.map(_.toBioMention))
    }
  }

}
