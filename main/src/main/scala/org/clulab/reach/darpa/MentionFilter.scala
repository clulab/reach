package org.clulab.reach.darpa

import org.clulab.coref.CorefUtils._
import org.clulab.odin._
import org.clulab.reach.mentions._

import scala.collection.mutable
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.reach.utils
import org.clulab.struct.DirectedGraph

object MentionFilter {

  // the filter removes (some of) the mentions that incorrectly grab arguments from other mentions
  //E.g., in "Insulin stimulated tyrosine phosphorylation of IRS-1, PI 3-kinase activity, and Akt phosphorylation."
  //the filter eliminates the following incorrect relations: phosphorylation1-Akt and phosphorylation2-IRS1
  def filterOverlappingMentions(ms: Seq[CorefMention]): Seq[CorefMention] = {
    // For each mention, check to see if any other mention has argument overlap and if there is arg overlap,
    // check if the path from the trigger to the overlapping argument contains a trigger of the other mention +
    //checks if the overlapping argument is connected to the trigger of another mention with an incoming edge
    for {
      i <- 0 until ms.length
      m1 = ms(i)
      overlapping = getOverlappingMentions(m1, ms)
      if !triggerOverlap(m1, overlapping)
    } yield m1
  }

  def argumentsOverlap(m1: Mention, m2: Mention): Boolean = {
    val m1Themes = m1.arguments.getOrElse("theme", Seq.empty[Mention]).map(_.tokenInterval)
    val m2Themes = m2.arguments.getOrElse("theme", Seq.empty[Mention]).map(_.tokenInterval)
    for (interval <- m1Themes) {
      if (m2Themes.exists(interval2 => interval2.overlaps(interval))) {
        return true
      }
    }
    false
  }

  def getOverlappingMentions(m: Mention, ms: Seq[Mention]): Seq[Mention] = {
    ms.filter(argumentsOverlap(m, _))
  }

  // fixme: make a better name
  // If argument overlap is found, check to see if it's a problem: i.e., if the synPath between the trigger
  // and an argument goes through another trigger
  def triggerOverlap(m1: Mention, ms: Seq[Mention]): Boolean = {
    for (m2 <- ms) {
      if (triggerOverlap(m1, m2)) {
        return true
      }
    }
    false
  }

  def triggerOverlap(m1: Mention, m2: Mention): Boolean = {
    m2 match {
      case em: EventMention if (em.trigger.tokenInterval != m1.asInstanceOf[CorefEventMention].trigger.tokenInterval && m1.arguments.get("theme").nonEmpty && m2.arguments.get("theme").nonEmpty && em.labels.contains("SimpleEvent") && !em.labels.contains("Amount")) =>
        // Does the synPath to the argument of m1 contain the trigger of m2?
        val m1Themes = m1.arguments.get("theme")
        val m2Themes = em.arguments.get("theme")
        val overlappingThemes = m1Themes.filter(theme => m2Themes.contains(theme))
        if (overlappingThemes.nonEmpty) {
          overlappingThemes.get.exists(theme => synPathContainsTrigger(m1, theme, em.trigger))
        } else false
      case _ => false
    }
  }

  def synPathContainsTrigger(m: Mention, theme: Mention, trigger: Mention): Boolean = {
    if (m.paths.contains("theme")) {
      val synPath = m.paths("theme").get(theme)
      val pathfinder = new utils.PathFinder(m.sentenceObj)

      // Does the synPath contain the trigger
      val graph = m.sentenceObj.dependencies.get
      val tokensOnPath = synPath.get.flatMap(path => Seq(path._1, path._2)).toSet
      // get edges for each token of the argument (more than one for compound arguments)
      val edges_prelim = for (node <- theme.tokenInterval) yield {mkPrev(node, m.sentenceObj, graph)}
      val edges = edges_prelim.flatten
      val outgoingRelation = mkOutgoing(theme.tokenInterval.start, m.sentenceObj, graph)
      trigger.tokenInterval.exists(tok => (tokensOnPath.contains(tok) || edges.contains(tok)) && !outgoingRelation.contains("appos"))
    } else false
  }

  //checks incoming rel for the given node (token int)
  def mkPrev(node: Int, sent: Sentence, graph: DirectedGraph[String]): Seq[Int] = {
    val edges = graph.incomingEdges(node)
    edges.map(_._1).distinct
  }

  //check outgoing rel (rel label, e.g., 'appos')
  def mkOutgoing(node: Int, sent: Sentence, graph: DirectedGraph[String]): Array[String] = {
    val outgoing = graph.outgoingEdges(node)
    outgoing.map(_._2).distinct
  }


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
        case tex: CorefTextBoundMention => Nil
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
                    relReg.paths,
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
                    eventReg.paths,
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
      // the purpose of this method *seems* to be to filter out duplicates. Is that the case?
      // we can do that pretty easily in assembly...maybe this could be retired/rewritten?
      val highestOrderControlled = for {
        r <- regulations.map(_.toCorefMention)
      } yield {
        val isRedundant = regulations.exists{ m =>
          val mctrld = m.arguments("controlled").head
          ((m.isInstanceOf[CorefRelationMention] && r.isInstanceOf[CorefRelationMention]) ||
            (m.isInstanceOf[CorefEventMention] && r.isInstanceOf[CorefEventMention] &&
              m.asInstanceOf[CorefEventMention].trigger == r.asInstanceOf[CorefEventMention].trigger)) &&
            // ensure both mentions have a controller
            m.arguments.get("controller").isDefined && r.arguments.get("controller").isDefined &&
            m.arguments("controller") == r.arguments("controller") &&
            mctrld.matches("Regulation") &&
            mctrld.arguments("controlled") == r.arguments("controlled")
        }
        if (isRedundant) None else Some(r)
      }
      val highestOrder = for {
        r <- highestOrderControlled.flatten
        // ensure there is a controller
        if r.arguments.keySet contains "controller"
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
        case (ent: CorefTextBoundMention, ev: CorefEventMention) => DarpaActions.convertEventToEntity(ev) == ent
        case (ev: CorefEventMention, ent: CorefTextBoundMention) => DarpaActions.convertEventToEntity(ev) == ent
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
