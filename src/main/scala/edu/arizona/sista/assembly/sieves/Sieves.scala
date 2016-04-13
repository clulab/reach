package edu.arizona.sista.assembly.sieves

import edu.arizona.sista.assembly.AssemblyManager
import edu.arizona.sista.assembly.representations.{Complex, Event, SimpleEntity}
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.RuleReader

/**
 * Contains all sieves of the signature (mentions: Seq[Mention], manager: AssemblyManager) => AssemblyManager.
 * @param mentions a Seq of Odin Mentions
 */
class Sieves(mentions: Seq[Mention]) {

  import Constraints._
  import SieveUtils._

  val reachMentions = mentions

  /**
   * Populates an AssemblyManager with mentions (default behavior of AssemblyManager)
   * @param mentions a sequence of Odin Mentions
   * @param manager an AssemblyManager
   * @return an AssemblyManager
   */
  def trackMentions(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {
    val am = AssemblyManager()
    am.trackMentions(mentions)
    am
  }

  /**
   * Rule-based method for detecting precedence relations
   * @param mentions a sequence of Odin Mentions
   * @param manager an AssemblyManager
   * @return an AssemblyManager
   */
  def ruleBasedPrecedence(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    val p = "/edu/arizona/sista/assembly/grammars/precedence.yml"

    val name = "ruleBasedPrecedence"
    // find rule-based PrecedenceRelations
    for {
      rel <- assemblyViaRules(p, mentions)
      before = rel.arguments.getOrElse(beforeRole, Nil)
      after = rel.arguments.getOrElse(afterRole, Nil)
      if before.nonEmpty && after.nonEmpty
      // both "before" and "after" should have single mentions
      b = before.head
      a = after.head
      // cannot be an existing regulation
      if notAnExistingComplexEvent(rel)
    } {
      // store the precedence relation
      manager.storePrecedenceRelation(b, a, Set(rel), name)
    }

    manager
  }
}


/**
 * Constraints used to validate the output of sieves
 */

object Constraints {

  // roles for precedence relations
  val beforeRole = "before"
  val afterRole = "after"

  // For a complex event "C", with a controlled "A", do not re-create "A precedes C"
  def notAnExistingComplexEvent(link: Mention):Boolean = {
    val before = link.arguments(beforeRole).head
    val after = link.arguments(afterRole).head
    val argsOfBefore: Set[Mention] = before.arguments.values.flatten.toSet
    val argsOfAfter: Set[Mention] = after.arguments.values.flatten.toSet

    !(argsOfBefore contains after) && !(argsOfAfter contains before)
  }

  //
  // Used by relation classifier
  //

  /** Checks if two EERs share at least one SimpleEntity with the same grounding ID */
  def shareArg(r1: Event, r2: Event): Boolean = r1.I exists {
    case entity: SimpleEntity =>
      r2.hasApproximateArgument(entity)
    case complex: Complex =>
      complex.flattenMembers exists { ent => r2.hasApproximateArgument(ent) }
  }

  /** Use this check to automatically label negative examples **/
  def shareControlleds(mention1: Mention, mention2: Mention): Boolean = {
    // resolve both event mentions
    val m1 = AssemblyManager.getResolvedForm(mention1)
    val m2 = AssemblyManager.getResolvedForm(mention2)
    (m1, m2) match {
      // are the controlleds identical?
      case (ce1: Mention, ce2: Mention) if (ce1 matches "ComplexEvent") && (ce2 matches "ComplexEvent") =>
        val c1 = AssemblyManager.getResolvedForm(ce1.arguments("controlled").head)
        val c2 = AssemblyManager.getResolvedForm(ce2.arguments("controlled").head)
        c1.text == c2.text
      case _ => false
    }
  }

  /** Ensure that the pair of event mentions meet corpus constraints. <br>
    * Requirements: <br>
    * 1. the text of the two mentions should not be the same <br>
    * 2. a regulation should not be paired with its controlled <br>
    *
    * These requirements validate pairs of mentions before relation identification is attempted by a statistical classifier
    * */
  def isValidPair(mention1: Mention, mention2: Mention): Boolean = {
    // resolve both event mentions
    val m1 = AssemblyManager.getResolvedForm(mention1)
    val m2 = AssemblyManager.getResolvedForm(mention2)
    // a regulation should not be paired with its controlled
    // ex: "inhibited" neg-regs "activation". remove interactions between Regulations and their Controlled
    val ceConstraint: Boolean = (m1, m2) match {

      // two complex events should not share their controlled (activation-specific check)
      case (a1: Mention, a2: Mention) if (a1 matches "ActivationEvent") && (a2 matches "ActivationEvent") =>
        // controlled arg for each Activation mention should not be identical (according to grounding id)
        val controlled1 = AssemblyManager.getResolvedForm(a1.arguments("controlled").head)
        val controlled2 = AssemblyManager.getResolvedForm(a2.arguments("controlled").head)
        // grounding ids should be distinct
        controlled1.nsId() != controlled2.nsId()

      // neither of the two complex events should be the controlled of the other
      case (ce1: Mention, ce2: Mention) if (ce1 matches "ComplexEvent") && (ce2 matches "ComplexEvent") =>
        val controlled1 = AssemblyManager.getResolvedForm(ce1.arguments("controlled").head)
        val controlled2 = AssemblyManager.getResolvedForm(ce2.arguments("controlled").head)
        controlled1.text != ce2.text && controlled2.text != ce1.text

      // general checks for complex events (fall-through)
      case (m: Mention, ce: Mention) if ce matches "ComplexEvent" =>
        m.text != ce.arguments("controlled").head.text
      case (ce: Mention, m: Mention) if ce matches "ComplexEvent" =>
        m.text != ce.arguments("controlled").head.text
      case _ => true
    }
    // text spans should be unique
    (m1.words != m2.words) && ceConstraint
  }
}

/**
 * Utilities commonly used by Sieves
 */
object SieveUtils {


  // the label used to identify Mentions modelling precedence relations
  val precedenceMentionLabel = "Precedence"

  /**
   * Applies a set of assembly rules to provide mentions (existingMentions).
   * Care is taken to apply the rules to each set of mentions from the same Document.
   * @param rulesPath a path to a rule file (under resources)
   * @param existingMentions a Seq of Odin Mentions
   * @return a Seq of RelationMentions
   */
  def assemblyViaRules(rulesPath: String, existingMentions: Seq[Mention]): Seq[Mention] = {

    // read rules and initialize state with existing mentions
    val rules:String = RuleReader.readResource(rulesPath)
    val ee = ExtractorEngine(rules)

    // since we break a paper into sections, we'll need to group the mentions by doc
    // rule set only produces target RelationMentions

    val assembledMentions: Iterable[Mention] =
      for {
        // NOTE: Odin expects all mentions in the state to belong to the same doc!
        (doc, mentionsFromReach) <- existingMentions.groupBy(_.document)
        // create a new state with just the mentions from a particular doc
        // note that a doc is as granular as a section of a paper
        oldState = State(mentionsFromReach)
        // initialize a state with the subset of mentions
        // belonging to the same doc.
        m <- ee.extractFrom(doc, oldState)
        // ensure that mention is one related to Assembly
        // we don't want to return things from the old state
        if m matches precedenceMentionLabel
      } yield m

    assembledMentions
      .toSeq
  }
}
