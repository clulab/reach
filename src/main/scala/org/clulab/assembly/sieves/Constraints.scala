package org.clulab.assembly.sieves

import org.clulab.assembly.AssemblyManager
import org.clulab.assembly.representations.{Complex, EntityEventRepresentation, Event, SimpleEntity}
import org.clulab.odin.Mention


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

  // It's neither the case that x is a predecessor of y nor vice versa
  def noExistingPrecedence(x: Mention, y: Mention, am:AssemblyManager): Boolean = {
    noExistingPrecedence(am.getEER(x), am.getEER(y))
  }

  // It's neither the case that x is a predecessor of y nor vice versa
  def noExistingPrecedence(x: EntityEventRepresentation, y: EntityEventRepresentation): Boolean = {
    !(x.manager.distinctPredecessorsOf(x).map(_.equivalenceHash) contains y.equivalenceHash) &&
      !(y.manager.distinctPredecessorsOf(y).map(_.equivalenceHash) contains x.equivalenceHash)
  }

  //
  // Used by relation classifier & relation corpus
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
  def isValidRelationPair(mention1: Mention, mention2: Mention): Boolean = {
    // resolve both event mentions
    val m1 = AssemblyManager.getResolvedForm(mention1)
    val m2 = AssemblyManager.getResolvedForm(mention2)
    // a regulation should not be paired with its controlled
    // ex: "inhibited" neg-regs "activation". remove interactions between Regulations and their Controlled
    val ceConstraint: Boolean = (m1, m2) match {
      // make sure both mentions can be handled by the AssemblyManager
      case (p1: Mention, p2: Mention) if !AssemblyManager.isValidMention(p1) || !AssemblyManager.isValidMention(p2) => false
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
