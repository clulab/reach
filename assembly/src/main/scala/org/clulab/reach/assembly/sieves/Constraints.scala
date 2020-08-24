package org.clulab.reach.assembly.sieves

import org.clulab.reach.assembly.{AssemblyManager, GroundingID}
import org.clulab.reach.assembly.representations._
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

  /** Checks if two Mentions share at least one SimpleEntity with the same grounding ID */
  def shareArg(m1: Mention, m2: Mention): Boolean = {

    def getInput(eer: EntityEventRepresentation): Set[Entity] = eer match {
      case complex: Complex =>
        // a complex could contain another complex, so flatten
        // until members are all simple entities
        // then cast each as Entity for uniformity
        complex.flattenMembers.map(_.asInstanceOf[Entity])
      case entity: Entity => Set(entity)
      case simpleEvent: SimpleEvent => simpleEvent.I
      case complexEvent: ComplexEvent => complexEvent.I
    }

    /** Check if two sets of [[Entity]] have an approximate intersection in terms of [[GroundingID]] */
    def fuzzyIntersects(s1: Set[Entity], s2: Set[Entity]): Boolean = {

      def getSimpleEntities(entities: Set[Entity]): Set[SimpleEntity] =
        entities.filter(_.isInstanceOf[SimpleEntity]).map(_.asInstanceOf[SimpleEntity])

      def getGroundingIDs(entities: Set[Entity]): Set[GroundingID] =
        getSimpleEntities(entities).map(_.grounding)

      // check if any simple entities have intersecting grounding IDs
      getGroundingIDs(s1).intersect(getGroundingIDs(s2)).nonEmpty
    }

    // test mention pair

    (m1, m2) match {
      // can the mentions be handled by the assembly manager?
      case (v1, v2) if AssemblyManager.isValidMention(v1) && AssemblyManager.isValidMention(v2) =>
        val mngr = AssemblyManager(Seq(v1, v2))
        // retrieve EER corresponding to each mentions
        val eer1 = mngr.getEER(v1)
        val eer2 = mngr.getEER(v2)
        // get input sets
        val input1 = getInput(eer1)
        val input2 = getInput(eer2)

        // check if inputs intersect in terms of grounding IDs
        fuzzyIntersects(input1, input2)

      case _ => false
    }
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

  /**
    * Ensure mention pair is within the allotted sentential window
    *
    * @param m1 [[org.clulab.odin.Mention]]
    * @param m2 [[org.clulab.odin.Mention]]
    * @param window the maximum sentential distance a pair of mentions may occur within
    * @return
    */
  def withinWindow(m1: Mention, m2: Mention, window: Int): Boolean = {
    require(window >= 0, "window size must be >= 0")
    // mentions must be from same document
    (m1.document == m2.document) &&
      // mentions must be in specified sentential window
      Math.abs(m1.sentence - m2.sentence) <= window
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