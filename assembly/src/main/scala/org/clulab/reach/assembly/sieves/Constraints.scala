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

  def isNegated(m: Mention): Boolean = {
    val am = AssemblyManager(Seq(m))
    am.getEER(m).negated
  }

  def hasMultipleInputs(m: Mention): Boolean = {
    val am = AssemblyManager(Seq(m))
    val input = am.getEER(m) match {
      case event: Event => event.I
      case complex: Complex => complex.members
      case entity: Entity => Set(entity)
    }
    input.size > 1
  }

  // For a complex event "C", with a controlled "A", do not re-create "A precedes C"
  def notAnExistingComplexEvent(link: Mention):Boolean = {
    val before = link.arguments(beforeRole).head
    val after = link.arguments(afterRole).head
    val argsOfBefore: Set[Mention] = before.arguments.values.flatten.toSet
    val argsOfAfter: Set[Mention] = after.arguments.values.flatten.toSet

    !(argsOfBefore contains after) && !(argsOfAfter contains before)
  }

  // It's neither the case that x is a predecessor of y nor vice versa
  def noExistingPrecedence(
    x: Mention,
    y: Mention,
    am: AssemblyManager,
    // strict matching is the default
    ignoreMods: Boolean = false): Boolean = {
    noExistingPrecedence(am.getEER(x), am.getEER(y), ignoreMods)
  }

  // It's neither the case that x is a predecessor of y nor vice versa
  def noExistingPrecedence(x: EntityEventRepresentation, y: EntityEventRepresentation, ignoreMods: Boolean): Boolean = {
    !(x.manager.distinctPredecessorsOf(x).map(_.equivalenceHash(ignoreMods)) contains y.equivalenceHash(ignoreMods)) &&
      !(y.manager.distinctPredecessorsOf(y).map(_.equivalenceHash(ignoreMods)) contains x.equivalenceHash(ignoreMods))
  }

  //
  // Used by relation classifier & relation corpus
  //

  /** Checks if two Mentions have an equivalent EER */
  def areEquivalent(m1: Mention, m2: Mention, ignoreMods: Boolean): Boolean = {
    val am = AssemblyManager(Seq(m1, m2))
    am.getEER(m1).isEquivalentTo(am.getEER(m2), ignoreMods)
  }

  /** Checks if two Mentions share at least one SimpleEntity with the same grounding ID */
  def shareEntityGrounding(m1: Mention, m2: Mention): Boolean = {

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
        val input1 = AssemblyManager.getInputEntities(eer1)
        val input2 = AssemblyManager.getInputEntities(eer2)

        // check if inputs intersect in terms of grounding IDs
        fuzzyIntersects(input1, input2)

      case _ => false
    }
  }

  /** Determine if two mentions share an equivalent controlled/patient **/
  def shareControlleds(mention1: Mention, mention2: Mention, ignoreMods: Boolean): Boolean = {
    // resolve both event mentions
    val m1 = AssemblyManager.getResolvedForm(mention1)
    val m2 = AssemblyManager.getResolvedForm(mention2)

    val manager = AssemblyManager(Seq(mention1, mention2))

    def getControlled(m: Mention): Seq[Mention] = m match {
      case ce if ce matches "ComplexEvent" =>
        ce.arguments("controlled").map(AssemblyManager.getResolvedForm).flatMap(getControlled)
      case se if se matches "SimpleEvent" =>
        se.arguments("theme").map(AssemblyManager.getResolvedForm).flatMap(getControlled)
      case other => Seq(other)
    }

    val m1controlleds = getControlled(m1).map(manager.getEER).map(_.equivalenceHash(ignoreMods)).toSet
    val m2controlleds = getControlled(m2).map(manager.getEER).map(_.equivalenceHash(ignoreMods)).toSet

    m1controlleds.intersect(m2controlleds).nonEmpty
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

      // if this is a cross-sentence case, simply check that these mentions can be handled by the manager
      case (s1: Mention, s2: Mention) if s1.sentence != s2.sentence =>
        AssemblyManager.isValidMention(s1) && AssemblyManager.isValidMention(s2)

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

      // make sure the mentions can be handled by the manager
      case (o1: Mention, o2: Mention) =>
        AssemblyManager.isValidMention(o1) && AssemblyManager.isValidMention(o2)
    }
    // text spans should be unique
    (m1.text != m2.text) && ceConstraint &&
      // neither mention should be negated
      !isNegated(m1) && !isNegated(m2)
  }
}