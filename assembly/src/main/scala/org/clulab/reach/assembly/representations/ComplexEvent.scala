package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly._
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.utils.Hash
import org.clulab.odin.Mention


/**
 * Representation of a ComplexEvent.
 */
trait ComplexEvent extends Event {
  /** The [[IDPointer]] assigned to this [[ComplexEvent]] */
  val uniqueID: IDPointer
  /**
    * A Set of [[IDPointer]] corresponding to the Mentions serving as controllers to the [[ComplexEvent]] <br>
    * It is a set because each Mention of a Regulation may have more than one controller, and each Mention contained in [[AssemblyManager.mentionToID]] points to exactly one [[IDPointer]] which corresponds to exactly one [[EntityEventRepresentation]] in [[AssemblyManager.idToEER]]
    * */
  val controllerPointers: Set[IDPointer]
  /**
   * A Set of [[IDPointer]] corresponding to the Mentions serving as the controlled to the [[ComplexEvent]] <br>
   * It is a set because each Mention of a Regulation may have more than one controlled, and each Mention contained in [[AssemblyManager.mentionToID]] points to exactly one [[IDPointer]] which corresponds to exactly one [[EntityEventRepresentation]] in [[AssemblyManager.idToEER]]
   */
  val controlledPointers: Set[IDPointer]
  /** Whether the [[ComplexEvent]] is [[AssemblyManager.positive]], [[AssemblyManager.negative]], or [[AssemblyManager.unknown]] */
  val polarity: String
  /** The Mention from which this [[ComplexEvent]] was constructed */
  val sourceMention: Option[Mention]
  /** a pointer to the [[AssemblyManager]] instance that produced this [[ComplexEvent]] */
  val manager: AssemblyManager

  override val eerString = "assembly.ComplexEvent"

  /**
   * The [[EntityEventRepresentation]] Set corresponding to the referencing Regulation Mention's "controller" argument (retrieved using using the [[manager.idToEER]] and the [[controllerPointers]]).
 *
   * @return a Set of [[EntityEventRepresentation]]
   */
  def controller: Set[EntityEventRepresentation] =
    controllerPointers.map(manager.getEER)

/**
   * The [[EntityEventRepresentation]] Set corresponding to the referencing Regulation Mention's "controlled" argument (retrieved using using the [[manager.idToEER]] and the [[controlledPointers]]).
 *
   * @return a Set of [[EntityEventRepresentation]]
   */
  def controlled: Set[EntityEventRepresentation] =
    controlledPointers.map(id => manager.getEER(id))

  /**
   * Hash representing the [[controller]]. <br>
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @param ignoreMods whether or not to ignore modifications when calculating the controllerHash
   * @return an Int hash based on the [[EntityEventRepresentation.equivalenceHash]] of each element in the [[controller]]
   */
  def controllerHash(ignoreMods: Boolean): Int = {
    val hs = controller.map(_.equivalenceHash(ignoreMods))

    Hash.withLast(controller.size)(
      Hash(s"$eerString.controller"),
      Hash.unordered(hs)
    )
  }

  /**
   * Hash representing the [[controlled]]. <br>
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @param ignoreMods whether or not to ignore modifications when calculating the controlledHash
   * @return an Int hash based on the [[EntityEventRepresentation.equivalenceHash]] of each element in the [[controlled]]
   */
  def controlledHash(ignoreMods: Boolean): Int = {
    val hs = controlled.map(_.equivalenceHash(ignoreMods))

    Hash.withLast(controlled.size)(
      Hash(s"$eerString.controlled"),
      Hash.unordered(hs)
    )
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[ComplexEvent]].
   * @param ignoreMods whether or not to ignore modifications when calculating the controlledHash
   * @return an Int hash based on the [[polarity]], [[controllerHash]], [[controlledHash]], and [[negated.hashCode]]
   */
  def equivalenceHash(ignoreMods: Boolean): Int = Hash.withLast(
    Hash(eerString),
    Hash(polarity),
    controllerHash(ignoreMods),
    controlledHash(ignoreMods),
    negated.hashCode
  )

  /**
   * Used to compare against another [[ComplexEvent]]. <br>
   * Based on the equality of [[equivalenceHash]] to that of another [[ComplexEvent]].
   * @param ignoreMods whether or not to ignore modifications when assessing equivalence
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any, ignoreMods: Boolean): Boolean = other match {
    // controller and controlled must be the same
    case ce: ComplexEvent => this.equivalenceHash(ignoreMods) == ce.equivalenceHash(ignoreMods)
    case _ => false
  }

  /**
   * Whether or not the [[ComplexEvent]] contains the provided [[IDPointer]].
   *
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean = {
    uniqueID == someID || ((controlledPointers ++ controllerPointers) contains someID)
  }

  // NOTE: this is only using controlled
  def I: Set[Entity] = controlled flatMap {
      case entity: Entity =>
        Set(entity)
      case simpleEvent: SimpleEvent =>
        simpleEvent.I
      case complexEvent: ComplexEvent =>
        complexEvent.I
  }

  // FIXME: should these include a modification (ex. Activated)?
  def O: Set[Entity] = I

  def hasArgument(arg: EntityEventRepresentation, ignoreMods: Boolean): Boolean = {
    controller ++ controlled exists ( _.isEquivalentTo(arg, ignoreMods) )
  }

  def hasExactArgument(arg: EntityEventRepresentation): Boolean = hasArgument(arg, ignoreMods = false)

  def hasApproximateArgument(arg: SimpleEntity): Boolean = I.exists {
    // are the grounding ids the same?
    case entity: SimpleEntity =>
      entity.grounding == arg.grounding
    // does at least one member of the complex
    // share a grounding id with the provided arg?
    case complex: Complex =>
      complex.flattenMembers.exists(_.grounding == arg.grounding)
  }
}
