package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly._
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.odin.Mention
import scala.util.hashing.MurmurHash3._


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
 *
   * @return an Int hash based on the [[EntityEventRepresentation.equivalenceHash]] of each element in the [[controller]]
   */
  def controllerHash: Int = {
    val h0 = stringHash(s"$eerString.controller")
    val hs = controller.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, controller.size)
  }

  /**
   * Hash representing the [[controlled]]. <br>
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
 *
   * @return an Int hash based on the [[EntityEventRepresentation.equivalenceHash]] of each element in the [[controlled]]
   */
  def controlledHash: Int = {
    val h0 = stringHash(s"$eerString.controlled")
    val hs = controlled.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, controlled.size)
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[ComplexEvent]].
 *
   * @return an Int hash based on the [[polarity]], [[controllerHash]], [[controlledHash]], and [[negated.hashCode]]
   */
  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash(eerString)
    // the polarity of the Regulation
    val h1 = mix(h0, stringHash(polarity))
    // controller
    val h2 = mix(h1, controllerHash)
    // controlled
    val h3 = mix(h2, controlledHash)
    // whether or not the representation is negated
    val h4 = mixLast(h3, negated.hashCode)
    finalizeHash(h4, 4)
  }

  /**
   * Used to compare against another [[ComplexEvent]]. <br>
   * Based on the equality of [[equivalenceHash]] to that of another [[ComplexEvent]].
 *
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any): Boolean = other match {
    // controller and controlled must be the same
    case ce: ComplexEvent => this.equivalenceHash == ce.equivalenceHash
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

  def hasExactArgument(arg: EntityEventRepresentation): Boolean = {
    controller ++ controlled exists ( _.isEquivalentTo(arg) )
  }

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
