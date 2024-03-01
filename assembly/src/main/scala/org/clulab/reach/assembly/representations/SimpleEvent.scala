package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly._
import org.clulab.odin.Mention
import org.clulab.utils.Hash
import scala.collection.Map

/**
 * Representation for any Mention with the label SimpleEvent.  Note that a Binding is represented using a [[Complex]].
 *
 * @param uniqueID the [[IDPointer]] assigned to this [[SimpleEvent]] by the [[AssemblyManager]]
 * @param inputPointers a Set of [[IDPointer]] corresponding to the Mentions serving as input to the [[SimpleEvent]]
 * @param outputPointers a Set of [[IDPointer]] corresponding to the Mentions serving as output to the [[SimpleEvent]] <br>
 *                       In practice, this is a single [[Entity]] with at least one [[representations.PTM]] (corresponding to [[SimpleEvent.label]].
 * @param label the label of the SimpleEvent (ex. Phosphorylation, Farnesylation, etc)
 * @param sourceMention the Mention from which this [[SimpleEvent]] was constructed
 * @param manager a pointer to the [[AssemblyManager]] instance that produced this [[SimpleEvent]]
 */
class SimpleEvent(
  val uniqueID: IDPointer,
  val inputPointers: Map[String, Set[IDPointer]],
  val outputPointers: Set[IDPointer],
  val label: String,
  val sourceMention: Option[Mention],
  val manager: AssemblyManager
) extends Event {

  override val eerString = "assembly.SimpleEvent"

  // all elements of input must be Entities
  require(input.values.flatten.forall(_.isInstanceOf[Entity]), s"not all input elements of $label are entities")

  // all elements of output must be Entities
  require(output.forall(_.isInstanceOf[Entity]), s"not all output elements of $label are entities")

  /**
   * A representation of the argument roles and the [[Entity]] Set corresponding to each role (retrieved using using the [[manager.idToEER]] and the [[inputPointers]]).
 *
   * @return a Map from argument role (a String) -> a Set of [[Entity]]
   */
  def input: Map[String, Set[Entity]] = {
    inputPointers.map(pair =>
      (pair._1, pair._2.map(
        // retrieve each id and cast as Entity
        id => manager.getEER(id).asInstanceOf[Entity]
        )
      )
    )
  }

  /**
   * A representation of the output of the [[SimpleEvent]].
 *
   * @return an [[Entity]] Set retrieved using the [[manager.idToEER]] and the [[outputPointers]]
   */
  def output: Set[Entity] =
    outputPointers.map(id => manager.getEER(id).asInstanceOf[Entity])

  /**
   * Hash representing the [[input]]. <br>
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @param ignoreMods whether or not to ignore modifications when calculating the inputHash
   * @return an Int hash based on hashes of the keys in the [[input]] and the [[Entity.equivalenceHash]] of each element contained in the corresponding value in the [[input]]
   */
  def inputHash(ignoreMods: Boolean): Int = {
    val hs = output.map(_.equivalenceHash(ignoreMods))

    Hash.withLast(input.size)(
      Hash(s"$eerString.input"),
      Hash.unordered(hs)
    )
  }

  /**
   * Hash representing the [[output]]. <br>
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @param ignoreMods whether or not to ignore modifications when calculating the outputHash
   * @return an Int hash based on the [[Entity.equivalenceHash]] of each element in the [[output]]
   */
  def outputHash(ignoreMods: Boolean): Int = {
    val hs = output.map(_.equivalenceHash(ignoreMods))

    Hash.withLast(output.size)(
      Hash(s"$eerString.output"),
      Hash.unordered(hs)
    )
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[SimpleEvent]].
   * @param ignoreMods whether or not to ignore modifications when calculating the equivalenceHash
   * @return an Int hash based primarily on the [[label]], [[inputHash]], and [[outputHash]]
   */
  def equivalenceHash(ignoreMods: Boolean): Int = Hash.withLast(
    Hash(eerString),
    label.hashCode,
    inputHash(ignoreMods),
    outputHash(ignoreMods),
    negated.hashCode
  )

  /**
   * Used to compare against another [[SimpleEvent]]. <br>
   * Based on the equality of [[equivalenceHash]] to that of another [[SimpleEvent]].
   * @param other the thing to compare against
   * @param ignoreMods whether or not to ignore modifications when assessing equivalence
   * @return true or false
   */
  def isEquivalentTo(other: Any, ignoreMods: Boolean): Boolean = other match {
    case complex: Complex => this.label match {
      // Binding being compared to a complex
      case "Binding" => this.outputHash(ignoreMods) == complex.membersHash(ignoreMods)
      // Complex cannot be equivalent to an event that is not a binding
      case _ => false
    }
    case se: SimpleEvent => this.equivalenceHash(ignoreMods) == se.equivalenceHash(ignoreMods)
    case _ => false
  }

  /**
   * Whether or not the [[SimpleEvent]] contains the provided [[IDPointer]]. <br>
   *
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean = {
    uniqueID == someID || ((inputPointers.values.flatten.toSet ++ outputPointers) contains someID)
  }

  def I: Set[Entity] = input("theme")
  def O: Set[Entity] = output


  def hasArgument(arg: EntityEventRepresentation, ignoreMods: Boolean): Boolean = {
    input.values.flatten exists ( _.isEquivalentTo(arg, ignoreMods) )
  }

  def hasExactArgument(arg: EntityEventRepresentation): Boolean = hasArgument(arg, ignoreMods = false)

  def hasApproximateArgument(arg: SimpleEntity): Boolean = {
    input.values.flatten exists {
      case entity: SimpleEntity =>
        entity.grounding == arg.grounding
      case complex: Complex =>
        complex.flattenMembers exists (_.grounding == arg.grounding)
    }
  }
}
