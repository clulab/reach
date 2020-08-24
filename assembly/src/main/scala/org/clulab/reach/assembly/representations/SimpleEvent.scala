package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly._
import org.clulab.odin.Mention
import scala.collection.Map
import scala.util.hashing.MurmurHash3._


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
 *
   * @return an Int hash based on hashes of the keys in the [[input]] and the [[Entity.equivalenceHash]] of each element contained in the corresponding value in the [[input]]
   */
  def inputHash: Int = {
    val h0 = stringHash(s"$eerString.input")
    val hs = output.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, input.size)
  }

  /**
   * Hash representing the [[output]]. <br>
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
 *
   * @return an Int hash based on the [[Entity.equivalenceHash]] of each element in the [[output]]
   */
  def outputHash: Int = {
    val h0 = stringHash(s"$eerString.output")
    val hs = output.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, output.size)
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[SimpleEvent]].
 *
   * @return an Int hash based primarily on the [[label]], [[inputHash]], and [[outputHash]]
   */
  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash(eerString)
    // the label of the SimpleEvent
    val h1 = mix(h0, label.hashCode)
    // the input of the SimpleEvent
    val h2 = mix(h1, inputHash)
    // the output of the SimpleEvent
    val h3 = mix(h2, outputHash)
    // whether or not the representation is negated
    val h4 = mixLast(h3, negated.hashCode)
    finalizeHash(h4, 4)
  }

  /**
   * Used to compare against another [[SimpleEvent]]. <br>
   * Based on the equality of [[equivalenceHash]] to that of another [[SimpleEvent]].
 *
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any): Boolean = other match {
    case se: SimpleEvent => this.equivalenceHash == se.equivalenceHash
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

  def hasExactArgument(arg: EntityEventRepresentation): Boolean = {
    input.values.flatten exists ( _.isEquivalentTo(arg) )
  }

  def hasApproximateArgument(arg: SimpleEntity): Boolean = {
    input.values.flatten exists {
      case entity: SimpleEntity =>
        entity.grounding == arg.grounding
      case complex: Complex =>
        complex.flattenMembers exists (_.grounding == arg.grounding)
    }
  }
}
