package org.clulab.reach.assembly.representations

import org.clulab.odin.Mention
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly._
import scala.util.hashing.MurmurHash3._


/**
 * A [[Entity]] representation of a Binding Mention.
 *
 * @param memberPointers a Set of [[IDPointer]] corresponding to the Mentions serving as members to the [[Complex]]
 * @param uniqueID the [[IDPointer]] assigned to the [[Complex]] by the AssemblyManager
 * @param sourceMention the Mention from which this [[Complex]] was constructed
 * @param manager a pointer to the [[AssemblyManager]] instance that produced this [[Complex]]
 */
class Complex(
  val uniqueID: IDPointer,
  val memberPointers: Set[IDPointer],
  val sourceMention: Option[Mention],
  // assembly manager used for the retrieval of EntityEventRepresentations
  val manager: AssemblyManager
) extends Entity {

  override val eerString = "assembly.Complex"

  /**
   * The [[Entity]] Set of members, retrieved from [[manager.idToEER]] using the [[memberPointers]].
 *
   * @return the [[Entity]] Set of [[Complex]] members
   */
  def members: Set[Entity] = memberPointers.map(m => manager.getEER(m).asInstanceOf[Entity])

  /**
   * Summary making use of [[members]], [[coref]], and [[manager]]
 *
   * @return a String summary of the [[Complex]]
   */
  def summarize: String =
    s"Complex(members=${members.map(_.summarize).mkString("{", ", ", "}")}, coref=${this.coref}, mngr=${this.manager})"

  /**
   * Uses [[Entity.isEquivalentTo]] to check if an [[Entity]] is contained in the Set of [[members]].
 *
   * @param other the thing to compare against
   * @param ignoreMods whether or not to ignore modifications when determining containment
   * @return true or false
   */
  def contains(other: Any, ignoreMods: Boolean): Boolean = other match {
    case e: Entity => this.members exists(_.equivalenceHash(ignoreMods) == e.equivalenceHash(ignoreMods))
    case _ => false
  }

  /**
   * Hash representing the [[members]]. <br>
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @param ignoreMods whether or not to ignore modifications when calculating the membersHash
   * @return an Int hash based on the [[Entity.equivalenceHash]] of each member
   */
  def membersHash(ignoreMods: Boolean): Int = {
    val h0 = stringHash(s"$eerString.members")
    val hs = members.map(_.equivalenceHash(ignoreMods))
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, members.size)
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[Complex]].
   * @param ignoreMods whether or not to ignore modifications when calculating the equivalenceHash
   * @return a hash (Int) based primarily on the [[membersHash]]
   */
  def equivalenceHash(ignoreMods: Boolean): Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash(eerString)
    // comprised of the equiv. hash of members
    val h1 = mix(h0, membersHash(ignoreMods))
    // whether or not the representation is negated
    val h2 = mixLast(h1, negated.hashCode)
    finalizeHash(h2, 2)
  }

  /**
   * Used to compare against another [[Complex]]. <br>
   * Based on the equality of [[equivalenceHash]] to that of another [[Complex]].
   * @param ignoreMods whether or not to ignore modifications when assessing equivalence
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any, ignoreMods: Boolean): Boolean = other match {
    case complex: Complex => this.equivalenceHash(ignoreMods) == complex.equivalenceHash(ignoreMods)
    case _ => false
  }

  /**
   * Whether or not the [[Complex]] contains the provided [[IDPointer]].
 *
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean = {
    uniqueID == someID || (memberPointers contains someID)
  }

  /** Recurse over members until SimpleEntities are revealed */
  def flattenMembers: Set[SimpleEntity] = {
    val ses = members flatMap {
      case entity: SimpleEntity => Seq(entity)
      case complex: Complex => Seq(complex.flattenMembers)
    }
    ses.map(_.asInstanceOf[SimpleEntity])
  }
}
