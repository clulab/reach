package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly._
import org.clulab.odin.Mention
import org.clulab.utils.Hash


/**
 * A [[SimpleEntity]] representation of a Mention of a Protein, GGP, Simple_chemical, etc. (see the children of "Entity" in the taxonomy)
 *
 * @param uniqueID [[IDPointer]] assigned to this [[SimpleEntity]] by the [[AssemblyManager]]
 * @param grounding [[GroundingID]] for the [[SimpleEntity]]
 * @param modifications a Set of [[AssemblyModification]], such as [[representations.PTM]] and [[EntityLabel]]. <br>
 *                      These are relevant to the identity of the [[SimpleEntity]] and describe its state (ex. Phosphorylated @ Ser123).
 * @param sourceMention the Mention from which this [[SimpleEntity]] was constructed
 * @param manager a pointer to the [[AssemblyManager]] instance that produced this [[SimpleEntity]]
 */
class SimpleEntity(
  val uniqueID: IDPointer,
  val grounding: GroundingID,
  val modifications: Set[AssemblyModification],
  val sourceMention: Option[Mention],
  val manager: AssemblyManager
) extends Entity {

  override val eerString = "assembly.SimpleEntity"
  /**
   * Summary making use of [[grounding]], [[modifications]], [[coref]], and [[manager]].
   *
   * @return a String summary of the [[SimpleEntity]]
   */
  def summarize: String = {
    val src = if (sourceMention.nonEmpty) s"${sourceMention.get} w/ text '${sourceMention.get.text}'" else "??"
    s"SimpleEntity(grounding=${this.grounding}, modifications=${this.modifications}, coref=${this.coref}, mngr=${this.manager}, sourceMention=$src})"
  }

  /**
   * Returns the Set of [[representations.PTM]] contained in [[modifications]].
   */
  def getPTMs: Set[representations.PTM] = for {
    m: AssemblyModification <- modifications
    if m.isInstanceOf[representations.PTM]
    ptm = m.asInstanceOf[representations.PTM]
  } yield ptm

  /**
   * Returns the Set of [[representations.PTM]] contained in [[modifications]] matching the given label.
   *
   * @param label the label used to filter [[representations.PTM]]
   */
  def getPTMs(label: String): Set[representations.PTM] = for {
    m: AssemblyModification <- modifications
    if m.isInstanceOf[representations.PTM]
    ptm = m.asInstanceOf[representations.PTM]
    if ptm.label == label
  } yield ptm

  /**
   * Check if SimpleEvent is negated
   *
   * @return true or false
   */
  override def negated: Boolean = {
    getPTMs.exists(_.negated == true)
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[SimpleEntity]].
   * @param ignoreMods whether or not to ignore modifications when calculating the equivalenceHash
   * @return a hash (Int) based primarily on the [[grounding]] and [[modsHash]]
   */
  def equivalenceHash(ignoreMods: Boolean): Int = {
    val h0 = Hash(eerString)
    val h1 = grounding.hashCode // a representation of the ID
    val h3 = negated.hashCode

    if (ignoreMods) Hash.withLast(h0, h1,           h3)
    else            Hash.withLast(h0, h1, modsHash, h3)
  }

  /**
   * Hash representing the [[modifications]]. <br>
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   *
   * @return an Int hash based on the hashcodes of the modifications
   */
  def modsHash: Int = {
    val hs = modifications.map(_.hashCode)

    Hash.withLast(modifications.size)(
      Hash(s"$eerString.modifications"),
      Hash.unordered(hs)
    )
  }

  /**
   * Used to compare against another [[SimpleEntity]]. <br>
   * Based on the equality of [[equivalenceHash]] to that of another [[SimpleEntity]].
   *
   * @param other the thing to compare against
   * @param ignoreMods whether or not to ignore modifications when assessing equivalence
   * @return true or false
   */
  def isEquivalentTo(other: Any, ignoreMods: Boolean): Boolean = other match {
    case se: SimpleEntity => this.equivalenceHash(ignoreMods) == se.equivalenceHash(ignoreMods)
    case _ => false
  }

  /**
   * Whether or not the [[SimpleEntity]] contains the provided [[IDPointer]].
   *
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean = {
    uniqueID == someID
  }

  /**
   * Find SimpleEntities with simple same grounding
   */
  def withSameGrounding: Set[SimpleEntity] = {
    for {
      se <- manager.getSimpleEntities
      if se.grounding == this.grounding
    } yield se
  }
}
