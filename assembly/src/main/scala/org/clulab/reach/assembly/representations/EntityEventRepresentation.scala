package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly._
import org.clulab.reach.mentions._
import org.clulab.odin.Mention


/**
 * Trait used for entity/event representations of a Mention.
 */
trait EntityEventRepresentation extends Serializable {

  val eerString = "assembly.EER"
  /**
   * The evidence from which this [[EntityEventRepresentation]] was constructed.
   */
  def sourceMention: Option[Mention]

  /**
   * Whether or not the [[EntityEventRepresentation]] was produced by a Mention resolved through coref.
 *
   * @return true or false
   */
  def coref: Boolean = if (sourceMention.nonEmpty) hasCorefResolution(sourceMention.get) else false

  /**
   * Whether or not the [[EntityEventRepresentation]] is negated by its evidence (i.e., whether or not the evidence gives a negative example for this [[EntityEventRepresentation]]).
 *
   * @return true or false
   */
  def negated: Boolean = if (sourceMention.nonEmpty) hasNegation(sourceMention.get) else false

  /**
   * The Set of Mentions serving as textual evidence for this [[EntityEventRepresentation]].
 *
   * @return Set[Mention]
   */
  def evidence: Set[Mention] = manager.getEvidence(this)

  /**
   * A custom equality that ignores [[IDPointer]] information.  Used to compared derived classes of [[EntityEventRepresentation]]. <br>
   * Though not enforced, the implementation should make use of [[equivalenceHash]]. <br>
   * Must be implemented by classes which include the [[EntityEventRepresentation]] trait.
   *
   * @param other the thing to compare against
   * @param ignoreMods whether or not to ignore modifications when assessing equivalence
   * @return true or false
   */
  def isEquivalentTo(other: Any, ignoreMods: Boolean): Boolean

  /**
   * A hash used for equivalency comparisons of derived classes of [[EntityEventRepresentation]]. <br>
   * Must be implemented by classes which include the [[EntityEventRepresentation]] trait.
   * @param ignoreMods whether or not to ignore modifications when assessing equivalenceHash
   * @return a hash (Int) representing a derived instance of [[EntityEventRepresentation]]
   */
  def equivalenceHash(ignoreMods: Boolean): Int

  /**
   * a pointer to the [[AssemblyManager]] instance that produced this [[EntityEventRepresentation]]
   */
  val manager: AssemblyManager

  /**
   * the [[IDPointer]] assigned to this [[EntityEventRepresentation]]
   */
  val uniqueID: IDPointer

  /**
   * Whether or not this [[EntityEventRepresentation]] contains a reference to the provided [[IDPointer]]
 *
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean

  //
  // evidence checks
  //

  /**
   * Checks whether evidence contains a Negation modification
 *
   * @param m an Odin Mention
   * @return true or false
   */
  def hasNegation(m: Mention): Boolean = {
    // get mention's coref resolution
    val cm: CorefMention = m.toCorefMention
    val ante = cm.antecedentOrElse(cm)

    ante match {
      // does the entity have a Negation mod?
      case entity if ante matches "Entity" =>
        entity.modifications exists (_.isInstanceOf[Negation])
      // does the event have a Negation mod OR do any of its arguments have a Negation mod?
      case event if event matches "Event" =>
        (event.modifications exists (_.isInstanceOf[Negation])) || (event.arguments.values.flatten exists hasNegation)
      case _ => false
    }
  }

  /**
   * Checks to see if a coref mention has an antecedent. <br>
   *
   * If the mentions made it through the coref component of reach,
   * the only mentions that might have an antecedent should be those with a "Generic_*" <br>
   * this is just a broader, fail-safe check...
 *
   * @param m an Odin Mention
   * @return true if cm has an antecedent; false otherwise
   */
  def hasCorefResolution(m: Mention): Boolean = {
    val cm = m.toCorefMention
    cm.antecedent.nonEmpty
  }
}
