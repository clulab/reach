package edu.arizona.sista.assembly

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.assembly
import collection.Map
import scala.util.hashing.MurmurHash3._

/**
 * Trait used for entity/event representations of a Mention.
 */
trait EntityEventRepresentation {
  /**
   * Whether or not the [[EntityEventRepresentation]] was produced by a Mention resolved through coref.
   * Must be implemented by classes which include the [[EntityEventRepresentation]] trait.
   * @return true or false
   */
  def coref: Boolean

  /**
   * Whether or not the [[EntityEventRepresentation]] is negated by its evidence (i.e., whether or not the evidence gives a negative example for this [[EntityEventRepresentation]]).
   * Must be implemented by classes which include the [[EntityEventRepresentation]] trait.
   * @return true or false
   */
  def negated: Boolean

  /**
   * The Set of Mentions serving as textual evidence for this [[EntityEventRepresentation]].
   * @return Set[Mention]
   */
  def evidence: Set[Mention] = manager.getEvidence(this)

  /**
   * A custom equality that ignores [[IDPointer]] information.  Used to compared derived classes of [[EntityEventRepresentation]].
   * Though not enforced, the implementation should make use of [[equivalenceHash]].
   * Must be implemented by classes which include the [[EntityEventRepresentation]] trait.
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any): Boolean

  /**
   * A hash used for equivalency comparisons of derived classes of [[EntityEventRepresentation]].
   * Must be implemented by classes which include the [[EntityEventRepresentation]] trait.
   * @return a hash (Int) representing a derived instance of [[EntityEventRepresentation]]
   */
  def equivalenceHash: Int

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
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean
}

/**
 * Trait for entity representations of a Mention.
 */
trait Entity extends EntityEventRepresentation {
  /**
   * Intended to provide a high-level summary of the [[Entity]]
   * @return a String summary of the [[Entity]]
   */
  def summarize: String
}

/**
 * A [[Entity]] representation of a Mention of a Protein, GGP, Simple_chemical, etc. (see the children of "Entity" in the taxonomy)
 * @param uniqueID [[IDPointer]] assigned to this [[SimpleEntity]] by the [[AssemblyManager]]
 * @param id [[GroundingID]] for the [[SimpleEntity]]
 * @param modifications a Set of [[AssemblyModification]], such as [[edu.arizona.sista.assembly.PTM]] and [[edu.arizona.sista.assembly.EntityLabel]].
 *                      These are relevant to the identity of the [[SimpleEntity]] and describe its state (ex. Phosphorylated @ Ser123).
 * @param coref whether or not the [[SimpleEntity]] was produced by a Mention resolved through coref
 * @param manager a pointer to the [[AssemblyManager]] instance that produced this [[SimpleEntity]]
 */
class SimpleEntity(
  val uniqueID: IDPointer,
  val id: GroundingID,
  val modifications: Set[AssemblyModification],
  val coref: Boolean,
  val negated: Boolean,
  val manager: AssemblyManager
) extends Entity {

  /**
   * Summary making use of [[id]], [[modifications]], [[coref]], and [[manager]].
   * @return a String summary of the [[SimpleEntity]]
   */
  def summarize: String =
    s"SimpleEntity(id=${this.id}, modifications=${this.modifications}, coref=${this.coref}, mngr=${this.manager})"

  /**
   * Returns the Set of [[assembly.PTM]] contained in [[modifications]].
   */
  def getPTMs: Set[assembly.PTM] = for {
    m: AssemblyModification <- modifications
    if m.isInstanceOf[assembly.PTM]
    ptm = m.asInstanceOf[assembly.PTM]
  } yield ptm

  /**
   * Returns the Set of [[assembly.PTM]] contained in [[modifications]] matching the given label.
   * @param label the label used to filter [[assembly.PTM]]
   */
  def getPTMs(label: String): Set[assembly.PTM] = for {
    m: AssemblyModification <- modifications
    if m.isInstanceOf[assembly.PTM]
    ptm = m.asInstanceOf[assembly.PTM]
    if ptm.label == label
  } yield ptm

  /**
   * Used by [[isEquivalentTo]] to compare against another [[SimpleEntity]].
   * @return a hash (Int) based primarily on the [[id]] and [[modsHash]]
   */
  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash("edu.arizona.sista.assembly.SimpleEntity")
    // a representation of the ID
    val h1 = mix(h0, id.hashCode)
    // a representation of the set of modifications
    val h2 = mix(h1, modsHash)
    // whether or not the representation is negated
    val h3 = mixLast(h2, negated.hashCode)
    finalizeHash(h3, 3)
  }

  /**
   * Hash representing the [[modifications]].
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @return an Int hash based on the hashcodes of the modifications
   */
  def modsHash: Int = {
    val h0 = stringHash("SimpleEntity.modifications")
    val hs = modifications.map(_.hashCode)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, modifications.size)
  }

  /**
   * Used to compare against another [[SimpleEntity]].
   * Based on the equality of [[equivalenceHash]] to that of another [[SimpleEntity]].
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any): Boolean = other match {
    case se: SimpleEntity => this.equivalenceHash == se.equivalenceHash
    case _ => false
  }

  /**
   * Whether or not the [[SimpleEntity]] contains the provided [[IDPointer]].
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean = {
    uniqueID == someID
  }
}

/**
 * A [[Entity]] representation of a Binding Mention.
 * @param memberPointers a Set of [[IDPointer]] corresponding to the Mentions serving as members to the [[Complex]]
 * @param uniqueID the [[IDPointer]] assigned to the [[Complex]] by the AssemblyManager
 * @param coref whether or not the [[Complex]] was produced by a Mention resolved through coref
 * @param manager a pointer to the [[AssemblyManager]] instance that produced this [[Complex]]
 */
class Complex(
  val uniqueID: IDPointer,
  val memberPointers: Set[IDPointer],
  val coref: Boolean,
  val negated: Boolean,
  // assembly manager used for the retrieval of EntityEventRepresentations
  val manager: AssemblyManager
) extends Entity {

  /**
   * The [[Entity]] Set of members, retrieved from [[manager.idToEERepresentation]] using the [[memberPointers]].
   * @return the [[Entity]] Set of [[Complex]] members
   */
  def members: Set[Entity] = memberPointers.map(m => manager.getEERepresentation(m).asInstanceOf[Entity])

  /**
   * Summary making use of [[members]], [[coref]], and [[manager]]
   * @return a String summary of the [[Complex]]
   */
  def summarize: String =
    s"Complex(members=${members.map(_.summarize).mkString("{", ", ", "}")}, coref=${this.coref}, mngr=${this.manager})"

  /**
   * Uses [[Entity.isEquivalentTo]] to check if an [[Entity]] is contained in the Set of [[members]].
   * @param other the thing to compare against
   * @return true or false
   */
  def contains(other: Any): Boolean = other match {
    case e: Entity => this.members exists(_.equivalenceHash == e.equivalenceHash)
    case _ => false
  }

  /**
   * Hash representing the [[members]].
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @return an Int hash based on the [[Entity.equivalenceHash]] of each member
   */
  def membersHash: Int = {
    val h0 = stringHash("Complex.members")
    val hs = members.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, members.size)
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[Complex]].
   * @return a hash (Int) based primarily on the [[membersHash]]
   */
  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash("edu.arizona.sista.assembly.Complex")
    // comprised of the equiv. hash of members
    val h1 = mix(h0, membersHash)
    // whether or not the representation is negated
    val h2 = mixLast(h1, negated.hashCode)
    finalizeHash(h2, 2)
  }

  /**
   * Used to compare against another [[Complex]].
   * Based on the equality of [[equivalenceHash]] to that of another [[Complex]].
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any): Boolean = other match {
    case complex: Complex => this.equivalenceHash == complex.equivalenceHash
    case _ => false
  }

  /**
   * Whether or not the [[Complex]] contains the provided [[IDPointer]].
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean = {
    uniqueID == someID || (memberPointers contains someID)
  }
}

/**
 * Trait for an Event representation of a Mention.
 */
trait Event extends EntityEventRepresentation

/**
 * Representation for any Mention with the label SimpleEvent.  Note that a Binding is represented using a [[Complex]].
 * @param uniqueID the [[IDPointer]] assigned to this [[SimpleEvent]] by the [[AssemblyManager]]
 * @param inputPointers a Set of [[IDPointer]] corresponding to the Mentions serving as input to the [[SimpleEvent]]
 * @param outputPointers a Set of [[IDPointer]] corresponding to the Mentions serving as output to the [[SimpleEvent]].
 *                       In practice, this is a single [[Entity]] with at least one [[edu.arizona.sista.assembly.PTM]] (corresponding to [[SimpleEvent.label]].
 * @param label the label of the SimpleEvent (ex. Phosphorylation, Farnesylation, etc)
 * @param coref whether or not the [[Complex]] was produced by a Mention resolved through coref
 * @param manager a pointer to the [[AssemblyManager]] instance that produced this [[Complex]]
 */
class SimpleEvent(
  val uniqueID: IDPointer,
  val inputPointers: Map[String, Set[IDPointer]],
  val outputPointers: Set[IDPointer],
  val label: String,
  val coref: Boolean,
  val negated: Boolean,
  val manager: AssemblyManager
) extends Event {

  // all elements of input must be Entities
  require(input.values.flatten.forall(_.isInstanceOf[Entity]), s"not all input elements of $label are entities")

  // all elements of output must be Entities
  require(output.forall(_.isInstanceOf[Entity]), s"not all output elements of $label are entities")

  /**
   * A representation of the argument roles and the [[Entity]] Set corresponding to each role (retrieved using using the [[manager.idToEERepresentation]] and the [[inputPointers]]).
   * @return a Map from argument role (a String) -> a Set of [[Entity]]
   */
  def input: Map[String, Set[Entity]] = {
    inputPointers.map(pair =>
      (pair._1, pair._2.map(
        // retrieve each id and cast as Entity
        id => manager.getEERepresentation(id).asInstanceOf[Entity]
        )
      )
    )
  }

  /**
   * A representation of the output of the [[SimpleEvent]].
   * @return an [[Entity]] Set retrieved using the [[manager.idToEERepresentation]] and the [[outputPointers]]
   */
  def output: Set[Entity] =
    outputPointers.map(id => manager.getEERepresentation(id).asInstanceOf[Entity])

  /**
   * Hash representing the [[input]].
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @return an Int hash based on hashes of the keys in the [[input]] and the [[Entity.equivalenceHash]] of each element contained in the corresponding value in the [[input]]
   */
  def inputHash: Int = {
    val h0 = stringHash("SimpleEvent.input")
    val hs = output.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, input.size)
  }

  /**
   * Hash representing the [[output]].
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @return an Int hash based on the [[Entity.equivalenceHash]] of each element in the [[output]]
   */
  def outputHash: Int = {
    val h0 = stringHash("SimpleEvent.output")
    val hs = output.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, output.size)
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[SimpleEvent]].
   * @return an Int hash based primarily on the [[label]], [[inputHash]], and [[outputHash]]
   */
  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash("edu.arizona.sista.assembly.SimpleEvent")
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
   * Used to compare against another [[SimpleEvent]].
   * Based on the equality of [[equivalenceHash]] to that of another [[SimpleEvent]].
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any): Boolean = other match {
    case se: SimpleEvent => this.equivalenceHash == se.equivalenceHash
    case _ => false
  }

  /**
   * Whether or not the [[SimpleEvent]] contains the provided [[IDPointer]].
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean = {
    uniqueID == someID || ((inputPointers.values.flatten.toSet ++ outputPointers) contains someID)
  }
}

/**
 * Representation of a Regulation event.
 * @param uniqueID the [[IDPointer]] assigned to this [[Regulation]]
 * @param controllerPointers a Set of [[IDPointer]] corresponding to the Mentions serving as controllers to the [[Regulation]].
 *                           It is a set because each Mention of a Regulation may have more than one controller, and each Mention contained in [[AssemblyManager.mentionToID]] points to exactly one [[IDPointer]] which corresponds to exactly one [[EntityEventRepresentation]] in [[AssemblyManager.idToEERepresentation]].
 * @param controlledPointers a Set of [[IDPointer]] corresponding to the Mentions serving as the controlled to the [[Regulation]].
 *                           It is a set because each Mention of a Regulation may have more than one controlled, and each Mention contained in [[AssemblyManager.mentionToID]] points to exactly one [[IDPointer]] which corresponds to exactly one [[EntityEventRepresentation]] in [[AssemblyManager.idToEERepresentation]].
 * @param polarity whether the [[Regulation]] is [[AssemblyManager.positive]], [[AssemblyManager.negative]], or [[AssemblyManager.unknown]]
 * @param coref whether or not the [[Complex]] was produced by a Mention resolved through coref
 * @param manager a pointer to the [[AssemblyManager]] instance that produced this [[Complex]]
 */
class Regulation(
  val uniqueID: IDPointer,
  val controllerPointers: Set[IDPointer],
  val controlledPointers: Set[IDPointer],
  val polarity: String,
  val coref: Boolean,
  val negated: Boolean,
  val manager: AssemblyManager) extends Event {

  /**
   * The [[EntityEventRepresentation]] Set corresponding to the referencing Regulation Mention's "controller" argument (retrieved using using the [[manager.idToEERepresentation]] and the [[controllerPointers]]).
   * @return a Set of [[EntityEventRepresentation]]
   */
  def controller: Set[EntityEventRepresentation] =
    controlledPointers.map(manager.getEERepresentation)

  /**
   * The [[EntityEventRepresentation]] Set corresponding to the referencing Regulation Mention's "controlled" argument (retrieved using using the [[manager.idToEERepresentation]] and the [[controlledPointers]]).
   * In REACH, the controlled of a Regulation can be either Binding or SimpleEvent converted into an Entity with a PTM.  Eventually, though, we will probably allow other Regulations.
   * @return a Set of [[EntityEventRepresentation]]
   */
  def controlled: Set[EntityEventRepresentation] =
    controlledPointers.map(id => manager.getEERepresentation(id))

  /**
   * Hash representing the [[controller]].
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @return an Int hash based on the [[EntityEventRepresentation.equivalenceHash]] of each element in the [[controller]]
   */
  private def controllerHash: Int = {
    val h0 = stringHash("Regulation.controller")
    val hs = controller.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, controller.size)
  }

  /**
   * Hash representing the [[controlled]].
   * Used by [[equivalenceHash]] for [[isEquivalentTo]] comparisons.
   * @return an Int hash based on the [[EntityEventRepresentation.equivalenceHash]] of each element in the [[controlled]]
   */
  private def controlledHash: Int = {
    val h0 = stringHash("Regulation.controlled")
    val hs = controlled.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, controlled.size)
  }

  /**
   * Used by [[isEquivalentTo]] to compare against another [[Regulation]].
   * @return an Int hash based primarily on the [[polarity]], [[controllerHash]], and [[controlledHash]]
   */
  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash("edu.arizona.sista.assembly.Regulation")
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
   * Used to compare against another [[Regulation]].
   * Based on the equality of [[equivalenceHash]] to that of another [[Regulation]].
   * @param other the thing to compare against
   * @return true or false
   */
  def isEquivalentTo(other: Any): Boolean = other match {
    // controller and controlled must be the same
    case reg: Regulation => this.equivalenceHash == reg.equivalenceHash
    case _ => false
  }

  /**
   * Whether or not the [[Regulation]] contains the provided [[IDPointer]].
   * @param someID an [[IDPointer]] identifying some [[EntityEventRepresentation]]
   * @return true or false
   */
  def containsID(someID: IDPointer): Boolean = {
    uniqueID == someID || ((controlledPointers ++ controllerPointers) contains someID)
  }
}