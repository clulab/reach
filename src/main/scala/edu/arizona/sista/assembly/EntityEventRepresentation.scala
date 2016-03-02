package edu.arizona.sista.assembly

import collection.Map
import scala.util.hashing.MurmurHash3._

trait EntityEventRepresentation {
  // whether or not the representation comes from a coref mention
  def coref: Boolean
  def isEquivalentTo(other: Any): Boolean

  def equivalenceHash: Int
}

trait Entity extends EntityEventRepresentation {
  def summarize: String
}

class SimpleEntity (
  val id: GroundingID,
  val modifications: Set[AssemblyModification],
  val coref: Boolean,
  val manager: AssemblyManager
) extends Entity {

  def summarize: String =
    s"SimpleEntity(id=${this.id}, modifications=${this.modifications}, coref=${this.coref}, mngr=${this.manager})"

  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash("edu.arizona.sista.assembly.SimpleEntity")
    // a representation of the ID
    val h1 = mix(h0, id.hashCode)
    // a representation of the set of modifications
    val h2 = mixLast(h1, modsHash)
    finalizeHash(h2, 2)
  }

  // hash of mods is sum of mods hashes
  def modsHash: Int = modifications.map(_.hashCode).sum

  def isEquivalentTo(other: Any): Boolean = other match {
    case se: SimpleEntity => this.equivalenceHash == se.equivalenceHash
    case _ => false
  }
}

class Complex (
  val memberPointers: Set[IDPointer],
  // complicated in that this is true if any member is resolved through coref
  val coref: Boolean,
  // assembly manager used for the retrieval of EntityEventRepresentations
  val manager: AssemblyManager
) extends Entity {

  // the actual members of the complex
  def members: Set[Entity] = memberPointers.map(m => manager.getEERepresentation(m).asInstanceOf[Entity])
  // members should be entities
  require(members.forall(_.isInstanceOf[Entity]), "All members of complex must be a kind of Entity")

  def summarize: String =
    s"Complex(members=${members.map(_.summarize).mkString("{", ", ", "}")}, coref=${this.coref}, mngr=${this.manager})"

  def membersHash: Int = {
    val h0 = stringHash("Complex.members")
    val hs = members.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, members.size)
  }

  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash("edu.arizona.sista.assembly.Complex")
    // comprised of the equiv. hash of members
    val h1 = mixLast(h0, members.map(_.equivalenceHash).sum)
    finalizeHash(h1, 1)
  }

  def contains(other: EntityEventRepresentation): Boolean = other match {
    // FIXME: should be able to just use this.members contains e
    case e: Entity => this.members exists(_.equivalenceHash == e.equivalenceHash)
    case _ => false
  }
  
  def isEquivalentTo(other: Any): Boolean = other match {
    case complex: Complex => this.equivalenceHash == complex.equivalenceHash
    case _ => false
  }
}


trait Event extends EntityEventRepresentation

class SimpleEvent (
  val inputPointers: Map[String, Set[IDPointer]],
  val outputPointers: Set[IDPointer],
  val label: String,
  val coref: Boolean,
  val manager: AssemblyManager
) extends Event {

  // all elements of input must be Entities
  require(input.values.flatten.forall(_.isInstanceOf[Entity]), s"not all input elements of $label are entities")

  // all elements of output must be Entities
  require(output.forall(_.isInstanceOf[Entity]), s"not all output elements of $label are entities")

  def input: Map[String, Set[Entity]] = {
    inputPointers.map(pair =>
      (pair._1, pair._2.map(
        // retrieve each id and cast as Entity
        id => manager.getEERepresentation(id).asInstanceOf[Entity]
        )
      )
    )
  }

  def output: Set[Entity] =
    outputPointers.map(id => manager.getEERepresentation(id).asInstanceOf[Entity])

  def inputHash: Int = {
    val h0 = stringHash("SimpleEvent.input")
    val hs = output.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, input.size)
  }

  def outputHash: Int = {
    val h0 = stringHash("SimpleEvent.output")
    val hs = output.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, output.size)
  }

  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash("edu.arizona.sista.assembly.SimpleEvent")
    // the label of the SimpleEvent
    val h1 = mix(h0, label.hashCode)
    // the input of the SimpleEvent
    val h2 = mix(h1, inputHash)
    // the output of the SimpleEvent
    val h3 = mixLast(h2, outputHash)
    finalizeHash(h3, 3)
  }

  def isEquivalentTo(other: Any): Boolean = other match {
    case se: SimpleEvent => this.equivalenceHash == se.equivalenceHash
    case _ => false
  }
}

class Regulation (
  val controllerPointers: Set[IDPointer],
  val controlledPointers: Set[IDPointer],
  val polarity: String,
  val coref: Boolean,
  val manager: AssemblyManager) extends Event {

  def controller: Set[EntityEventRepresentation] =
    controlledPointers.map(manager.getEERepresentation)

  def controlled: Set[SimpleEvent] =
    controlledPointers.map(id => manager.getEERepresentation(id).asInstanceOf[SimpleEvent])

  private def controllerHash: Int = {
    val h0 = stringHash("Regulation.controller")
    val hs = controller.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, controller.size)
  }

  private def controlledHash: Int = {
    val h0 = stringHash("Regulation.controlled")
    val hs = controlled.map(_.equivalenceHash)
    val h = mixLast(h0, unorderedHash(hs))
    finalizeHash(h, controlled.size)
  }

  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash("edu.arizona.sista.assembly.Regulation")
    // the polarity of the Regulation
    val h1 = mix(h0, stringHash(polarity))
    // controller
    val h2 = mix(h1, controllerHash)
    // controlled
    val h3 = mixLast(h2, controlledHash)
    finalizeHash(h3, 3)
  }

  def isEquivalentTo(other: Any): Boolean = other match {
    // controller and controlled must be the same
    case reg: Regulation => this.equivalenceHash == reg.equivalenceHash
    case _ => false
  }
}