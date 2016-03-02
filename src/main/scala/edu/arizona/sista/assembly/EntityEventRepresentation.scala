package edu.arizona.sista.assembly

import collection.Map

trait EntityEventRepresentation {
  // whether or not the representation comes from a coref mention
  def coref: Boolean
  def isEquivalentTo(other: EntityEventRepresentation): Boolean
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

  def isEquivalentTo(other: EntityEventRepresentation): Boolean = other match {
    // FIXME: understand why == doesn't work here...seems very strange
    // ...two SimpleEvents with the same id, modifications, coref, and manager are not necessarily equal...why?
    case se: SimpleEntity => this.id == se.id && this.modifications == se.modifications
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

  def contains(other: EntityEventRepresentation): Boolean = other match {
    // FIXME: should be able to just use this.members contains e
    case e: Entity => this.members exists(_ isEquivalentTo e)
    case _ => false
  }
  
  def isEquivalentTo(other: EntityEventRepresentation): Boolean = other match {
    case complex: Complex => this.members.forall(m => complex.members contains m)
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

  def isEquivalentTo(other: EntityEventRepresentation): Boolean = other match {
    // FIXME: should be able to just say a.input == b.input && a.output == b.output && a.label == b.label
    case se: SimpleEvent =>
      // both events must have same label
      this.label == se.label &&
      // both events must have same roles
      this.input.keys.toSet == se.input.keys.toSet &&
      // input values for each role must be equivalent
        this.input.forall(pair =>
          pair._2.forall{
            case i: Entity => se.input(pair._1: String) exists (_.isEquivalentTo(i))
          }
        ) &&
        // output should be the same size
        this.output.size == se.output.size &&
      // the output must be equivalent
      this.output.forall(o => output exists(_ isEquivalentTo(o)))
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

  def isEquivalentTo(other: EntityEventRepresentation): Boolean = other match {
    // controller and controlled must be the same
    case reg: Regulation => this.controller == reg.controller && reg.controlled == reg.controlled
    case _ => false
  }
}