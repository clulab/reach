package edu.arizona.sista.assembly

trait EntityEventRepresentation {
  // whether or not the representation comes from a coref mention
  def coref: Boolean
}

trait Entity extends EntityEventRepresentation

class SimpleEntity (
  val id: GroundingID,
  val modifications: Set[AssemblyModification],
  val coref: Boolean,
  val manager: AssemblyManager
) extends Entity

class Complex (
  val memberPointers: Set[IDPointer],
  // complicated in that this is true if any member is resolved through coref
  val coref: Boolean,
  // assembly manager used for the retrieval of EntityEventRepresentations
  val manager: AssemblyManager
) extends Entity {
  // the actual members of the complex
  def members = memberPointers.map(manager.getEERepresentation)
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
  require(input.forall(_.isInstanceOf[Entity]))

  // all elements of output must be Entities
  require(output.forall(_.isInstanceOf[Entity]))

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
}

class Regulation (
  val controllerPointer: Option[IDPointer],
  val controlledPointers: Set[IDPointer],
  val polarity: String,
  val coref: Boolean,
  val manager: AssemblyManager) extends Event {

  def controller: Option[Event] = controllerPointer match {
    case Some(id) => Some(manager.getEERepresentation(id).asInstanceOf[Event])
    case _ => None
  }
  def controlled: Set[Entity] =
    controlledPointers.map(id => manager.getEERepresentation(id).asInstanceOf[Entity])

}