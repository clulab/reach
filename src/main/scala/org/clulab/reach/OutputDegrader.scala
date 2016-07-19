package org.clulab.reach

import org.clulab.odin._
import org.clulab.reach.mentions._


/**
  * Degrade internal representations for output
  */
object OutputDegrader {

  /** Flattens nested controllers
    * TODO: Should this also flatten controlleds?
    */
  def flattenMentions(mentions: Seq[Mention]): Seq[BioMention] = for {
    mention <- mentions
  } yield flattenMention(mention)

  def flattenMention(m: Mention): BioMention = m.toBioMention match {

    // No flattening necessary
    case entity if entity matches "Entity" => entity

    // A Generic_event is essentially a trigger for a SimpleEvent (no theme is present)
    case generic if generic matches "Generic_event" => generic

    // SimpleEvents need not be modified
    case se if se matches "SimpleEvent" => se

    // Flatten controller of a ComplexEvent
    case ce if ce matches "ComplexEvent" =>
      ce.arguments.get("controller") match {
        // no controller
        // TODO: should controlled be flattened?
        case None => ce
        // a single controller
        case Some(Seq(controller)) => val flattenedController = flattenController(controller)
          val updatedArguments = ce.arguments.updated("controller", Seq(flattenedController))
          // TODO: should we flatten the controlled as well?
          val flattenedRepresentation = ce match {
            case rel: RelationMention => rel.copy(arguments = updatedArguments)
            case em: EventMention => em.copy(arguments = updatedArguments)
          }
          flattenedRepresentation.toBioMention
      }

    // Site, Cellular_component, CellLine, TissueType, Species, etc.
    case other => other
  }

  /** Recover controller from a potentially nested structure by recursively converting <br>
    * an Event to an Entity with the appropriate modifications representing its state. <br>
    * SimpleEvent -> theme + PTM <br>
    * Binding -> Complex (treated as an Entity) <br>
    * ComplexEvent -> recursive call on controller <br>
    */
  def flattenController(m: Mention, negated: Boolean = false): BioMention =
    DarpaActions.convertEventToEntity(m, asOutput = false, negated)
}