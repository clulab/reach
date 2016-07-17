package org.clulab.reach

import org.clulab.odin._
import org.clulab.reach.mentions._
import scala.annotation.tailrec


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
  }

  /** Recursively converts an Event to an Entity with the appropriate modifications representing its state.
    * SimpleEvent -> theme + PTM <br>
    * Binding -> Complex (treated as an Entity) <br>
    * ComplexEvent -> recursive call on controller <br>
    */
  @tailrec
  final def flattenController(m: Mention, negated: Boolean = false): BioMention = m.toBioMention match {

    // no conversion needed
    // FIXME: consider case of "activated RAS".
    // We may want to add a Negation mod to this entity conditionally
    // or add a PTM with the label "UNKNOWN" and negate it (depending on value of negated)
    case entity if entity matches "Entity" => entity

    // These are event triggers used by coref (a SimpleEvent without a theme)
    // FIXME: should these be handled differently?
    case generic if generic matches "Generic_event" => generic

    // convert a binding into a Complex so that it is treated as an entity
    case binding if binding matches "Binding" =>
      new BioRelationMention(
        taxonomy.hypernymsFor("Complex"),
        binding.arguments,
        binding.sentence,
        binding.document,
        binding.keep,
        binding.foundBy
      )

    // convert event to PTM on its theme.
    // negate PTM according to current value of "negated"
    // (this SimpleEvent may have been the controlled to some negative ComplexEvent)
    case se: BioEventMention if se matches "SimpleEvent" =>
      // get the theme of the event (assume only one theme)
      val entity = se.arguments("theme").head.toBioMention
      // get an optional site (assume only one site)
      val siteOption = se.arguments.get("site").map(_.head)
      // create new mention for the entity
      val modifiedEntity = new BioTextBoundMention(entity)
      // attach a modification based on the event trigger
      val label = DarpaActions.getModificationLabel(se.label)
      BioMention.copyAttachments(entity, modifiedEntity)
      modifiedEntity.modifications += PTM(label, evidence = Some(se.trigger), site = siteOption, negated)
      modifiedEntity

    // dig into the controller
    case posEvent if (posEvent matches "ComplexEvent") && (!DarpaActions.hasNegativePolarity(posEvent)) =>
      // get the controller of the event (assume only one controller)
      val controller = posEvent.arguments("controller").head.toBioMention
      flattenController(controller, negated)

    // dig into the controller
    case negEvent if (negEvent matches "ComplexEvent") && DarpaActions.hasNegativePolarity(negEvent) =>
      // get the controller of the event (assume only one controller)
      val controller = negEvent.arguments("controller").head.toBioMention
      // negate the underlying PTM
      // if received event has negative polarity (see issue #184)
      flattenController(controller, negated=true)
  }
}
