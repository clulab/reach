package org.clulab.reach.darpa

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


  /**
    * Recursively unpack contents of mention so that all mentions are available at top level
    * @param mns a Set[[Mention]]
    * @return a Set[[Mention]]
    */
  def unpackMentions(mns: Set[Mention]): Set[Mention] = {

    var m2mns = Map[Mention, Set[Mention]]()

    // memoize results of unpacking
    def cache(m: Mention, seen: Set[Mention] = Set.empty[Mention]): Set[Mention] = m2mns.get(m) match {
      case Some(contents) => contents
      // update if unseen
      case None =>
        val innards = unpackMention(m, seen)
        m2mns = m2mns + (m -> innards)
        innards
    }

    def unpackMention(m: Mention, seen: Set[Mention] = Set.empty[Mention]): Set[Mention] = {
      val unpacked = for {
        a: Mention <- m.arguments.values.flatten
        if !seen.contains(a)
      } yield cache(a, seen + m + a)
      unpacked.flatten.toSet ++ seen
    }

    val unpackedMentions = for (m <- mns) yield unpackMention(m)
    unpackedMentions.flatten
  }
  def unpackMention(m: Mention): Set[Mention] = unpackMentions(Set(m))

  /**
    * Prepare mentions for output
    * 1. "Flatten" mentions (flatten nested controllers)
    * 2. Remove duplicates
    * 3. validate
    * @param mentions a Seq[[Mention]]
    * @return
    */
  def prepareForOutput(mentions: Seq[Mention]): Seq[CorefMention] = {
    val flattenedMentions = flattenMentions(mentions)
    // remove duplicates
    // Even though this has likely already been run on the mentions (via ReachSystem),
    // it unfortunately has to be run again after flattening as things that
    // didn't appear as duplicates previously now will
    // (consider an event with a nested controller before and after flattening).
    val deduplicated = MentionFilter.keepMostCompleteMentions(flattenedMentions)
    val prepared = validateMentions(deduplicated)
    prepared.map(_.toCorefMention).distinct
  }

  def validateMentions(mns: Seq[Mention]): Seq[Mention] = mns.filter(isValidMention)

  def isValidMention(m: Mention): Boolean = m.toCorefMention match {
    case generic if generic matches "^Generic".r => false
    case se if se matches "Event" => se.arguments.values.flatten.forall(isValidMention)
    case _ => true
  }

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
        case None => ce
        // a single controller
        case Some(Seq(controller)) =>
          val flattenedController = m match {
            // FIXME: Is this right?  Recursively pull the controllER instead of controllED??
            case reg if reg matches "Regulation" =>
              flattenController(reg, DarpaActions.hasNegativePolarity(m))
            // recursively pull the controlled of this controller
            case _ => flattenControlled(controller)
          }
          val updatedArguments = ce.arguments.updated("controller", Seq(flattenedController))
          // TODO: should controlled be flattened?
          val flattenedRepresentation = ce match {
            case rel: RelationMention => rel.copy(arguments = updatedArguments)
            case em: EventMention => em.copy(arguments = updatedArguments)
          }
          flattenedRepresentation.toBioMention
      }

    // Site, Cellular_component, CellLine, TissueType, Species, etc.
    case other => other
  }

  /** Recover controlled from a potentially nested structure by recursively converting <br>
    * an Event to an Entity with the appropriate modifications representing its state. <br>
    * SimpleEvent -> theme + PTM <br>
    * Binding -> Complex (treated as an Entity) <br>
    * ComplexEvent -> recursive call on controlled <br>
    */
  def flattenControlled(m: Mention, negated: Boolean = false): BioMention =
    DarpaActions.convertEventToEntity(m, asOutput = true, negated)

  /** Recover controller from a potentially nested structure by recursively converting <br>
    * an Event to an Entity with the appropriate modifications representing its state. <br>
    * SimpleEvent -> theme + PTM <br>
    * Binding -> Complex (treated as an Entity) <br>
    * ComplexEvent -> recursive call on controller <br>
    */
  def flattenController(m: Mention, negated: Boolean = false): BioMention =
    DarpaActions.convertEventToEntity(m, asOutput = false, negated)
}