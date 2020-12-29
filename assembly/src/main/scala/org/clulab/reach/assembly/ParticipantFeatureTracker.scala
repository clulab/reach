package org.clulab.reach.assembly

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.assembly.representations._
import org.clulab.odin.Mention


/**
  * Storage class for an event's participant (+ the input features for the participant)
 *
  * @param role the participant's role (theme, controlled, etc.)
  * @param participant the [[Mention]] associated with the participant
  * @param parent the [[Mention]] (an event) serving as the participant's parent
  * @param features Set[[PTM]] defining the state of the participant upon event input (pulled from causal predecessors)
  */
case class RoleWithFeatures(role: String, participant: Mention, parent: Mention, features: Set[representations.PTM])

class ParticipantFeatureTracker(am: AssemblyManager) extends LazyLogging {

  private val manager = am

  /**
    * Get output features (Set of PTMs) for some EER and its causal predecessors. <br>
    * The output entities of causal predecessors must match the gid in order for their PTMs to be retrieved.
 *
    * @param gid A [[GroundingID]] that the eer must match
    * @param eer an [[EntityEventRepresentation]]
    * @return Set[[PTM]] defining the state of the participant upon event output
    */
  def getOutputFeatures(gid: GroundingID, eer: EER): Set[representations.PTM] = eer match {
    case entity: SimpleEntity if entity.grounding == gid => entity.getPTMs
    case diffEntity: SimpleEntity if diffEntity.grounding != gid => Set.empty[PTM]
    case complex: Complex => complex.members.flatMap(e => getOutputFeatures(gid, e))
    case event: Event => event.O.flatMap(entity => getOutputFeatures(gid, entity))
  }

  // Speed up calculation of precedence-linked input features via memoization
  var predecessorsCache = Map[EER, Set[EER]]()

  private def cache(eer: EER, seen: Set[EER]): Set[EER] = {
    predecessorsCache.get(eer) match {
      case Some(predecessors) =>
        //logger.debug(s"hit predecessorsCache for $eer")
        predecessors
      // update if unseen
      case None =>
        val predecessors = getPredecessors(eer, seen)
        predecessorsCache = predecessorsCache + (eer -> predecessors)
        predecessors
    }
  }

  /**
    * Recursively collect the causal predecessors of an [[EER]], <br>
    * keeping track of <br> which EERs have been seen (to avoid loops).
 *
    * @param eer an [[EER]]
    * @return the set of causal predecessors (and their causal predecessors, etc.)
    */
  private def getPredecessors(eer: EER, seen: Set[EER] = Set.empty[EER]): Set[EER] = {
    val unseenPredecessors = for {
      p <- manager.distinctPredecessorsOf(eer)
      if !(seen contains p)
    } yield cache(p, seen ++ Set(eer))
    unseenPredecessors.flatten ++ seen
  }

  private def getPredecessors(m: Mention): Set[EER] = {
    val eer = manager.getEER(m)
    getPredecessors(eer) - eer
  }

  final def getInputFeatures(m: Mention, parent: Mention): Set[representations.PTM] = m match {
    // we only retrieve PTMs from an entity that is not a complex
    case entity if entity matches "Entity" =>
      // can only retrieve features on an entity that is not a complex
      // TODO: should this be changed to get or create?
      // TODO: Check that an SimpleEntity with a PTM would be equivalent to a SimpleEvent without a cause
      manager.getEER(entity) match {
        case ent: SimpleEntity =>
          // Check for predecessors of parent
          val predecessors = getPredecessors(parent)
          // the Entity's PTMs + those of its relevant predecessors
          ent.getPTMs ++ predecessors.flatMap(p => getOutputFeatures(ent.grounding, p))
        // if this is a complex, we don't know which member to look at...
        case _ => Set.empty[representations.PTM]
      }
    case _ => Set.empty[representations.PTM]
//    // get the output entity of this event
//    case event if event matches "Event" =>
//      val outputEntity: Mention = DarpaActions.convertEventToEntity(m, asOutput = true, negated = DarpaActions.hasNegativePolarity(m))
//      getInputFeatures(outputEntity, parent)
//    // TODO: should this be changed to lump together all the features of each complex member?
//    case complex if complex matches "Complex" =>
//      Set.empty[representations.PTM]
  }
}
