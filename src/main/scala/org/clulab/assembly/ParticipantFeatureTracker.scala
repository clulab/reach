package org.clulab.assembly

import org.clulab.assembly.representations._
import org.clulab.odin.Mention


/**
  * Storage class for an event's participant (+ the input features for the participant)
  * @param role the participant's role (theme, controlled, etc.)
  * @param mention the mention associated with the participant
  * @param features Set[[PTM]] defining the state of the participant upon event input (pulled from causal predecessors)
  */
case class RoleWithFeatures(role: String, mention: Mention, features: Set[representations.PTM])

class ParticipantFeatureTracker(am: AssemblyManager) {

  private val manager = am

  /**
    * Get output features (Set of PTMs) for some EER and its causal predecessors. <br>
    * The output entities of causal predecessors must match the gid in order for their PTMs to be retrieved.
    * @param gid A [[GroundingID]] that the eer must match
    * @param eer an [[EntityEventRepresentation]]
    * @return Set[[PTM]] defining the state of the participant upon event output
    */
  def getOutputFeatures(gid: GroundingID, eer: EER): Set[representations.PTM] = eer match {
    case entity: SimpleEntity if entity.grounding == gid => entity.getPTMs
    case diffEntity: SimpleEntity if diffEntity.grounding != gid => Set.empty[PTM]
    case complex: Complex => complex.members.flatMap(e => getOutputFeatures(gid, e))
    case event: SimpleEvent => event.O.flatMap(entity => getOutputFeatures(gid, entity))
  }

  /**
    * Recursively collect the causal predecessors of an [[EER]]
    * @param eer an [[EER]]
    * @return the set of causal predecessors (and their causal predecessors, etc.)
    */
  private def getPredecessors(eer: EER): Set[EER] = eer match {
    case noPredecessors: EER if manager.distinctPredecessorsOf(noPredecessors).isEmpty => Set.empty[EER]
    case hasPredecessors: EER =>
      val predecessors: Set[EER] = manager.distinctPredecessorsOf(hasPredecessors)
      predecessors ++ predecessors.flatMap(getPredecessors)
  }

  private def getPredecessors(m: Mention): Set[EER] = getPredecessors(manager.getEER(m))

  final def getInputFeatures(m: Mention, parent: Mention): Set[representations.PTM] = m match {
    // we only retrieve PTMs from an entity that is not a complex
    case entity if entity matches "Entity" =>
      // can only retrieve features on an entity that is not a complex
      // TODO: should this be changed to get or create?
      // TODO: Check that an SimpleEntity with a PTM would be equivalent to a SimpleEvent without a cause
      manager.getEER(m) match {
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
