package org.clulab.assembly.representations

import org.clulab.assembly.PrecedenceRelation

/**
 * Trait for an Event representation of a Mention.
 */
trait Event extends EntityEventRepresentation {

  override val eerString = "assembly.Event"

  /** PrecedenceRelations for this Event */
  def precedenceRelations: Set[PrecedenceRelation] = {
    manager.getPrecedenceRelations(equivalenceHash)
  }

  /** Causal predecessors of this Event */
  def predecessors: Set[EntityEventRepresentation] =
    manager.predecessorsOf(equivalenceHash).map(_.asInstanceOf[Event])

  /** Distinct causal predecessors of this Event */
  def distinctPredecessors: Set[EntityEventRepresentation] =
    manager.distinctPredecessorsOf(equivalenceHash).map(_.asInstanceOf[Event])

  /** Causal successors of this Event */
  def successors: Set[EntityEventRepresentation] =
    manager.successorsOf(equivalenceHash).map(_.asInstanceOf[Event])

  /** Distinct causal successors of this Event */
  def distinctSuccessors: Set[EntityEventRepresentation] =
    manager.distinctSuccessorsOf(equivalenceHash).map(_.asInstanceOf[Event])

  /** Get the entities (patients) serving as input to the event */
  def I: Set[Entity]

  /** Get the entities (transformed patients) serving as output to the event */
  def O: Set[Entity]

  /** Checks if argument (including its mods) is contained in the event **/
  def hasExactArgument(arg: EntityEventRepresentation): Boolean

  /** Checks if SimpleEntity argument is contained in the event. <br>
    * Only requires grounding id to match.
    */
  def hasApproximateArgument(arg: SimpleEntity): Boolean
}
