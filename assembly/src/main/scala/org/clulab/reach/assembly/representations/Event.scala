package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly.PrecedenceRelation


/**
 * Trait for an Event representation of a Mention.
 */
trait Event extends EntityEventRepresentation {

  override val eerString = "assembly.Event"

  /** PrecedenceRelations for this Event */
  def precedenceRelations: Set[PrecedenceRelation] = {
    manager.getPrecedenceRelationsFor(this)
  }

  /** Causal predecessors of this Event */
  def predecessors: Set[EntityEventRepresentation] =
    manager.predecessorsOf(this).map(_.asInstanceOf[Event])

  /** Distinct causal predecessors of this Event */
  def distinctPredecessors: Set[EntityEventRepresentation] =
    manager.distinctPredecessorsOf(this).map(_.asInstanceOf[Event])

  /** Equivalent causal predecessors of this Event */
  def equivalentPredecessors(ignoreMods: Boolean): Set[EntityEventRepresentation] = for {
    p <- predecessors
    e <- manager.getEquivalentEERs(p, ignoreMods)
  } yield e

  /** Causal successors of this Event */
  def successors: Set[EntityEventRepresentation] =
    manager.successorsOf(this).map(_.asInstanceOf[Event])

  /** Distinct causal successors of this Event */
  def distinctSuccessors: Set[EntityEventRepresentation] =
    manager.distinctSuccessorsOf(this).map(_.asInstanceOf[Event])

  /** Equivalent causal successors of this Event */
  def equivalentSuccessors(ignoreMods: Boolean): Set[EntityEventRepresentation] = for {
    s <- successors
    e <- manager.getEquivalentEERs(s, ignoreMods)
  } yield e

  /** Get the entities (patients) serving as input to the event */
  def I: Set[Entity]

  /** Get the entities (transformed patients) serving as output to the event */
  def O: Set[Entity]

  /** Checks if argument (including its mods) is contained in the event **/
  def hasArgument(arg: EntityEventRepresentation, ignoreMods: Boolean): Boolean

  /** Checks if argument (including its mods) is contained in the event **/
  def hasExactArgument(arg: EntityEventRepresentation): Boolean

  /** Checks if SimpleEntity argument is contained in the event. <br>
    * Only requires grounding id to match.
    */
  def hasApproximateArgument(arg: SimpleEntity): Boolean
}
