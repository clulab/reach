package org.clulab.reach.focusedreading.reinforcement_learning.environment

import org.clulab.reach.focusedreading.Participant
import org.clulab.reach.focusedreading.agents.{PolicySearchAgent, SQLiteSearchAgent}
import org.clulab.reach.focusedreading.reinforcement_learning.actions._
import org.clulab.reach.focusedreading.reinforcement_learning.policies.DummyPolicy
import org.clulab.reach.focusedreading.reinforcement_learning.states.State


/**
  * Created by enrique on 30/03/17.
  */
class SimplePathEnvironment(participantA:Participant, participantB:Participant) extends Environment {

  val agent = new PolicySearchAgent(participantA, participantB, DummyPolicy())


  override def possibleActions(): Seq[Action] = agent.possibleActions()

  override def executePolicy(action: Action, persist: Boolean): Double = agent.executePolicy(action, persist)

  override def observeState: State = agent.observeState

  override def observeStates: Seq[State] = {
    (agent.stage: @unchecked) match {
      case agent.QUERY =>
        val state = agent.observeState
        Seq(state, state)
      case agent.ENDPOINT =>
        val exploreState = agent.observeExploreState(participantA, participantB, agent.triedPairs.toSet, agent.model)._2
        val exploitState = agent.observeExploitState(participantA, participantB, agent.triedPairs.toSet, agent.model)._2

        val actions = possibleActions()

        actions.map{
          case _:ExploreEndpoints =>
            exploreState
          case _:ExploitEndpoints =>
            exploitState
        }
    }
  }

  override def finishedEpisode:Boolean = agent.stage == agent.ENDPOINT && agent.hasFinished(participantA, participantB, agent.model)

}
