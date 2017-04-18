package org.clulab.reach.focusedreading.reinforcement_learning.toy

import org.clulab.reach.focusedreading.reinforcement_learning.actions.{Action, ExploitQuery, ExploreQuery}
import org.clulab.reach.focusedreading.reinforcement_learning.environment.Environment
import org.clulab.reach.focusedreading.reinforcement_learning.states.State

/**
  * Created by enrique on 31/03/17.
  */
//case class RandomWalkEnvironment(startingState:Int, val numStates:Int) extends Environment{
//
//  // The finishing state is the furthest to the right
//
//  private var currentState = startingState
//  private var stepNum = 0
//
//  assert(startingState > 1 && startingState < numStates, "Starting state shouldn't be in the boundary")
//
//  private def reward(state:Int) = if(state == numStates) 1.0 else -0.01
//
//  override def executePolicy(action: Action, persist: Boolean = true): Double = {
//
//    // Move to wherever the action says
//    action match {
//      case _:ExploitQuery =>
//        // Left
//        if(currentState > 0)
//          currentState -= 1
//      case _:ExploreQuery =>
//        // Right
//        if(currentState < 5)
//          currentState += 1
//    }
//
//    stepNum += 1
//    reward(currentState)
//  }
//
//  override def observeState: State = new RandomWalkState(currentState, numStates)
//
//  override def finishedEpisode: Boolean = stepNum > 2*numStates ||  currentState == numStates
//}
