package org.clulab.reach.focusedreading.reinforcement_learning.environment

import org.clulab.reach.focusedreading.reinforcement_learning.actions.Actions
import org.clulab.reach.focusedreading.reinforcement_learning.states.State

/**
  * Created by enrique on 31/03/17.
  */
trait Environment {
  def executePolicy(action:Actions.Value, persist:Boolean = true):Double
  def observeState:State
  def finishedEpisode:Boolean
}
