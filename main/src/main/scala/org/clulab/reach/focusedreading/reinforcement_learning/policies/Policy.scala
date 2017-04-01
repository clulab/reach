package org.clulab.reach.focusedreading.reinforcement_learning.policies

import org.clulab.reach.focusedreading.reinforcement_learning.Actions
import org.clulab.reach.focusedreading.reinforcement_learning.states.State

/**
  * Created by enrique on 26/03/17.
  */
trait Policy {
  def selectAction(s:State):Actions.Value

  // Save the policy as json
  def save(path:String)
}
