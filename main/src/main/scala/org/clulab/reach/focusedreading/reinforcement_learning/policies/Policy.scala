package org.clulab.reach.focusedreading.reinforcement_learning.policies

import org.clulab.reach.focusedreading.reinforcement_learning.{State, Actions}

/**
  * Created by enrique on 26/03/17.
  */
trait Policy {
  def selectAction(s:State):Actions.Value
}
