package org.clulab.reach.focusedreading.reinforcement_learning.policies

import org.clulab.reach.focusedreading.reinforcement_learning.actions.Action

/**
  * Created by enrique on 31/03/17.
  */
class GreedyPolicy(override val values:Values) extends EpGreedyPolicy(0, values)
