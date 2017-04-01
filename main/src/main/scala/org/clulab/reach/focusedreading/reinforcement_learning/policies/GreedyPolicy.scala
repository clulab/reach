package org.clulab.reach.focusedreading.reinforcement_learning.policies

/**
  * Created by enrique on 31/03/17.
  */
class GreedyPolicy(override val values:Values) extends EpGreedyPolicy(0, values)
