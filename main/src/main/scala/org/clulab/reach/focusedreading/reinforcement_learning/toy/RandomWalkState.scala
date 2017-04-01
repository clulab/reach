package org.clulab.reach.focusedreading.reinforcement_learning.toy

import org.clulab.reach.focusedreading.reinforcement_learning.states.State

/**
  * Created by enrique on 31/03/17.
  */
class RandomWalkState(val currentState:Int, numStates:Int) extends State {

  override def toFeatures: Map[String, Double] = {
    val states = 1 until numStates

    states.map{
      s =>
        val atState = if(currentState == s) 1.0 else 0.0
        (s"state_$s" -> atState)
    }.toMap
  }

  override def hashCode(): Int = currentState
}
