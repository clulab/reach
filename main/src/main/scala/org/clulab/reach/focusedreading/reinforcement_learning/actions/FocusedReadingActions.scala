package org.clulab.reach.focusedreading.reinforcement_learning.actions

/**
  * Created by enrique on 17/04/17.
  */

sealed class FocusedReadingAction() extends Action

case class Explore() extends FocusedReadingAction{
  override def toString: String = "Explore"
}

case class Exploit() extends FocusedReadingAction{
  override def toString: String = "Exploit"
}
