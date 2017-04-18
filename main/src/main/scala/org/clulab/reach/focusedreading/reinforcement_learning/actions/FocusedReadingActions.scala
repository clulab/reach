package org.clulab.reach.focusedreading.reinforcement_learning.actions

/**
  * Created by enrique on 17/04/17.
  */

sealed class FocusedReadingAction() extends Action

case class ExploreQuery() extends FocusedReadingAction{
  override def toString: String = "Explore"
}

case class ExploitQuery() extends FocusedReadingAction{
  override def toString: String = "Exploit"
}

case class ExploreEndpoints() extends FocusedReadingAction

case class ExploitEndpoints() extends FocusedReadingAction
