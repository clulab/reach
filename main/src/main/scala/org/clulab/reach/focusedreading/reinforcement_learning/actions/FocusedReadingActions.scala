package org.clulab.reach.focusedreading.reinforcement_learning.actions

/**
  * Created by enrique on 17/04/17.
  */

sealed class FocusedReadingAction() extends Action

case class ExploreQuery() extends FocusedReadingAction

case class ExploitQuery() extends FocusedReadingAction

case class ExploreEndpoints() extends FocusedReadingAction

case class ExploitEndpoints() extends FocusedReadingAction
