package org.clulab.reach.focusedreading.reinforcement_learning

/**
  * Created by enrique on 26/03/17.
  */
object Actions extends Enumeration{
  val Disjunction,Conjunction = Value

  val cardinality:Int = Actions.values.size

  def toFeatures(action:Value):Map[String, Double] = {
    action match {
      case Actions.Conjunction => Map("action_conjunction" -> 1.0)
      case Actions.Disjunction => Map("action_conjunction" -> 0.0)
    }
  }
}
