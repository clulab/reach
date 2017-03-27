package org.clulab.reach.focusedreading.reinforcement_learning

/**
  * Created by enrique on 26/03/17.
  */
object Actions extends Enumeration{
  val Conjunction, Disjunction = Value

  val cardinality:Int = Actions.values.size
}
