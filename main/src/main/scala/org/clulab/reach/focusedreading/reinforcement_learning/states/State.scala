package org.clulab.reach.focusedreading.reinforcement_learning.states

/**
  * Created by enrique on 26/03/17.
  */


trait State {
  def toFeatures:Map[String, Double]
}

case class DummyState() extends State{
  override def toFeatures: Map[String, Double] = Map()
}