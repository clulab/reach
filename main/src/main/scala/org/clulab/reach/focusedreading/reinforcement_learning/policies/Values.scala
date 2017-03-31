package org.clulab.reach.focusedreading.reinforcement_learning.policies

import collection.mutable
import org.clulab.reach.focusedreading.reinforcement_learning.{State, Actions}

/**
  * Created by enrique on 26/03/17.
  */


trait Values[K]{
  def apply(key:K):Double
  def set(key:K, value:Double):Unit
  def tdUpdate(current:K, next:K, reward:Double, rate:Double, decay:Double):Boolean
}

class TabularValues[K](default:Double, tolerance:Double = 1e-3) extends Values[K] {
  val backEnd = new mutable.HashMap[K, Double]

  override def apply(key:K): Double = {
    if(backEnd.contains(key))
      backEnd(key)
    else{
      backEnd += (key -> default)
      default
    }
  }

  override def set(key:K, value:Double) = {backEnd(key) = value}


  override def tdUpdate(current:K, next:K, reward:Double, rate:Double, decay:Double) = {
    val value:Double = this(current)
    val newValue:Double = value + (rate*(reward + decay*this(next) - value))
    this.set(current, newValue)

    // Return whether the current changed above the requested tolerance
    if(Math.abs(newValue - value) > tolerance)
      true
    else
      false
  }

}
