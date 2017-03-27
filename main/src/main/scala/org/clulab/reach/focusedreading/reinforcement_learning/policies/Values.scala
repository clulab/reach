package org.clulab.reach.focusedreading.reinforcement_learning.policies

import collection.mutable
import org.clulab.reach.focusedreading.reinforcement_learning.{State, Actions}

/**
  * Created by enrique on 26/03/17.
  */

object Values{
  type StateValues = LazyValues[State, Double]//mutable.HashMap[State, Double]
  type ActionValues = LazyValues[(State, Actions.Value), Double]//mutable.HashMap[(State, Actions.Value), Double]

//  def zeroStateValues:StateValues = {
//    val ret = new StateValues
//    ret ++= State.enumerate.map(s => (s -> 0.0))
//    ret
//  }
//
//  def uniformStateValues:StateValues = {
//    val ret = new StateValues
//    ret ++= State.enumerate.map(s => (s -> 1.0/State.cardinality))
//    ret
//  }
//
//  def zeroActionValues:ActionValues = {
//    val ret = new ActionValues
//    ret ++= State.enumerate.flatMap{
//      s =>
//        Actions.values.map{
//          a => ((s, a) -> 0.0)
//        }
//    }
//    ret
//  }
//
//  def uniformActionValues:ActionValues = {
//    val ret = new ActionValues
//    ret ++= State.enumerate.flatMap{
//      s =>
//        Actions.values.map{
//          a => ((s, a) -> 1.0/State.cardinality)
//        }
//    }
//    ret
//  }

  def zeroStateValues:StateValues = new StateValues(0.0)

  def uniformStateValues:StateValues = new StateValues(1.0/State.cardinality)

  def zeroActionValues:ActionValues = new ActionValues(0.0)

  def uniformActionValues:ActionValues = new ActionValues(1.0/State.cardinality)
}

class LazyValues[K, V](default:V) {
  val backEnd = new mutable.HashMap[K, V]

  def apply(key:K): V = {
    if(backEnd.contains(key))
      backEnd(key)
    else{
      backEnd += (key -> default)
      default
    }
  }

  def set(key:K, value:V) = {backEnd(key) = value}


}
