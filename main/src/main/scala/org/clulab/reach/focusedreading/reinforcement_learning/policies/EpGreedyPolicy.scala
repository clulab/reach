package org.clulab.reach.focusedreading.reinforcement_learning.policies

import java.io.{BufferedWriter, FileWriter}

import breeze.linalg._
import breeze.stats.distributions.Multinomial
import org.clulab.reach.focusedreading.reinforcement_learning.actions.Action
import org.clulab.reach.focusedreading.reinforcement_learning.states.State
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

/**
  * Created by enrique on 26/03/17.
  */

class EpGreedyPolicy(epsilon:Double, val values:Values, override val actionSet:Set[Action]) extends Policy(actionSet) {

  assert(epsilon <= 1 && epsilon >= 0, s"Invalid Epsilon value: $epsilon")

  override def selectAction(s: State):Action= {

    // Is there a more idiomatic way to do this in scala?
    val numActions = actionSet.size

    val slice = epsilon / numActions
    val greedyProb = 1 - epsilon + slice

    val possibleActions:Seq[(State, Action)] = actionSet.map(a => (s, a)).toSeq.reverse // TODO: The order of this matters! Figure out why
    val possibleActionValues = possibleActions map (k => values(k))
    val sortedActions = possibleActions.zip(possibleActionValues).sortBy{case(sa, v) => v}.map(_._1._2).reverse
    val probs = greedyProb::List.fill(numActions-1)(slice)

    // Do a random sample from a multinomial distribution using probs as parameter
    val dist = Multinomial(DenseVector(probs.toArray))

    val choiceIx = dist.sample
    val choice = sortedActions(choiceIx)

    // Return the random sample
    choice
  }

  override def save(path:String): Unit ={
    val ast = {
      ("type" -> "ep_greedy") ~
      ("epsilon" -> epsilon) ~
        ("values" -> values.toJson)
    }

    val json = pretty(render(ast))

    val bfw = new BufferedWriter(new FileWriter(path))
    bfw.write(json)
    bfw.close
  }

  def makeGreedy:GreedyPolicy = new GreedyPolicy(values, actionSet)
}
