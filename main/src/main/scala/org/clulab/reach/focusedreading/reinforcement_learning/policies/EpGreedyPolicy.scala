package org.clulab.reach.focusedreading.reinforcement_learning.policies

import breeze.linalg._
import breeze.stats.distributions.Multinomial
import org.clulab.reach.focusedreading.reinforcement_learning.{Actions, State}

/**
  * Created by enrique on 26/03/17.
  */
class EpGreedyPolicy(epsilon:Double, val values:Values[(State, Actions.Value)]) extends Policy {

  assert(epsilon <= 1 && epsilon >= 0, s"Invalid Epsilon value: $epsilon")

  override def selectAction(s: State):Actions.Value = {
    val numActions = Actions.values.size
    val slice = epsilon / numActions
    val greedyProb = 1 - epsilon + slice

    val possibleActions:Seq[(State, Actions.Value)] = Actions.values.toSeq.map(a => (s, a))
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
}
