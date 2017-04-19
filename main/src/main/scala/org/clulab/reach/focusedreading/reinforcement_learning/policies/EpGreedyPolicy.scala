package org.clulab.reach.focusedreading.reinforcement_learning.policies

import java.io.{BufferedWriter, FileWriter}

import breeze.linalg._
import breeze.stats.distributions.{Multinomial, Uniform}
import org.clulab.reach.focusedreading.reinforcement_learning.actions.Action
import org.clulab.reach.focusedreading.reinforcement_learning.states.State
import org.clulab.reach.focusedreading.reinforcement_learning.randGen
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

/**
  * Created by enrique on 26/03/17.
  */

class EpGreedyPolicy(epsilons:Iterator[Double], val values:Values) extends Policy {


  def this(epsilon:Double, values:Values){
    this(Stream.continually[Double](epsilon).iterator, values)
  }

  var firstEpsilon:Option[Double] = None


  override def selectAction(ss:Seq[State], possibleActions:Seq[Action]):(State, Action) = {

    val epsilon = epsilons.next()

    if(firstEpsilon == None)
      firstEpsilon = Some(epsilon)

    assert(epsilon <= 1 && epsilon >= 0, s"Invalid Epsilon value: $epsilon")

    // Is there a more idiomatic way to do this in scala?
    val numActions = possibleActions.size

    val slice = epsilon / numActions
    val greedyProb = 1 - epsilon + slice

    val stateActions:Seq[(State, Action)] = ss zip possibleActions  //TODO: The order of this matters! Figure out why
    val stateActionValues = stateActions map (k => values(k))
    val sortedActions = stateActions.zip(stateActionValues).sortBy{case(sa, v) => v}.map(_._1._2).reverse
    val probs = greedyProb::List.fill(numActions-1)(slice)

    // Do a random sample from a multinomial distribution using probs as parameter
    implicit val rand = randGen // This sets the random number generator for the
    val dist = new Multinomial(DenseVector(probs.toArray))


    val choiceIx = dist.sample
    val choice = sortedActions(choiceIx)

    val originalIndex = possibleActions.indexOf(choice)
    val usedState = ss(originalIndex)

    // Return the random sample
    (usedState, choice)
  }

  override def save(path:String): Unit ={
    val ast = {
      ("type" -> "ep_greedy") ~
      ("epsilon" -> firstEpsilon.getOrElse(0.0)) ~
        ("values" -> values.toJson)
    }

    val json = pretty(render(ast))

    val bfw = new BufferedWriter(new FileWriter(path))
    bfw.write(json)
    bfw.close
  }

  def makeGreedy:GreedyPolicy = new GreedyPolicy(values)
}
