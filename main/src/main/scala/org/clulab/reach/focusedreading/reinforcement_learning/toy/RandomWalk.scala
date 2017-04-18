package org.clulab.reach.focusedreading.reinforcement_learning.toy

import org.clulab.reach.focusedreading.reinforcement_learning.environment.Environment
import org.clulab.reach.focusedreading.reinforcement_learning.policies.{EpGreedyPolicy, LinearApproximationValues, TabularValues}
import org.clulab.reach.focusedreading.reinforcement_learning.policy_iteration.td.SARSA
import breeze.linalg._
import breeze.plot._

/**
  * Created by enrique on 31/03/17.
  */
//object RandomWalk extends App{
//
//  val numStates = 20
//  val generator = scala.util.Random
//  generator.setSeed(0)
//
//  def randomWalkFabric():Option[Environment] = {
//    val start = generator.nextInt(numStates-2)+2
//    Some(RandomWalkEnvironment(start, numStates))
//  }
//
//  val policyIterator = new SARSA(randomWalkFabric, 10000, 10)
//  val qFunction = new LinearApproximationValues
//  //val qFunction = new TabularValues(0.0)
//  val initialPolicy = new EpGreedyPolicy(0.5, qFunction)
//
//  println("Initial policy:")
//  for(i <- 1 until numStates){
//    val state = new RandomWalkState(i, numStates)
//    val chosenAction = initialPolicy.selectAction(state)
//    println(s"State $i: $chosenAction")
//  }
//
//  println()
//
//  val learntPolicy = policyIterator.iteratePolicy(initialPolicy)
//
//  learntPolicy.save("random_walk.json")
//
//  val greedy = learntPolicy.asInstanceOf[EpGreedyPolicy].makeGreedy
//
//  println("Iterated policy:")
//  for(i <- 1 until numStates){
//    val state = new RandomWalkState(i, numStates)
//    val chosenAction = greedy.selectAction(state)
//    println(s"State $i: $chosenAction")
//  }
//
////  // Print the results of the actions
////
////  val f = Figure()
////  val p = f.subplot(0)
////  val x = linspace(0.0, policyIterator.controlCount.toDouble, policyIterator.controlCount)
////
////  val num = qFunction.coefficients.size
////  val names = qFunction.coefficients.keySet.toSeq.sorted
////  for(i <- 0 until num) {
////    val history = DenseVector(qFunction.coefficientMemory.map(v => v(i)).toArray)
////    p += plot(x, history, '-', null, names(i))
////  }
////
////  p.xlabel = "Update #"
////  p.ylabel = "Coef value"
////
////  f.saveas("plot.png")
//
//
//}
