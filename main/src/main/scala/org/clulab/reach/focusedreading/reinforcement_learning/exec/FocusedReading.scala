package org.clulab.reach.focusedreading.reinforcement_learning.exec.focused_reading

import breeze.linalg.{DenseVector, linspace}
import breeze.plot.{Figure, plot}
import org.clulab.reach.focusedreading.Participant
import org.clulab.reach.focusedreading.reinforcement_learning.actions._
import org.clulab.reach.focusedreading.reinforcement_learning.environment.{Environment, SimplePathEnvironment}
import org.clulab.reach.focusedreading.reinforcement_learning.policies.{EpGreedyPolicy, LinearApproximationValues, TabularValues}
import org.clulab.reach.focusedreading.reinforcement_learning.policy_iteration.td.SARSA

import scala.collection.immutable.HashSet
import scala.collection.mutable

/**
  * Created by enrique on 31/03/17.
  */

object LinearSARSA extends App {

  // The first argument is the input file
  val dataSet:Iterator[Tuple2[String, String]] = Iterator.continually(io.Source.fromFile(args(0)).getLines
    .map{
      s =>
        val t = s.split("\t").toSeq
        //(t(0), t(1), t(2))
        (t.head, t.last)
    }
  ).flatten

  def focusedReadingFabric():Option[Environment] = {
    if(dataSet.hasNext){
      val episode = dataSet.next
      val participantA = Participant("", episode._1)
      val participantB = Participant("", episode._2)

      Some(new SimplePathEnvironment(participantA, participantB))
    }
    else
      None
  }

  private def plotCoefficients(title:String, steps:Int, names:Seq[String], memory:Array[DenseVector[Double]]) = {
    val f = Figure()
    val p = f.subplot(0)
    val x = linspace(0.0, steps.toDouble, steps)

    val num = names.size

    for (i <- 0 until num) {
      val history = DenseVector(memory.map {
        v =>
          if (v.length == 0)
            0.0
          else
            v(i)
      })

      p += plot(x, history, '-', null, names(i))
    }

    p.legend = true
    p.xlabel = "Update #"
    p.ylabel = "Coef Explore value"
    p.title = title

    f.saveas(s"plot_$title.png")
  }

  val numEpisodes = 2000

  val policyIteration = new SARSA(focusedReadingFabric, numEpisodes, 2000, 0.01)
  val possibleActions:Set[Action] = Set(ExploitQuery(), ExploreQuery(), ExploreEndpoints(), ExploitEndpoints())
  val qFunction = new LinearApproximationValues(possibleActions)

  // Decaying epsilon
  val epsilon = 0.3
  val epsilonDecrease = (epsilon-0.01)/numEpisodes
  val eps = (0 to numEpisodes).toStream.map(i => epsilon-(i*epsilonDecrease)).iterator ++ Stream.continually(0.01)
  ///////////////////
  val initialPolicy = new EpGreedyPolicy(eps, qFunction)

  val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

  // Store the policy somewhere
  // Serializer.save(learntPolicy, "learnt_policy.ser")
  learntPolicy.save("learnt_policy.json")

  val steps = policyIteration.controlCount
  val coefficients = qFunction.coefficients.toSeq
  for(co <- coefficients){
    val names = co._2.keySet.toSeq.sorted
    val title = co._1.toString
    val memory = qFunction.coefficientMemory(co._1).toArray
    plotCoefficients(title, steps, names, memory)
  }

}

