package org.clulab.reach.focusedreading.reinforcement_learning.exec.focused_reading

import org.clulab.reach.focusedreading.Participant
import org.clulab.reach.focusedreading.reinforcement_learning.environment.{Environment, SimplePathEnvironment}
import org.clulab.reach.focusedreading.reinforcement_learning.policies.{EpGreedyPolicy, LinearApproximationValues, TabularValues}
import org.clulab.reach.focusedreading.reinforcement_learning.policy_iteration.td.SARSA

/**
  * Created by enrique on 31/03/17.
  */

object TabularSARSA extends App {
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

  val policyIteration = new SARSA(focusedReadingFabric, 10000, 30)
  val qFunction = new TabularValues(0)
  val initialPolicy = new EpGreedyPolicy(0.1, qFunction)

  val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

  // Store the policy somewhere
  // Serializer.save(learntPolicy, "learnt_policy.ser")
}

object LinearSARSA extends App {
  // The first argument is the input file
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

  val policyIteration = new SARSA(focusedReadingFabric, 10000, 30)
  val qFunction = new LinearApproximationValues
  val initialPolicy = new EpGreedyPolicy(0.1, qFunction)

  val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

  // Store the policy somewhere
  // Serializer.save(learntPolicy, "learnt_policy.ser")
  learntPolicy.save("learnt_policy.json")
}

