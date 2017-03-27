package org.clulab.reach.focusedreading.reinforcement_learning.policy_iteration.td

import org.clulab.reach.focusedreading.{Connection, MostConnectedParticipantsStrategy, Participant}
import org.clulab.reach.focusedreading.agents.SQLiteSearchAgent
import org.clulab.reach.focusedreading.models.SearchModel
import org.clulab.reach.focusedreading.reinforcement_learning.{Actions, RankBin, State}
import org.clulab.reach.focusedreading.reinforcement_learning.policies._
import org.clulab.reach.focusedreading.ir._
import com.typesafe.scalalogging.LazyLogging
import org.clulab.utils.Serializer

import collection.mutable

/**
  * Created by enrique on 26/03/17.
  */
class SARSA(dataset:Seq[Seq[String]], alpha:Double = 0.5, gamma:Double = 1.0) extends LazyLogging {

  // For now we only care about the end points
  val trainingEpisodes = dataset map (e => (e.head, e.last))

  val tolerance = 0.001
  var stable = true
  val episodeBound = 1500000
  var episodeCount = 0


  def iteratePolicy(policy:EpGreedyPolicy):Policy = {


    // Initialize the policy we will learn online
    //val policy = new EpGreedyPolicy(.05)

    do {
      stable = true
      for(episode <- trainingEpisodes){

        val participantA = Participant("", episode._1)
        val participantB = Participant("", episode._2)
        val queryLog = new mutable.ArrayBuffer[(Participant, Participant)]
        val introductions = new mutable.HashMap[Participant, Int]

        var iterationNum = 0

        // Instantiate an agent for this episode
        val agent = new SQLiteSearchAgent(participantA, participantB)

        introductions += participantA -> 0
        introductions += participantB -> 0

        do{
          // Clear the iteration flag in the policy

          iterationNum += 1

          val (a, b) = agent.choseEndPoints(participantA, participantB, agent.triedPairs.toSet, agent.model)
          agent.triedPairs += Tuple2(a, b)

          queryLog += Tuple2(a, b)

          //val query = agent.choseQuery(a, b, agent.model)

          // Fetch the current state
          val currentState = fillState(agent.model, iterationNum, a, b, queryLog, introductions)

          // Compute the chosen action
          val action = policy.selectAction(currentState)


          // Build a query object based on the action
          val query = action match {
            case Actions.Conjunction => Query(QueryStrategy.Conjunction, a, Some(b))
            case Actions.Disjunction => Query(QueryStrategy.Disjunction, a, Some(b))
          }

          val paperIds = agent.informationRetrival(query)

          val findings = agent.informationExtraction(paperIds)

          // Count the introductions
          for(f <- findings){
            val x = f.controller
            val y = f.controlled

            if(!introductions.contains(x))
              introductions += (x -> iterationNum)

            if(!introductions.contains(y))
              introductions += (y -> iterationNum)
          }


          val modelGraphBefore = agent.getStateGraph
          agent.reconcile(findings)
          val modelGraphAfter = agent.getStateGraph

          val observedReward = agent.observeReward

          // Observe the next step
          val (c, d) = agent.choseEndPoints(participantA, participantB, agent.triedPairs.toSet, agent.model)
          val augmentedQueryLog = queryLog ++ Seq(Tuple2(c, d))
          val resultingStep = fillState(agent.model, iterationNum+1, c, d, augmentedQueryLog, introductions)

          // Had the epoc terminated?
          val terminated = agent.hasFinished(participantA, participantB, agent.model)

          // Se what's the next action selected by the policy
          val nextAction = policy.selectAction(resultingStep)

          // Do the policy improvement (update step)
          val qValue = policy.actionValues((currentState, action))

          // Make sure that the qValue of the next step is Zero if it is a terminating step
          val nextActionQValue = if(!terminated) policy.actionValues((resultingStep, nextAction)) else 0.0

          // Apply the Bellman equation for this value's update
          val newQValue = qValue + alpha * (observedReward+gamma*(nextActionQValue-qValue))

          policy.actionValues.set((currentState, action), newQValue)

          if(Math.abs(newQValue-qValue) > tolerance)
            stable = false


          episodeCount += 1

          if(episodeCount % 10 == 0)
            logger.info(s"Episode $episodeCount")

        }while(!agent.hasFinished(participantA, participantB, agent.model))
      }
    }while(!stable && episodeCount <= episodeBound)

    if(stable)
      logger.info(s"Converged on $episodeCount episodes")
    else
      logger.info(s"Didn't converge")

    policy
  }


  def fillState(model:SearchModel, iterationNum:Int, a:Participant, b:Participant, queryLog:Seq[(Participant, Participant)], introductions:mutable.Map[Participant, Int]):State = {

    val log = queryLog flatMap (l => Seq(l._1, l._2))
    val paQueryLogCount = log.count(p => p == a)
    val pbQueryLogCount = log.count(p => p == b)

    val compA = model.getConnectedComponentOf(a).get
    val compB = model.getConnectedComponentOf(b).get

    val sameComponent = compA == compB

    val paIntro = introductions(a)
    val pbIntro = introductions(b)

    val ranks:Map[Participant, Int] = model.rankedNodes

    val paRank = getRank(a, ranks)
    val pbRank = getRank(b, ranks)

    State(RankBin.First, RankBin.Bottom, iterationNum, paQueryLogCount,pbQueryLogCount,sameComponent,paIntro,pbIntro)
  }

  def getRank(p:Participant, ranks:Map[Participant, Int]):RankBin.Value = {
    val rank = ranks(p)
    if(rank == 0)
      RankBin.First
    else{
      val size = ranks.size
      if(size < 3)
        RankBin.Upper
      else{
        val stride = size/3
        val cutPoints = 1.to(3).map(i => i*stride).reverse

        var ret =RankBin.Bottom

        val bins = Seq(RankBin.Bottom, RankBin.Mid, RankBin.Upper)

        for((point, i) <- cutPoints.zipWithIndex){
          if(rank <= point)
            ret = bins(i)
        }

        ret
      }

    }
  }

}

object SARSA extends App {
  // The first argument is the input file
  val dataSet:Iterable[Seq[String]] = io.Source.fromFile(args(0)).getLines
    .map{
      s =>
        val t = s.split("\t").toSeq
        //(t(0), t(1), t(2))
        t
    }.toSeq

  val policyIteration = new SARSA(dataSet.toSeq)
  val initialPolicy = new EpGreedyPolicy(0.1)

  val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

  // Store the policy somewhere
  Serializer.save(learntPolicy, "learnt_policy.ser")
}
