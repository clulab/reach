package org.clulab.reach.focusedreading.reinforcement_learning.policy_iteration.td

import org.clulab.reach.focusedreading.{Connection, MostConnectedParticipantsStrategy, Participant}
import org.clulab.reach.focusedreading.agents.SQLiteSearchAgent
import org.clulab.reach.focusedreading.models.SearchModel
import org.clulab.reach.focusedreading.reinforcement_learning.{Actions, RankBin, State}
import org.clulab.reach.focusedreading.reinforcement_learning.policies._
import org.clulab.reach.focusedreading.ir._
import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import org.clulab.utils.Serializer

import collection.mutable

/**
  * Created by enrique on 26/03/17.
  */
class SARSA(dataset:Seq[Seq[String]], alpha:Double = 0.5, gamma:Double = 1.0) extends LazyLogging {

  // For now we only care about the end points
  val trainingEpisodes = dataset map (e => (e.head, e.last))

  var stable = true
  val episodeBound = 150
  var episodeCount = 0


  def iteratePolicy(policy:EpGreedyPolicy):Policy = {


    // Initialize the policy we will learn online
    //val policy = new EpGreedyPolicy(.05)

    do {
      stable = true
      for(episode <- trainingEpisodes){

        if(episodeCount <= episodeBound){
          // Instantiate an environment for this episode
          val participantA = Participant("", episode._1)
          val participantB = Participant("", episode._2)

          val environment = new SimplePathEnvironment(participantA, participantB)

          // Observe the initial state
          var currentState = environment.observeState

          // Evaluate the policy
          var currentAction = policy.selectAction(currentState)

          // Enter into the episode loop
          while(!environment.finishedEpisode){
            // Execute chosen action and observe reward
            val reward = environment.executePolicy(currentAction)

            // Observe the new state after executing the action
            val nextState = environment.observeState

            // Chose a new action
            val nextAction = policy.selectAction(nextState)


            // Perform the update
            val actionValues = policy.values
            val changed = actionValues.tdUpdate((currentState, currentAction), (nextState, nextAction), reward, alpha, gamma)

            // Keep track of the fluctuations of the values
            if(changed)
              stable = false


            // Update the state and action
            currentState = nextState
            currentAction = nextAction

          }

          episodeCount += 1

          if(episodeCount % 10 == 0)
            logger.info(s"Episode $episodeCount")
        }


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
  val qFunction = new TabularValues[(State, Actions.Value)](0)
  val initialPolicy = new EpGreedyPolicy(0.1, qFunction)

  val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

  // Store the policy somewhere
  // Serializer.save(learntPolicy, "learnt_policy.ser")
}
