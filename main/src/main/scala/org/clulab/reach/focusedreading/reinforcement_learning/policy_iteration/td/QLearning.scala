package org.clulab.reach.focusedreading.reinforcement_learning.policy_iteration.td

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.focusedreading.reinforcement_learning.Decays
import org.clulab.reach.focusedreading.reinforcement_learning.environment._
import org.clulab.reach.focusedreading.reinforcement_learning.policies._


/**
  * Created by enrique on 26/03/17.
  */
class QLearning(environmentFabric:() => Option[Environment], episodeBound:Int, burnInEpisodes:Int, alpha:Double = 0.01, gamma:Double = 0.8) extends LazyLogging {

  var stable = true
  var episodeCount = 0
  var controlCount = 0

  val alphaFloor = 0.00001

//  val alphaDecrease = alpha/episodeBound
//  val alphas = (0 to episodeBound).toStream.map{
//    i =>
//      val a = alpha-(i*alphaDecrease)
//      if(a > alphaFloor)
//        a
//      else
//        alphaFloor
//  }.iterator

  val alphas = Decays.exponentialDecay(alpha, alphaFloor, episodeBound, 500).iterator


  def iteratePolicy(policy:EpGreedyPolicy):Policy = {


    // Initialize the policy we will learn online
    //val policy = new EpGreedyPolicy(.05)

    var episode = environmentFabric()

    do {
      stable = true


      episode match {
        case Some(environment) =>

          val currentAlpha = alphas.next

          // Observe the initial state
          val possibleStates = environment.observeStates

          // Evaluate the policy
          val x = policy.selectAction(possibleStates, environment.possibleActions())
          var (currentState, currentAction) = x

          // Enter into the episode loop
          while(!environment.finishedEpisode){
            // Execute chosen action and observe reward
            val reward = environment.executePolicy(currentAction)

            // Observe the new state after executing the action
            val possibleNextStates = environment.observeStates
            val possibleNextActions = environment.possibleActions()

            val choices = (possibleNextStates zip possibleNextActions).sortBy{case (s, a) => policy.values(s, a)}.reverse

            // Chose a new action

            val (nextState, nextAction) = choices.head


            // Perform the update
            val actionValues = policy.values
            val changed = actionValues.tdUpdate((currentState, currentAction), (nextState, nextAction), reward, currentAlpha, gamma)

            // Keep track of the fluctuations of the values
            if(changed)
              stable = false


            // Update the state and action
            currentState = nextState
            currentAction = nextAction


            controlCount += 1
          }

          episodeCount += 1

          if(episodeCount % 10 == 0)
            logger.info(s"Episode $episodeCount")

        case None => Unit
      }

      episode = environmentFabric()


    }while(episode != None && (!stable || episodeCount <= burnInEpisodes) && episodeCount <= episodeBound)

    if(stable)
      logger.info(s"Converged on $episodeCount episodes")
    else
      logger.info(s"Didn't converge")

    policy
  }

}