package org.clulab.reach.focusedreading.reinforcement_learning.environment

import org.clulab.reach.focusedreading.{MostConnectedAndRecentParticipantsStrategy, MostConnectedParticipantsStrategy, Participant}
import org.clulab.reach.focusedreading.agents.SQLiteSearchAgent
import org.clulab.reach.focusedreading.ir.{Query, QueryStrategy}
import org.clulab.reach.focusedreading.models.{GFSModel, SearchModel}
import org.clulab.reach.focusedreading.reinforcement_learning.actions.{Action, Exploit, Explore}
import org.clulab.reach.focusedreading.reinforcement_learning.policies.Policy
import org.clulab.reach.focusedreading.reinforcement_learning.states.{FocusedReadingState, RankBin, State}

import scala.collection.mutable

/**
  * Created by enrique on 30/03/17.
  */
class SimplePathEnvironment(participantA:Participant, participantB:Participant) extends Environment {

  val agent = new SQLiteSearchAgent(participantA, participantB)
  val queryLog = new mutable.ArrayBuffer[(Participant, Participant)]
  val introductions = new mutable.HashMap[Participant, Int]

  introductions += participantA -> 0
  introductions += participantB -> 0

  var iterationNum = 0

  override def executePolicy(action:Action, persist:Boolean = true):Double = {

    if(persist)
      iterationNum += 1

    // UNCOMMENT for deterministic endpoint choosing
    val (a, b) = agent.choseEndPoints(participantA, participantB, agent.triedPairs.toSet, agent.model)
    ///////

    // UNCOMMENT for policy selected endpoint choosing
//    val exploitChooser = new {} with MostConnectedAndRecentParticipantsStrategy {
//      override val participantIntroductions = introductions
//    }
//    val exploreChooser = new {} with MostConnectedParticipantsStrategy {}
//
//    val selectedChooser = action match {
//      case Actions.Conjunction =>
//        exploitChooser
//      case Actions.Disjunction =>
//        exploreChooser
//    }
//
//    val (a, b) = selectedChooser.choseEndPoints(participantA, participantB, agent.triedPairs.toSet, agent.model)
    ////////

    if(persist){
      agent.triedPairs += Tuple2(a, b)
      queryLog += Tuple2(a, b)
    }

    // Fetch the current state
    //val currentState = fillState(agent.model, iterationNum, a, b, queryLog, introductions)

    // Build a query object based on the action
    val query = action match {
      case _:Exploit =>
        Query(QueryStrategy.Conjunction, a, Some(b))
      case _:Explore =>
        Query(QueryStrategy.Disjunction, a, Some(b))
    }

    val paperIds = agent.informationRetrival(query)

    val findings = agent.informationExtraction(paperIds)

    // Count the introductions
    for(f <- findings){
      val x = f.controller
      val y = f.controlled

      if(persist){
        if(!introductions.contains(x))
          introductions += (x -> iterationNum)

        if(!introductions.contains(y))
          introductions += (y -> iterationNum)
      }

    }

    // Add the stuff to the model
    agent.reconcile(findings)

    // Increment the iteration count
    agent.iterationNum += 1

    // Return the observed reward
    if(!agent.hasFinished(participantA, participantB, agent.model)){
      // If this episode hasn't finished
      -0.05
    }
    else{
      // If finished successfuly
      agent.successStopCondition(participantA, participantB, agent.model) match{
        case Some(p) => 1.0
        case None => -1.0
      }
    }
  }

  private def fillState(model:SearchModel, iterationNum:Int, queryLog:Seq[(Participant, Participant)], introductions:mutable.Map[Participant, Int]):State = {

      val (a, b) = queryLog.last
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

      FocusedReadingState(RankBin.First, RankBin.Bottom, iterationNum, paQueryLogCount,pbQueryLogCount,sameComponent,paIntro,pbIntro)
  }

  override def observeState:State = {
    if(queryLog.length > 0)
      fillState(agent.model, iterationNum, queryLog, introductions)
    else{
      fillState(agent.model, iterationNum, Seq((participantA, participantB)), introductions)
    }

  }

  private def getRank(p:Participant, ranks:Map[Participant, Int]):RankBin.Value = {
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

  override def finishedEpisode:Boolean = agent.hasFinished(participantA, participantB, agent.model)

}
