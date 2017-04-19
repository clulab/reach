package org.clulab.reach.focusedreading.agents

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.focusedreading.ie.{REACHIEStrategy, SQLIteIEStrategy}
import org.clulab.reach.focusedreading.ir.QueryStrategy._
import org.clulab.reach.focusedreading.ir.{LuceneIRStrategy, Query, SQLIRStrategy}
import org.clulab.reach.focusedreading.models._
import org.clulab.reach.focusedreading.reinforcement_learning.actions._
import org.clulab.reach.focusedreading.reinforcement_learning.states._
import org.clulab.reach.focusedreading.reinforcement_learning.policies.Policy
import org.clulab.reach.focusedreading._
import org.clulab.reach.focusedreading.agents.FocusedReadingStage._

import scala.collection.mutable

/**
  * Created by enrique on 18/02/17.
  */
class LuceneReachSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with LuceneIRStrategy
  with REACHIEStrategy {


  //override val model:Model = Graph[Participant, LDiEdge](participantA, participantB) // Directed graph with the model.
  override val model:SearchModel = new GFSModel(participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, source, Some(destination))


}

class SQLiteSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with SQLIRStrategy
  with SQLIteIEStrategy {


//  override val model:Model = Graph[Participant, LDiEdge](participantA, participantB) // Directed graph with the model.
  override val model:SearchModel = new GFSModel(participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, source, Some(destination))

}

class SQLiteMultiPathSearchAgent(participantA:Participant, participantB:Participant) extends MultiplePathsAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with SQLIRStrategy
  with SQLIteIEStrategy {


  override val model:SearchModel = new GFSModel(participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, source, Some(destination))


}


class PolicySearchAgent(participantA:Participant, participantB:Participant, val policy:Policy) extends SimplePathAgent(participantA, participantB)
  with ExploreExploitParticipantsStrategy
  //with MostConnectedParticipantsStrategy
  with SQLIRStrategy
  with SQLIteIEStrategy {

  // Fields

  val actionCounters = new mutable.HashMap[String, Int]() ++ Map[String, Int](ExploitEndpoints().toString -> 0, ExploreEndpoints().toString -> 0, ExploreQuery().toString -> 0, ExploitQuery().toString -> 0)


  var stage:FocusedReadingStage.Value = FocusedReadingStage.EndPoints

  this.introductions += participantA -> 0
  this.introductions += participantB -> 0
  ////////////

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)],
                              model: SearchModel): (Participant, Participant) = {

    // Choose the endpoints with the policy
    val endpoints = super.choseEndPoints(source, destination, previouslyChosen, model)

    // Keep track of the chosen actions
    actionCounters(this.lastActionChosen.get.toString) += 1

    // Set the stage to query after choosing the endpoints
    stage = FocusedReadingStage.Query

    endpoints
  }

  override val model:SearchModel = new GFSModel(participantA, participantB) // Directed graph with the model.

  override def reconcile(connections: Iterable[Connection]): Unit = {
    // Count the introductions
    for(f <- connections){
      val x = f.controller
      val y = f.controlled


      if(!introductions.contains(x))
        introductions += (x -> iterationNum)

      if(!introductions.contains(y))
        introductions += (y -> iterationNum)


    }

    super.reconcile(connections)
  }


  override def choseQuery(a: Participant,
                          b: Participant,
                          model: SearchModel) = {

    queryLog += Tuple2(a, b)

    val possibleActions:Seq[Action] = Seq(ExploreQuery(), ExploitQuery())

    // Create state
    val state = this.observeState

    // Query the policy
    val (_, action) = policy.selectAction(state, possibleActions)

    // Keep track of the action selection
    actionCounters(action.toString) += 1

    // Set the process stage to endpoint
    stage = FocusedReadingStage.EndPoints

    queryActionToStrategy(action, a, b)
  }

  override def observeState:State = {
    //if(queryLog.length > 0)
      fillState(this.model, iterationNum, queryLog, introductions)
    //else{
    //  fillState(this.model, iterationNum, Seq((participantA, participantB)), introductions)
    //}

  }

  override def getIterationNum: Int = iterationNum

  // Auxiliary methods
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

  private def executePolicyQueryStage(action:Action, persist:Boolean):Double = {

    // Fetch the chosen participants (endpoints)
    val (a, b) = queryLog.last

    // Build a query object based on the action
    val query = queryActionToStrategy(action, a, b)

    val paperIds = this.informationRetrival(query)

    val findings = this.informationExtraction(paperIds)

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
    reconcile(findings)

    // Increment the iteration count
    iterationNum += 1

    // Set the stage to endpoint
    stage = FocusedReadingStage.EndPoints


    // Return the observed reward
    if(!this.hasFinished(participantA, participantB, model)){
      // If this episode hasn't finished
      -0.05
    }
    else{
      // If finished successfuly
      successStopCondition(participantA, participantB, model) match{
        case Some(p) => 1.0
        case None => -1.0
      }
    }
  }

  private def queryActionToStrategy(action: Action, a: Participant, b: Participant) = {
    action match {
      case _: ExploitQuery =>
        Query(Conjunction, a, Some(b))
      case _: ExploreQuery =>
        Query(Disjunction, a, Some(b))
      case _ =>
        throw new RuntimeException("Got an invalid action type for the query stage")
    }
  }

  private def executePolicyEndpointsStage(action:Action, persist:Boolean):Double = {
    if(persist)
      iterationNum += 1


    val selectedChooser = action match {
      case _:ExploitEndpoints => exploitChooser
      case _:ExploreEndpoints => exploreChooser
      case _ => throw new RuntimeException("Invalid action for the ENDPOINTS stage")
    }

    val (a, b) = selectedChooser.choseEndPoints(participantA, participantB, triedPairs.toSet, model)
    ////////


    if(persist){
      triedPairs += Tuple2(a, b)
      queryLog += Tuple2(a, b)
    }

    stage = FocusedReadingStage.Query

    0.0 //TODO: Tinker with this reward
  }


  // Public methods
  def executePolicy(action:Action, persist:Boolean = true):Double = (stage: @unchecked) match {
    case FocusedReadingStage.Query => executePolicyQueryStage(action, persist)
    case FocusedReadingStage.EndPoints => executePolicyEndpointsStage(action, persist)
  }

  def possibleActions(): Seq[Action] = (stage: @unchecked) match {
    case FocusedReadingStage.EndPoints => Seq(ExploitEndpoints(), ExploreEndpoints())
    case FocusedReadingStage.Query => Seq(ExploitQuery(), ExploreQuery())
  }
  /////////////////


}



