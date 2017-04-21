package org.clulab.reach.focusedreading

import collection.mutable
import org.clulab.reach.focusedreading.models._
import org.clulab.reach.focusedreading.reinforcement_learning.actions._
import org.clulab.reach.focusedreading.reinforcement_learning.policies.Policy
import org.clulab.reach.focusedreading.reinforcement_learning.states.{FocusedReadingState, State}

import scala.annotation.tailrec

/**
  * Created by enrique on 19/02/17.
  */
trait ParticipantChoosingStrategy {
  def choseEndPoints(source:Participant,
                     destination:Participant,
                     previouslyChosen:Set[(Participant, Participant)],
                     model:SearchModel):(Participant, Participant)

  /**
    * Checks wether there's a change from the latest endpoints
    */
  def differentEndpoints(a:(Participant, Participant), previouslyChosen:Set[(Participant, Participant)]) = !previouslyChosen.contains(a)

  /**
    * Picks two enpoints from the stacks
    * @param sA
    * @param sB
    * @return
    */
  def pickEndpoints(sA:mutable.Stack[Participant], sB:mutable.Stack[Participant]):(Participant, Participant) = {

    val (left, right) = if(sA.size <= sB.size) (sA, sB) else (sB, sA)
    val a = left.pop()
    var b = right.pop()

    var stop = false
    while(!stop){
      if(right.nonEmpty) {
        b = right.pop()
        stop = a != b
      }
      else
        stop = true
    }

    (a, b)
  }
}

trait MostConnectedParticipantsStrategy extends ParticipantChoosingStrategy{
  override def choseEndPoints(source:Participant,
                     destination:Participant,
                     previouslyChosen:Set[(Participant, Participant)],
                     model:SearchModel) = {

    // Find the components to which each endpoint belongs
    val components = (model.getConnectedComponentOf(source), model.getConnectedComponentOf(destination))


    // Sorted by degree
    var (sA, sB) = components match {
      case (Some(comS), Some(comD)) =>
        val sortedS = comS.toSeq.map(n => (model.degree(n), n)).sortBy(n => n._1)//.reverse
        val sortedD = comD.toSeq.map(n => (model.degree(n), n)).sortBy(n => n._1)//.reverse
        (sortedS, sortedD)
      case _ => throw new RuntimeException("BEEEP!!")
    }


    val ssA = new mutable.Stack[Participant]()
    ssA.pushAll(sA map (_._2))

    val ssB = new mutable.Stack[Participant]()
    ssB.pushAll(sB map (_._2))

    val allNodes = new mutable.Stack[Participant]()
    allNodes.pushAll(model.nodes.toSeq.sortBy(n => model.degree(n)))//.reverse)

    var endpoints:(Participant, Participant) = (null, null)

    do{
      endpoints = pickEndpoints(ssA, ssB)
    }while(!differentEndpoints(endpoints, previouslyChosen) && ssA.nonEmpty && ssB.nonEmpty)

    // Fallback if there are no new nodes in the components
    if(!differentEndpoints(endpoints, previouslyChosen)){
      ssA.pushAll(sA map (_._2))
      do{
        endpoints = pickEndpoints(ssA, allNodes)
      }while(!differentEndpoints(endpoints, previouslyChosen) && ssA.nonEmpty && allNodes.nonEmpty)
    }


    endpoints
  }



}

trait MostRecentParticipantsStrategy extends ParticipantChoosingStrategy{
  def participantIntroductions:mutable.HashMap[Participant, Int]

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)],
                              model: SearchModel): (Participant, Participant) = {


    // Uncomment for more sophisticated, less effective version
//    val mostRecentIteration = participantIntroductions.values.max
//
//    val possibleEndpoints = (participantIntroductions map {
//      case(p, i) =>
//        if(i == mostRecentIteration)
//          Some(p)
//        else
//          None
//    } collect { case Some(p) => p }).toSeq
//
//
//    val sourceComponent = model.getConnectedComponentOf(source).get.toSet
//    val destComponent = model.getConnectedComponentOf(destination).get.toSet
//
//    val sourceChoices = possibleEndpoints.filter(p => sourceComponent.contains(p)).toSeq.sortBy(p => model.degree(p))//.reverse
//    val destChoices = possibleEndpoints.filter(p => destComponent.contains(p)).toSeq.sortBy(p => model.degree(p))//.reverse
//
//    val ssA = new mutable.Stack[Participant]()
//    ssA.push(source)
//    ssA.pushAll(sourceChoices)
//
//    val ssB = new mutable.Stack[Participant]()
//    ssB.push(destination)
//    ssB.pushAll(destChoices)
//
//    val allNodes = new mutable.Stack[Participant]()
//    allNodes.pushAll(possibleEndpoints.toSeq.sortBy(n => model.degree(n)))//.reverse)
//
//    var endpoints:(Participant, Participant) = (null, null)
//
//
//    do{
//      endpoints = pickEndpoints(ssA, ssB)
//    }while(!differentEndpoints(endpoints, previouslyChosen) && ssA.nonEmpty && ssB.nonEmpty)
//
//    // Fallback if there are no new nodes in the components
//    if(!differentEndpoints(endpoints, previouslyChosen)){
//      ssA.pushAll(Seq(source) ++ sourceChoices)
//      do{
//        endpoints = pickEndpoints(ssA, allNodes)
//      }while(!differentEndpoints(endpoints, previouslyChosen) && ssA.nonEmpty && allNodes.nonEmpty)
//    }
//
//    endpoints
    //////////////////////////

    // Uncomment for the simpler more effective implementation
    val mostRecentIteration = participantIntroductions.values.max

    val possibleEndpoints = participantIntroductions map { case(p, i) => if(i == mostRecentIteration) Some(p) else None} collect { case Some(p) => p }


    val sourceComponent = model.getConnectedComponentOf(source).get.toSet
    val destComponent = model.getConnectedComponentOf(destination).get.toSet

    val sourceChoices = possibleEndpoints.filter(p => sourceComponent.contains(p)).toSeq.sortBy(p => model.degree(p)).reverse
    val destChoices = possibleEndpoints.filter(p => destComponent.contains(p)).toSeq.sortBy(p => model.degree(p)).reverse

    val endpoints = pickEndpoints(sourceChoices, destChoices, source, destination, previouslyChosen)

    endpoints
    /////////////////////

  }

  private def pickEndpoints(sourceChoices:Seq[Participant], destChoices:Seq[Participant],
                    source:Participant, destination:Participant,
                   previouslyChosen:Set[(Participant, Participant)]):(Participant, Participant) = {
    val endpoints = {
      val a = if(sourceChoices.size > 0) sourceChoices.head else source
      val b = if(destChoices.size > 0) destChoices.head else destination

      val candidates = if(a != b)
        (a, b)
      else
        (source, a)

      if(differentEndpoints(candidates, previouslyChosen))
        candidates
      else if(candidates == (source, destination))
        candidates
      else {
        val newSourceChoices = if(sourceChoices.size > 0) sourceChoices.tail else Seq()

        val newDestChoices = if(destChoices.size > 0) destChoices.tail else Seq()

        pickEndpoints(newSourceChoices, newDestChoices, source, destination, previouslyChosen)
      }
    }




    endpoints
  }

}

trait FurthestParticipantStrategy extends ParticipantChoosingStrategy{

  val distancesCache = new mutable.HashMap[Participant, Int]()

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)],
                              model: SearchModel): (Participant, Participant) = {

    val sourceComponent = sortByDistance(model.getConnectedComponentOf(source).get.toSeq, source, model)
    val destComponent = sortByDistance(model.getConnectedComponentOf(destination).get.toSeq, destination, model)


    val ssA = new mutable.Stack[Participant]()
    ssA.pushAll(sourceComponent)

    val ssB = new mutable.Stack[Participant]()
    ssB.pushAll(destComponent)

    var endpoints:(Participant, Participant) = (null, null)

    do{
      endpoints = pickEndpoints(ssA, ssB)
    }while(!differentEndpoints(endpoints, previouslyChosen) && ssA.nonEmpty && ssB.nonEmpty)

    endpoints
  }

  private def sortByDistance(s:Seq[Participant], reference:Participant, model:SearchModel):Seq[Participant] = {
    //val maxDist = model.numNodes -1 // This is an upper bound of the distance

    val distances = s map {
      node =>
        if(distancesCache.contains(node))
          Some(distancesCache(node))
        else {
          model.shortestPath(reference, node) match {
            case Some(path) =>
              val dist = path.size
              distancesCache += node -> dist
              Some(dist)
            case None =>
              None
          }
        }
    }

    //val degrees = s map (node => model.degree(node))

    //val scores = distances zip degrees map { case (d, dd) => d +dd }

    val sorted = s.zip(distances).collect{
      case (p, Some(d)) => (p, d)
    }.sortBy(_._2).reverse

    sorted map (_._1)
  }
}

trait ExploreExploitParticipantsStrategy extends ParticipantChoosingStrategy{


  // Abstract members
  def observeState:State
  def getIterationNum:Int
  def getUsedActions:Seq[Action]
  val policy:Policy
  var lastActionChosen:Option[Action] = None
  val chosenEndpointsLog = new mutable.ArrayBuffer[((Participant, Participant),(Participant, Participant))]
  ///////////////////


  // Concrete members
  val queryLog = new mutable.ArrayBuffer[(Participant, Participant)]
  val introductions:mutable.HashMap[Participant, Int] = new mutable.HashMap[Participant, Int]()
  ///////////////////

  // Private auxiliary members
  protected val exploitChooser = new {} with MostRecentParticipantsStrategy {
    override def participantIntroductions: mutable.HashMap[Participant, Int] = introductions
  }
//    override val participantIntroductions = introductions
//  }

//  protected val exploitChooser = new {} with FurthestParticipantStrategy {}

  protected val exploreChooser = new {} with MostConnectedParticipantsStrategy {}

  private def possibleActions:Seq[Action] = getUsedActions//Seq(ExploreEndpoints(), ExploitEndpoints())

  private def peekState(a:Participant, b:Participant):State = {
    this.queryLog += Tuple2(a, b)
    val containsA = introductions.contains(a)
    val containsB = introductions.contains(b)

    if (!containsA)
      introductions += a -> getIterationNum
    if (!containsB)
      introductions += b -> getIterationNum

    val state = this.observeState

    // Undo the changes
    queryLog.remove(queryLog.size - 1)
    if (!containsA)
      introductions.remove(a)
    if (!containsB)
      introductions.remove(b)

    state
  }
  ///////////////////////////

  // Alternate state observation methods
  def observeExploreState(source: Participant, destination: Participant,
                          previouslyChosen: Set[(Participant, Participant)]
                          , model: SearchModel):((Participant, Participant), State) =
    observeStrategtState(exploreChooser, source, destination, previouslyChosen, model)

  def observeExploitState(source: Participant, destination: Participant,
                          previouslyChosen: Set[(Participant, Participant)]
                          , model: SearchModel):((Participant, Participant), State) =
    observeStrategtState(exploitChooser, source, destination, previouslyChosen, model)

  private def observeStrategtState(chooser:ParticipantChoosingStrategy,
                                   source: Participant, destination: Participant,
                                   previouslyChosen: Set[(Participant, Participant)],
                                   model: SearchModel):((Participant, Participant), State) = {
    val endpoints = chooser.choseEndPoints(source, destination, previouslyChosen, model)

    var a = endpoints._1
    var b = endpoints._2

    val state = peekState(a, b)

    (endpoints, state)
  }
  /////////////////////////////////////

  // Strategy implementation
  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)]
                              , model: SearchModel): (Participant, Participant) = {

    // Endpoint choices


    // State variations
    // Explore state
    val (exploreEndpoints, exploreState) = observeExploreState(source, destination, previouslyChosen, model)


    // Exploit state
    val (exploitEndpoints, exploitState) = observeExploitState(source, destination, previouslyChosen, model)
    //////////////////////

    // DEBUG: Keep track of how many times the explore/exploit pairs are equal
    chosenEndpointsLog += Tuple2(exploreEndpoints, exploitEndpoints)


    val states = possibleActions map {
      case _:ExploreEndpoints =>
        exploreState
      case _:ExploitEndpoints =>
        exploitState
    }


    // Choose the action
    val (_, action) = policy.selectAction(states, possibleActions)

    lastActionChosen = Some(action)

    val chosenEndpoints = action match {
      case _:ExploreEndpoints =>
        exploreEndpoints
      case _:ExploitEndpoints =>
        exploitEndpoints
    }

    // Persist the changes to the state
    this.queryLog += chosenEndpoints

    val a = chosenEndpoints._1
    val b = chosenEndpoints._2

    if(!introductions.contains(a))
      introductions += a -> getIterationNum
    if(!introductions.contains(b))
      introductions += b -> getIterationNum
    //////////////////////////////////

    // Return the chosen endpoints
    chosenEndpoints
  }
  /////////////////////////
}
