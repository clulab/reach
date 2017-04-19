package org.clulab.reach.focusedreading

import collection.mutable
import org.clulab.reach.focusedreading.models._
import org.clulab.reach.focusedreading.reinforcement_learning.actions._
import org.clulab.reach.focusedreading.reinforcement_learning.policies.Policy
import org.clulab.reach.focusedreading.reinforcement_learning.states.{FocusedReadingState, State}

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
        val sortedS = comS.toSeq.map(n => (model.degree(n), n)).sortBy(n => n._1).reverse
        val sortedD = comD.toSeq.map(n => (model.degree(n), n)).sortBy(n => n._1).reverse
        (sortedS, sortedD)
      case _ => throw new RuntimeException("BEEEP!!")
    }


    val ssA = new mutable.Stack[Participant]()
    ssA.pushAll(sA map (_._2))

    val ssB = new mutable.Stack[Participant]()
    ssB.pushAll(sB map (_._2))

    val allNodes = new mutable.Stack[Participant]()
    allNodes.pushAll(model.nodes.toSeq.sortBy(n => model.degree(n)).reverse)

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

trait MostConnectedAndRecentParticipantsStrategy extends ParticipantChoosingStrategy{
  val participantIntroductions:mutable.HashMap[Participant, Int]

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)],
                              model: SearchModel): (Participant, Participant) = {


    val mostRecentIteration = participantIntroductions.values.max

    val possibleEndpoints = participantIntroductions map { case(p, i) => if(i == mostRecentIteration) Some(p) else None} collect { case Some(p) => p }


    val sourceComponent = model.getConnectedComponentOf(source).get.toSet
    val destComponent = model.getConnectedComponentOf(destination).get.toSet

    val sourceChoices = possibleEndpoints.filter(p => sourceComponent.contains(p)).toSeq.sortBy(p => model.degree(p)).reverse
    val destChoices = possibleEndpoints.filter(p => destComponent.contains(p)).toSeq.sortBy(p => model.degree(p)).reverse

    val endpoints = {
      val a = if(sourceChoices.size > 0) sourceChoices.head else source
      val b = if(destChoices.size > 0) destChoices.head else destination

      if(a != b)
        (a, b)
      else
        (source, a)
    }


    endpoints

  }

}

trait ExploreExploitParticipantsStrategy extends ParticipantChoosingStrategy{


  // Abstract members
  def observeState:State
  def getIterationNum:Int
  val policy:Policy
  var lastActionChosen:Option[Action] = None
  val chosenEndpointsLog = new mutable.ArrayBuffer[((Participant, Participant),(Participant, Participant))]
  ///////////////////


  // Concrete members
  val queryLog = new mutable.ArrayBuffer[(Participant, Participant)]
  val introductions:mutable.HashMap[Participant, Int] = new mutable.HashMap[Participant, Int]()
  ///////////////////

  // Private auxiliary members
  protected val exploitChooser = new {} with MostConnectedAndRecentParticipantsStrategy {
    override val participantIntroductions = introductions
  }

  protected val exploreChooser = new {} with MostConnectedParticipantsStrategy {}

  private val possibleActions:Seq[Action] = Seq(ExploreEndpoints(), ExploitEndpoints())

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
