package org.clulab.reach.focusedreading

import collection.mutable
import org.clulab.reach.focusedreading.models._
import org.clulab.reach.focusedreading.reinforcement_learning.actions.Actions
import org.clulab.reach.focusedreading.reinforcement_learning.policies.Policy
import org.clulab.reach.focusedreading.reinforcement_learning.states.State

/**
  * Created by enrique on 19/02/17.
  */
trait ParticipantChoosingStrategy {
  def choseEndPoints(source:Participant,
                     destination:Participant,
                     previouslyChosen:Set[(Participant, Participant)],
                     model:SearchModel):(Participant, Participant)
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
  def pickEndpoints(sA:mutable.Stack[Participant], sB:mutable.Stack[Participant], defaultA:Participant, defaultB:Participant):(Participant, Participant) = {

    val (left, right) = if(sA.size <= sB.size) (sA, sB) else (sB, sA)
    val a = left.size match {
      case 0 => defaultA
      case _ => left.pop()
    }

    var b = right.size match {
      case 0 => defaultB
      case _ => right.pop()
    }

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

trait ExploreExploitParticipantsStrategy extends ParticipantChoosingStrategy{


  def observeState:State
  val policy:Policy

  val introductions:mutable.HashMap[Participant, Int] = new mutable.HashMap[Participant, Int]()

  val exploitChooser = new {} with MostConnectedAndRecentParticipantsStrategy {
    override val participantIntroductions = introductions
  }

  val exploreChooser = new {} with MostConnectedParticipantsStrategy {}

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)]
                              , model: SearchModel): (Participant, Participant) = {

    val state = this.observeState
    val action = policy.selectAction(state)

    action match {
      case Actions.Explore =>
        exploreChooser.choseEndPoints(source, destination, previouslyChosen, model)
      case Actions.Exploit =>
        exploitChooser.choseEndPoints(source, destination, previouslyChosen, model)
    }
  }
}
