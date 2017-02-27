package org.clulab.reach.dyce

import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph
import collection.mutable
import SearchAgent.Model

/**
  * Created by enrique on 19/02/17.
  */
trait ParticipantChoosingStrategy {
  def choseEndPoints(source:Participant,
                     destination:Participant,
                     previouslyChosen:Set[(Participant, Participant)],
                     model:Model):(Participant, Participant)
}

trait MostConnectedParticipantsStrategy extends ParticipantChoosingStrategy{
  override def choseEndPoints(source:Participant,
                     destination:Participant,
                     previouslyChosen:Set[(Participant, Participant)],
                     model:Model) = {

    // Find the components to which each endpoint belongs
    val components = (GraphUtils.getComponentOf(source, model), GraphUtils.getComponentOf(destination, model))

    // Sort the components by their degree
    var (sA, sB) = components match {
      case (Some(comS:Set[model.NodeT]), Some(comD:Set[model.NodeT])) =>
        val sortedS = comS.toSeq.sortBy(n => n.degree).reverse.map(n => (n.degree, n.value))
        val sortedD = comD.toSeq.sortBy(n => n.degree).reverse.map(n => (n.degree, n.value))
        (sortedS, sortedD)
      case _ => throw new RuntimeException("BEEEP!!")
    }


    val ssA = new mutable.Stack[Participant]()
    ssA.pushAll(sA map (_._2))

    val ssB = new mutable.Stack[Participant]()
    ssB.pushAll(sB map (_._2))

    val allNodes = new mutable.Stack[Participant]()
    allNodes.pushAll(model.nodes.toSeq.sortBy(n => n.degree).reverse.map(n => n.value))

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
