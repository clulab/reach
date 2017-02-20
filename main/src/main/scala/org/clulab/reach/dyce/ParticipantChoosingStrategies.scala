package org.clulab.reach.dyce

import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph
import SearchAgent.Model

/**
  * Created by enrique on 19/02/17.
  */
trait ParticipantChoosingStrategy {
  def choseEndPoints(source:Participant,
                     destination:Participant,
                     model:Model):(Participant, Participant)
}

trait MostConnectedParticipantsStrategy extends ParticipantChoosingStrategy{
  override def choseEndPoints(source:Participant,
                     destination:Participant,
                     model:Model) = {

    // Find the components to which each endpoint belongs
    val components = (GraphUtils.getComponentOf(source, model), GraphUtils.getComponentOf(destination, model))

    // Sort the components by their degree
    val (sA, sB) = components match {
      case (Some(comS:Set[model.NodeT]), Some(comD:Set[model.NodeT])) =>
        val sortedS = comS.toSeq.sortBy(n => n.degree).map(n => (n.degree, n.value))
        val sortedD = comD.toSeq.sortBy(n => n.degree).map(n => (n.degree, n.value))
        (sortedS, sortedD)
      case _ => throw new RuntimeException("BEEEP!!")
    }

    val endpoints = if(sA.head != sB.head){
      (sA.head._2, sB.head._2)
    }
    else{
      (sA.head._2, sB.tail.head._2)
    }

    endpoints
  }
}
