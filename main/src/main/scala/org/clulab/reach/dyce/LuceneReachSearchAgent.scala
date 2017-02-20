package org.clulab.reach.dyce


import org.clulab.reach.dyce.QueryStrategy._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph

/**
  * Created by enrique on 18/02/17.
  */
class LuceneReachSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with LuceneIRStrategy
  with REACHIEStrategy {


  override val model:Graph[Participant, LDiEdge] = Graph[Participant, LDiEdge](participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: Graph[Participant, LDiEdge]) = Query(Cascade, source, Some(destination))


}