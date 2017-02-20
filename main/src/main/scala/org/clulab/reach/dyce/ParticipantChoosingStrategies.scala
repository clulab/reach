package org.clulab.reach.dyce

import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph

/**
  * Created by enrique on 19/02/17.
  */
trait ParticipantChoosingStrategy {
  def choseEndPoints(source:Participant,
                     destination:Participant,
                     model:Graph[Participant, LDiEdge]):(Participant, Participant)
}

trait MostConnectedParticipantsStrategy extends ParticipantChoosingStrategy{
  override def choseEndPoints(source:Participant,
                     destination:Participant,
                     model:Graph[Participant, LDiEdge]) = {
    (source, destination)
  }
}
