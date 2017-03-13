package org.clulab.reach.dyce.agents

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.dyce.ie.{REACHIEStrategy, SQLIteIEStrategy}
import org.clulab.reach.dyce.ir.QueryStrategy._
import org.clulab.reach.dyce.ir.{LuceneIRStrategy, Query, SQLIRStrategy}
import org.clulab.reach.dyce.models._
import org.clulab.reach.dyce.{Connection, MostConnectedParticipantsStrategy, Participant}

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


  //  override val model:Model = Graph[Participant, LDiEdge](participantA, participantB) // Directed graph with the model.
  override val model:SearchModel = new GFSModel(participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, source, Some(destination))


}


