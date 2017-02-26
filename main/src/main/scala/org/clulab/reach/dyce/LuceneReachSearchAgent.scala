package org.clulab.reach.dyce


import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.dyce.QueryStrategy._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph
import SearchAgent.Model

/**
  * Created by enrique on 18/02/17.
  */
class LuceneReachSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with LuceneIRStrategy
  with REACHIEStrategy {


  override val model:Model = Graph[Participant, LDiEdge](participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: Model) = Query(Cascade, source, Some(destination))


}

class SQLiteSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with SQLIRStrategy
  with SQLIteIEStrategy {


  override val model:Model = Graph[Participant, LDiEdge](participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: Model) = Query(Cascade, source, Some(destination))


}


object CoolSearchAgent extends App with LazyLogging{

  val participantA =  Participant("uniprot", "Q13315") // ATM, Grounding ID of the controller
  // val participantA = Participant("uniprot","P19838")
  val participantB = Participant("uniprot", "P42345") // mTOR, Grounding ID of the controller
  //val participantB = Participant("uniprot", "O14757") // Chek1

  logger.info(s"About to start a focused search with")

  //val agent = new LuceneReachSearchAgent(participantA, participantB)
  val agent = new SQLiteSearchAgent(participantA, participantB)
  agent.focusedSearch(participantA, participantB)

  agent.successStopCondition(participantA, participantB, agent.model) match {
    case Some(path) =>
      println("Success!!")
      logger.info(path.mkString(" || "))
//      for(c <- path){
//        println(s"Evidence of $c")
//        println()
//        val sentences = evidence(c)
//        println(sentences.mkString("\n"))
//        println("----------------------------")
//      }
    case None => println("Failure")
  }

}