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

  // The first argument is the input file
  val dataSet:Iterable[(String, String, String)] = io.Source.fromFile(args(0)).getLines
    .map{
      s =>
        val t = s.split("\t");
        (t(0), t(1), t(2))
    }.toSeq

  var (successes, failures) = (0, 0)

  logger.info(s"About to do ${dataSet.size} searches ...")
  logger.info(s"")
  for(datum <- dataSet){
    logger.info(s"Searching for path: ${datum._1}-${datum._2}-${datum._3}")

    val participantA =  Participant("", datum._1)
    val participantB = Participant("", datum._3)

    logger.info(s"About to start a focused search with")

    //val agent = new LuceneReachSearchAgent(participantA, participantB)
    val agent = new SQLiteSearchAgent(participantA, participantB)
    agent.focusedSearch(participantA, participantB)

    agent.successStopCondition(participantA, participantB, agent.model) match {
      case Some(path) =>
        successes += 1
        logger.info("Success!!")
        logger.info(path.mkString(" || "))
      //      for(c <- path){
      //        println(s"Evidence of $c")
      //        println()
      //        val sentences = evidence(c)
      //        println(sentences.mkString("\n"))
      //        println("----------------------------")
      //      }
      case None =>
        failures += 1
        logger.info("Failure")
    }

    logger.info("")
  }
  logger.info(s"Finished attaining $successes successes and $failures failures")
}