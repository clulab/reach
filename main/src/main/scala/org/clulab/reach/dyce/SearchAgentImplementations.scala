package org.clulab.reach.dyce


import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.dyce.QueryStrategy._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph
import SearchAgent.Model
import collection.mutable

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

  def getParticipants(path:List[Connection]):List[String] = {
    path match {
      case h::t =>
        h.controller.id :: (if(t == Nil) List(h.controlled.id) else getParticipants(t))
      case Nil => Nil
    }
  }

  // The first argument is the input file
  val dataSet:Iterable[(String, String, String)] = io.Source.fromFile(args(0)).getLines
    .map{
      s =>
        val t = s.split("\t");
        (t(0), t(1), t(2))
    }.toSeq

  var (successes, failures, hits) = (0, 0, 0)
  val pathLengths = new mutable.ArrayBuffer[Int]
  val successIterations = new mutable.ArrayBuffer[Int]
  val failureIterations = new mutable.ArrayBuffer[Int]

  logger.info(s"About to do ${dataSet.size} searches ...")
  logger.info(s"")


  /***
    * Prints the sentences that back the evidence found
    * @param path
    */
  def printEvidence(path: Seq[Connection], agent:SearchAgent) = {
    val evidence:Seq[Iterable[String]] = path map agent.getEvidence

    for((c, e) <- path zip evidence){
      logger.info("")
      logger.info(s"Evidence for connection $c")
      logger.info("")
      for(s <- e){
        logger.info(s)
      }
    }
  }

  for((datum, ix) <- dataSet.zipWithIndex){
    logger.info(s"Searching for path: ${datum._1}-${datum._2}-${datum._3}")

    val participantA =  Participant("", datum._1)
    val participantB = Participant("", datum._3)

    logger.info(s"About to start a focused search $ix of ${dataSet.size}")

    //val agent = new LuceneReachSearchAgent(participantA, participantB)
    val agent = new SQLiteSearchAgent(participantA, participantB)
    agent.focusedSearch(participantA, participantB)

    agent.successStopCondition(participantA, participantB, agent.model) match {
      case Some(path) =>
        successes += 1
        successIterations += agent.iterationNum
        logger.info("Success!!")
        logger.info("Path: " + path.mkString(" || "))
      //      for(c <- path){
      //        println(s"Evidence of $c")
      //        println()
      //        val sentences = evidence(c)
      //        println(sentences.mkString("\n"))
      //        println("----------------------------")
      //      }

        printEvidence(path, agent)

       // Analysis
        val participants = getParticipants(path.toList)
        val groundTruth = datum.productIterator.toList
        logger.info("GT: " + groundTruth.mkString(" || "))

        assert(participants.head == groundTruth.head)
        assert(participants.last == groundTruth.last)

        if(participants == groundTruth) {
          hits += 1
          logger.info(s"Type: Exact")
        }
        else{
          // Get the length of the paths
          pathLengths += participants.length
          logger.info(s"Type: Alternative")
        }

        logger.info("End Success")

      case None =>
        failures += 1
        failureIterations += agent.iterationNum
        logger.info("Failure")

    }




    logger.info("")
  }
  logger.info(s"Finished attaining $successes successes and $failures failures")
  logger.info("")
  logger.info("Postmortem analysis:")
  logger.info(s"Exact matches: $hits\t Alternative matches: ${successes-hits}")
  logger.info("")
  // Length counts
  val lengths = pathLengths.groupBy(identity).mapValues(_.size)
  logger.info(s"Alternate path lengths:")
  val keys = lengths.keys.toSeq.sortBy(k => lengths(k))
  for(l <- keys){
    logger.info(s"\tLength $l: ${lengths(l)}")
  }

  logger.info("")
  logger.info("Iteration counts for successes")
  val sIterations = successIterations.groupBy(identity).mapValues(_.size)
  for(l <- sIterations.keys.toSeq.sorted){
    logger.info(s"\t$l iterations: ${sIterations(l)}")
  }

  logger.info("")
  logger.info("Iteration counts for failures")
  val fIterations = failureIterations.groupBy(identity).mapValues(_.size)
  for(l <- fIterations.keys.toSeq.sorted){
    logger.info(s"\t$l iterations: ${fIterations(l)}")
  }


}