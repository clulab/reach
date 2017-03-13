package org.clulab.reach.dyce.executable

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.dyce.{Connection, Participant}
import org.clulab.reach.dyce.agents._
import java.io.File
import org.json4s.JsonDSL._
import org.json4s._
import scala.collection.JavaConverters._
import org.json4s.native.JsonMethods._
import org.apache.commons.io.{ FileUtils, FilenameUtils }


import scala.collection.mutable

/**
  * Created by enrique on 12/03/17.
  */
object MultiplePath extends App with LazyLogging{


  def getParticipants(path:List[Connection]):List[String] = {
    path match {
      case h::t =>
        h.controller.id :: (if(t == Nil) List(h.controlled.id) else getParticipants(t))
      case Nil => Nil
    }
  }

  def serializeItem(paths:Iterable[Seq[Connection]], groundTruth:Seq[String], matchType:String, agent:SearchAgent, file:File): Unit ={

    val pathValues = paths.take(10) map {
      path =>
        ("path" -> (path map (c => s"${c.controller} ${if(c.sign) "+" else "-"}> ${c.controlled}"))) ~
          ("evicende" -> {
            path map {
              c =>
                agent.getEvidence(c)
            }
          })
    }

    val pathInfo = ("ground_truth" -> groundTruth) ~
      ("match_type" -> matchType) ~
      ("paths" -> pathValues)


    val model = agent.model
    val participants = paths flatMap (path => path flatMap (c => Set(c.controller, c.controlled)))


    val jsonObj = ("info" -> pathInfo) ~ ("degrees" -> participants.map(d => (d.toString -> model.degree(d))).toMap)

    val json = compact(render(jsonObj))

    FileUtils.writeLines(file, Seq(json).asJavaCollection)
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
    //val agent = new SQLiteSearchAgent(participantA, participantB)
    val agent = new SQLiteMultiPathSearchAgent(participantA, participantB)
    agent.focusedSearch(participantA, participantB)

    agent.successStopCondition(participantA, participantB, agent.model) match {
      case Some(paths) =>
        successes += 1
        successIterations += agent.iterationNum
        logger.info("Success!!")


//        logger.info("")
//        logger.info("Path: " + path.mkString(" || "))


        // Analysis
        val participants = getParticipants(paths.head.toList)
        val groundTruth = datum.productIterator.map(_.asInstanceOf[String]).toList
//        logger.info("GT: " + groundTruth.mkString(" || "))

        assert(participants.head == groundTruth.head)
        assert(participants.last == groundTruth.last)

        val matchType = if(participants == groundTruth) {
          hits += 1
          logger.info(s"Type: Exact")
          "exact"
        }
        else{
          // Get the length of the paths
          pathLengths += participants.length
          logger.info(s"Type: Alternative")
          "alternative"
        }

        serializeItem(paths, groundTruth, "Undefined", agent, new File(s"hits_multi/hit_$ix.json"))


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

