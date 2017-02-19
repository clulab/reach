package org.clulab.reach.dyce

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions.{BioMention, BioTextBoundMention, CorefEventMention, CorefMention}

import scala.collection.mutable
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph // shortcuts

/**
  * Created by enrique on 18/02/17.
  */
trait SearchAgent extends LazyLogging{

  val model:Graph[Participant, LDiEdge]
  var iterationNum = 0

  def focusedSearch(source:Participant, destination:Participant):Unit ={
    logger.info(s"Starting focused search with end points $source and $destination")
    while(hasFinished(source, destination, this.model)){
      iterationNum += 1
      logger.info(s"Iteration #$iterationNum")
      val (a, b) = choseEndPoints(source, destination, this.model)
      logger.info(s"Chosen end query elements: $a, $b")
      val query = choseQuery(source, destination, this.model)
      logger.info(s"Chosen query: $query")
      val findings = lucenePlusReach(query)
      logger.info(s"Found ${findings.size} connections")
      reconcile(findings)
    }
    logger.info(s"Focused search finished after $iterationNum iterations")
  }

  def hasFinished(source:Participant, destination:Participant, model:Graph[Participant, LDiEdge]):Boolean = {
    if(successStopCondition(source, destination, model) != None)
      true
    else if(failureStopCondition(source, destination, model))
      true
    else
      false
  }

  def successStopCondition(source:Participant,
                           destination:Participant,
                           model:Graph[Participant, LDiEdge]):Option[Seq[Connection]]

  def failureStopCondition(source:Participant,
                           destination:Participant,
                           model:Graph[Participant, LDiEdge]):Boolean

  def choseEndPoints(source:Participant,
                     destination:Participant,
                     model:Graph[Participant, LDiEdge]):(Participant, Participant)

  def choseQuery(source:Participant, destination:Participant, model:Graph[Participant, LDiEdge]):Query

  def lucenePlusReach(query:Query):Iterable[Connection]

  def reconcile(findings:Iterable[Connection]):Unit
}


abstract class SimplePathAgent(participantA:Participant, participantB:Participant) extends SearchAgent with LazyLogging{

  val G:Graph[Participant, LDiEdge] = Graph[Participant, LDiEdge](participantA, participantB)

  var (nodesCount, edgesCount) = (0, 0)
  var (prevNodesCount, prevEdgesCount) = (0, 0)

  override def successStopCondition(source: Participant, destination: Participant, model: Graph[Participant, LDiEdge]) = {
    (model find source, model find destination) match {
      case (Some(pa), Some(pb)) =>
        pa shortestPathTo pb match{
          case Some(path) => Some{
            path.edges.map{
              e => Connection(e.source, e.target, e.label.value.asInstanceOf[Boolean], Seq(""))
            }.toSeq
          }
          case None => None
        }
      case _ => None
    }
  }


  override def failureStopCondition(source: Participant,
                                    destination: Participant,
                                    model: Graph[Participant, LDiEdge]) = {
    if((nodesCount, edgesCount) == (prevNodesCount, prevEdgesCount)){
      logger.info("The model didn't change.")
      true
    }
    else
      false
  }

  /***
    * Creates a new graph with the connections of the existing graph and new connections from the second argument
    *
    * @param connections New information to incorporate to the graph
    * @return A tuple with the number of nodes and the number of edges after the modification
    */
   override def reconcile(connections: Iterable[Connection]){
    // How large was the graph before?
    this.prevNodesCount = this.G.nodes.size
    this.prevEdgesCount = this.G.edges.size
    // Make labeled directed edges out of each connection
    val edges = connections map {c => (c.controller ~+> c.controlled)(c.sign)}
    // Add them to the graph
    this.G ++= edges
    // How large is it now?
    this.nodesCount = this.G.nodes.size
    this.edgesCount = this.G.edges.size

    logger.info(s"Model participants; Before: $prevNodesCount\tAfter: $nodesCount")
    logger.info(s"Model connections; Before: $prevEdgesCount\tAfter: $edgesCount")
  }

}