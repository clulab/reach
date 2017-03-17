package org.clulab.reach.focusedreading.agents

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.focusedreading.ie.IEStrategy
import org.clulab.reach.focusedreading.ir.{IRStrategy, Query}
import org.clulab.reach.focusedreading.models._
import org.clulab.reach.focusedreading.{Connection, Participant, ParticipantChoosingStrategy}
import org.clulab.reach.focusedreading.tracing.IterativeStep

import scala.collection.mutable
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph

/**
  * Created by enrique on 18/02/17.
  */


trait SearchAgent extends LazyLogging with IRStrategy with IEStrategy with ParticipantChoosingStrategy {

  val model:SearchModel
  var iterationNum = 0

  val triedPairs = new mutable.HashSet[(Participant, Participant)]

  val trace = new mutable.ArrayBuffer[IterativeStep]

  def focusedSearch(source:Participant, destination:Participant):Unit ={
    logger.info(s"Starting focused search with end points $source and $destination")
    do{
      iterationNum += 1
      logger.info(s"Iteration #$iterationNum")
      val (a, b) = choseEndPoints(source, destination, triedPairs.toSet, model)
      triedPairs += Tuple2(a, b)
      val query = choseQuery(a, b, this.model)
      logger.info(s"Chosen query: $query")
      val paperIds = informationRetrival(query)
      if(!paperIds.isEmpty)
        logger.info(s"Found ${paperIds.size} IR matches")
      else
        logger.info(s"Empty query $query")
      val findings = informationExtraction(paperIds)
      logger.info(s"Found ${findings.size} connections")

      val modelGraphBefore = getStateGraph
      reconcile(findings)
      val modelGraphAfter = getStateGraph

      // Store the step into the trace
      // TODO: Return IR scores from IRStrategy
      // TODO: Return the PMCIDS in each connection
      val step = IterativeStep(iterationNum, modelGraphBefore, modelGraphAfter, (a, b), query.strategy,
        paperIds.map(p => (p, 0.0)), findings)

      // Add it to the trace
      trace += step

    }while(!hasFinished(source, destination, this.model))
    logger.info(s"Focused search finished after $iterationNum iterations")
  }

  def hasFinished(source:Participant, destination:Participant, model:SearchModel):Boolean = {
    if(successStopCondition(source, destination, model) != None)
      true
    else if(failureStopCondition(source, destination, model))
      true
    else
      false
  }

  def successStopCondition(source:Participant,
                           destination:Participant,
                           model:SearchModel):Option[Seq[Seq[Connection]]]

  def failureStopCondition(source:Participant,
                           destination:Participant,
                           model:SearchModel):Boolean


  def choseQuery(source:Participant, destination:Participant, model:SearchModel):Query


  def reconcile(findings:Iterable[Connection]):Unit

  def getStateGraph = this.model match {
    case gsfModel:GFSModel => Some(gsfModel.G.clone())
    case _ => None
  }
}


abstract class SimplePathAgent(participantA:Participant, participantB:Participant) extends SearchAgent {

  val model:SearchModel = new GFSModel(participantA, participantB)

  var (nodesCount, edgesCount) = (0, 0)
  var (prevNodesCount, prevEdgesCount) = (0, 0)


  override def successStopCondition(source: Participant, destination: Participant, model: SearchModel) = {
    model.shortestPath(source, destination) match {
      case Some(path) => Some(Seq(path))
      case None => None
    }
  }


  override def failureStopCondition(source: Participant,
                                    destination: Participant,
                                    model: SearchModel) = {
    if(this.iterationNum >= 10)
      true
    else if((nodesCount, edgesCount) == (prevNodesCount, prevEdgesCount)){
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
    this.prevNodesCount = this.model.numNodes
    this.prevEdgesCount = this.model.numEdges
    // Make labeled directed edges out of each connection
    // Add them to the graph
    this.model addEdges connections
    // How large is it now?
    this.nodesCount = this.model.numNodes
    this.edgesCount = this.model.numEdges


    logger.info(s"Model participants; Before: $prevNodesCount\tAfter: $nodesCount")
    logger.info(s"Model connections; Before: $prevEdgesCount\tAfter: $edgesCount")
  }

}

abstract class MultiplePathsAgent(participantA:Participant, participantB:Participant, val maxIdleIterations:Int = 10)
  extends SimplePathAgent(participantA, participantB){

  var noChangeIterations = 0
  var prevSolution:Seq[Seq[Connection]] = Seq()

  private def sameAs(a:Seq[Seq[Connection]], b:Seq[Seq[Connection]]):Boolean = {
    if(a.size != b.size)
      false
    else{
      true
    }
  }

  override def successStopCondition(source: Participant, destination: Participant, model: SearchModel) = {
    val allPaths = model.allPaths(source, destination).toSeq

    if(sameAs(allPaths, prevSolution))
      noChangeIterations += 1
    else
      noChangeIterations = 0

    if(allPaths.nonEmpty && (allPaths.size >= 10 || this.noChangeIterations == maxIdleIterations))
      Some(allPaths)
    else
      None
  }

  override def failureStopCondition(source: Participant, destination: Participant, model: SearchModel): Boolean = {
    if(this.noChangeIterations >= 10)
      true
    else if((nodesCount, edgesCount) == (prevNodesCount, prevEdgesCount)){
      logger.info("The model didn't change.")
      true
    }
    else
      false
  }

}