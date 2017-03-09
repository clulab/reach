package org.clulab.reach.dyce

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.dyce.models._

import scala.collection.mutable

/**
  * Created by enrique on 18/02/17.
  */


trait SearchAgent extends LazyLogging with IRStrategy with IEStrategy with ParticipantChoosingStrategy {

  val model:SearchModel
  var iterationNum = 0

  val triedPairs = new mutable.HashSet[(Participant, Participant)]

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
      reconcile(findings)
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
                           model:SearchModel):Option[Seq[Connection]]

  def failureStopCondition(source:Participant,
                           destination:Participant,
                           model:SearchModel):Boolean


  def choseQuery(source:Participant, destination:Participant, model:SearchModel):Query


  def reconcile(findings:Iterable[Connection]):Unit
}


abstract class SimplePathAgent(participantA:Participant, participantB:Participant) extends SearchAgent {

  val model:SearchModel = new GFSModel(participantA, participantB)

  var (nodesCount, edgesCount) = (0, 0)
  var (prevNodesCount, prevEdgesCount) = (0, 0)

  override def successStopCondition(source: Participant, destination: Participant, model: SearchModel) = {
    /* SUBSTITUTED
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
    */


    model.shortestPath(source, destination)

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
    this.prevNodesCount = this.model.nodes.size
    this.prevEdgesCount = this.model.edges.size
    // Make labeled directed edges out of each connection
     // SUBSTITUTED
//    val edges = connections map {c => (c.controller ~+> c.controlled)(c.sign)}
    // Add them to the graph
    this.model addEdges connections
    // How large is it now?
    this.nodesCount = this.model.nodes.size
    this.edgesCount = this.model.edges.size

    logger.info(s"Model participants; Before: $prevNodesCount\tAfter: $nodesCount")
    logger.info(s"Model connections; Before: $prevEdgesCount\tAfter: $edgesCount")
  }

}

//abstract class MultiplePathsAgent(participantA:Participant, participantB:Participant)
//  extends SimplePathAgent(participantA, participantB){
//
//  override def successStopCondition(source: Participant, destination: Participant, model: SearchModel) = {
//    (model find source, model find destination) match {
//      case (Some(pa), Some(pb)) =>
//        pa shortestPathTo pb match{
//          case Some(path) => Some{
//            path.edges.map{
//              e => Connection(e.source, e.target, e.label.value.asInstanceOf[Boolean], Seq(""))
//            }.toSeq
//          }
//          case None => None
//        }
//      case _ => None
//    }
//  }
//}