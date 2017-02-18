package org.clulab.reach.dyce

import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph // shortcuts

case class Query()

/**
  * Created by enrique on 18/02/17.
  */
trait SearchAgent {

  val model:Graph[Participant, LDiEdge]

  def focusedSearch(source:Participant, destination:Participant):Unit ={
    while(hasFinished(source, destination, this.model)){
      val (a, b) = choseEndPoints(source, destination, this.model)
      val query = choseQuery(source, destination, this.model)
      val findings = lucenePlusReach(query)
      reconcile(findings)
    }
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
