package org.clulab.reach.dyce

import org.clulab.odin.Mention
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions.{BioMention, BioTextBoundMention, CorefEventMention, CorefMention}
import org.clulab.reach.dyce.QueryStrategy._

import scala.collection.mutable

/**
  * Created by enrique on 18/02/17.
  */
abstract class LuceneIndexSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB){


  val maxHits = 200
  val positiveLabels = Vector("Positive_regulation", "Positive_activation", "IncreaseAmount", "AdditionEvent")
  val negativeLabels = Vector("Negative_regulation", "Negative_activation", "DecreaseAmount", "RemovalEvent", "Translocation")
  val evidence = new mutable.HashMap[Connection, mutable.Set[String]]()


  /***
    * Builds edges for the model graph out of raw REACH extractions
    * Filters out all those edges that only happen once
    *
    * @param activations REACH events to use
    * @return Iterable of connection instances
    */
  def buildEdges(activations:Iterable[CorefMention]):Iterable[Connection] = {
    val data:Iterable[Option[Connection]] = activations map {
      a =>
        val event = a.asInstanceOf[CorefEventMention]
        val controller = unravelEvent(event.namedArguments("controller"))
        val controlled = unravelEvent(event.namedArguments("controlled"))
        val text = event.text

        (controller, controlled) match {
          case (Some(cr), Some(cd)) =>
            val sign = getSign(event)
            Some(Connection(Participant(cr.namespace, cr.id), Participant(cd.namespace, cd.id), sign, Seq(text)))

          case _ => None
        }
    }

    val unfilteredConnections = data.collect{ case Some(connection) => connection }

    // Filter out the connections that appear only once
    val counter = unfilteredConnections groupBy identity mapValues (_.size)
    val filteredConnections = counter.filter(_._2 > 1).map(_._1)

    // Store the evidence
    for(con <- filteredConnections){
      if(evidence.contains(con)){
        evidence(con) ++= con.evidence
      }
      else{
        val s = new mutable.HashSet[String]
        s ++= con.evidence
        evidence += (con -> s)
      }
    }


    filteredConnections
  }

  /***
    * Gives back the KBResolution object of an entity or of the controlled reaction down to one element
    * @param arg Value coming from namedArguments from an Event
    * @return KBResolution or None
    */
  def unravelEvent(arg: Option[Seq[Mention]]):Option[KBResolution] =  arg match {
    case Some(a) =>
      val candidate = a.head.asInstanceOf[BioMention]
      // Is it a simple event?
      if(candidate.matches("SimpleEvent")){
        // Get it's theme
        candidate.namedArguments("theme") match {
          case Some(theme) =>
            if(!theme.head.matches("Event"))
              theme.head.asInstanceOf[BioTextBoundMention].grounding()
            else
              None
          case None => None
        }
      }
      else if(!candidate.matches("Event")){
        candidate.grounding()
      }
      else
        None

    case None => None
  }

  /***
    * Computes the sign of the event
    * @param event REACH event
    * @return Sign of the reaction
    */
  def getSign(event: CorefEventMention):Boolean = {

    // If this event is a simple event just read the labels to figure out the sign
    if(event.matches("SimpleEvent")){
      val positiveEvidence = positiveLabels.map(event.matches).reduce((a,b) => a | b)
      val negativeEvidence = negativeLabels.map(event.matches).reduce((a,b) => a | b)

      assert(positiveEvidence != negativeEvidence, "An event can't have positive and negative signs at the same time")
      positiveEvidence // This should be enough because of mutual exclusivity
    }
    else{
      // Then this is a complex event
      val controllerOpt = event.namedArguments("controller")
      val controlledOpt = event.namedArguments("controlled")

      (controllerOpt, controlledOpt) match {
        case(Some(cr), Some(cd)) =>
          val controller = cr.head
          val controlled = cd.head

          // If the particpant is an entity, then give "positive" sign by default, otherwise infer it from the labels
          val crSign = if(controller.matches("Event")) {
            if(controller.matches("Ubiquitination"))
              false
            else if(controller.matches("Deubiquitination"))
              true
            else
              positiveLabels.map(controller.matches).reduce((a, b) => a | b)
          }
          else true
          val cdSign = if(controlled.matches("Event")) {
            if(controlled.matches("Ubiquitination"))
              false
            else if(controlled.matches("Deubiquitination"))
              true
            else
              positiveLabels.map(controlled.matches).reduce((a, b) => a | b)
          }
          else
            true



          // If both participants have the same sign ...
          if(crSign == cdSign){
            // Return positive:
            //  - Positive regulation of a positive simple event is a positive activation
            //  - Negative regulation of a negative simple event is a positive activation by double negation
            true
          }
          // If they have different sign ...
          else {
            // Return positive:
            //  - Negative regulation of a positive simple event is a negative activation
            //  - Positive regulation of a positive simple event is a negative activation
            false
          }
        case _ => false // This case doesn't matter because will be filtered downstream
      }
    }

  }


  override def lucenePlusReach(query: Query) = {
    val pmcids:Iterable[String] = query.strategy match {
      case Singleton => LuceneQueries.singletonQuery(query.A, maxHits)
      case Disjunction => LuceneQueries.binaryDisonjunctionQuery(query.A, query.B.get, maxHits)
      case Conjunction => LuceneQueries.binaryConjunctionQuery(query.A, query.B.get, maxHits)
      case Spatial => LuceneQueries.binarySpatialQuery(query.A, query.B.get, 20, maxHits)
      case Cascade => {
        // TODO: Continue here
        Seq()
      }

    }

    Seq()
  }
}