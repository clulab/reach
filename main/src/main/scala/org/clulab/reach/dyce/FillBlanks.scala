package org.clulab.reach.dyce

import java.nio.file.Paths

import org.clulab.reach.grounding.ReachKBUtils
import org.clulab.reach.mentions.CorefMention

/**
  * Created by enrique on 21/11/16.
  */

case class Participant(val namespace:String, val id:String){
  lazy val synonyms =  FillBlanks.dict.lift(id);
}

case class Connection(val controller:Participant, val controlled:Participant, val sign:Boolean)

class FillBlanks {

  val participantA =  Participant("uniprot", "Q13315") // ATM, Grounding ID of the controller
  val participantB = Participant("uniprot", "P42345") // mTOR, Grounding ID of the controller

  val indexPath = "/data/nlp/corpora/pmc_openaccess/pmc_aug2016_index" // Lucene index path


  // First step, bootstrap the graph by querying individually the participants
  val docsA:Iterable[String] = FillBlanks.queryIndividualParticipant(participantA)
  val docsB :Iterable[String] = FillBlanks.queryIndividualParticipant(participantB)

  // Annotate them
  val paperSet:Set[String] = docsA.toSet & docsB.toSet

  // Extract them
  val activations = FillBlanks.readPapers(paperSet)

  //TODO: Compute overlap with dyce model - Export arizona output and call my python script

  // Build a set of connections out of the extractions
  val connections:Iterable[Connection] = FillBlanks.buildEdges(activations)
  //TODO: Grow the graph


}

object FillBlanks{

  /***
    * Builds edges for the model graph out of raw REACH extractions
    * @param activations REACH evenbts to use
    * @return Iterable of connection instances
    */
  def buildEdges(activations:Iterable[CorefMention]):Iterable[Connection] = {
    Nil
  }

  /***
    * Reads the NXML files and returns events to export and build the graph
    * @param paths Paths to the relevant NXML documents
    * @return The REACH events
    */
  def readPapers(paths: Iterable[String]):Iterable[CorefMention] = {
    Nil
  }


  var lines = ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("uniprot-proteins.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("GO-subcellular-locations.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PubChem.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PFAM-families.tsv.gz")).getLines.toSeq

  val dict = lines.map{ l => val t = l.split("\t"); (t(1), t(0)) }.groupBy(t=> t._1).mapValues(l => l.map(_._2).toSet.toSeq)

  def resolveParticipant(term:String) = {

    dict.lift(term) match {
      case Some(l) => "(" + l.map( x => "\"" + x + "\"").mkString(" OR ") + ")"
      case None =>
        println(s"Warning: missing term in the KB: $term")
        ""
    }
  }

  /***
    * Finds papers that expand the fronteir anchored on this participant
    * @param p Participant to anchor out lucene query
    * @return Iterable with the ids of the papers in the output directory
    */
  def queryIndividualParticipant(p:Participant):Iterable[String] = {
    Nil // TODO: Replace me with an actual path.
  }

  def queryParticipants(a:Participant,b:Participant):Iterable[String] = {
    Nil // TODO: Replace me with an actual path.
  }
}
