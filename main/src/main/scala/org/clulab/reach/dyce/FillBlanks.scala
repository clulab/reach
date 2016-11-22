package org.clulab.reach.dyce

import collection.mutable
import org.clulab.reach.grounding.ReachKBUtils
import org.clulab.reach.mentions.CorefMention
import org.clulab.struct.DirectedGraph

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

  val annotationsRecord = mutable.Set[String]()

  val indexPath = "/data/nlp/corpora/pmc_openaccess/pmc_aug2016_index" // Lucene index path

  var G:Option[DirectedGraph[Participant]] = None // Directed graph with the model. It is a mutable variable as it will change each step


  // First step, bootstrap the graph by querying individually the participants
  val docsA:Iterable[String] = FillBlanks.queryIndividualParticipant(participantA)
  val docsB :Iterable[String] = FillBlanks.queryIndividualParticipant(participantB)

  // Join them
  val paperSet:Set[String] = docsA.toSet & docsB.toSet

  // Add them to the annotations record
  annotationsRecord ++= paperSet

  // Extract them
  val activations = FillBlanks.readPapers(paperSet)

  //TODO: Compute overlap with dyce model - Export arizona output and call my python script or reimplement here for efficiency

  // Build a set of connections out of the extractions
  val connections:Iterable[Connection] = FillBlanks.buildEdges(activations)
  //Grow the graph
  G = Some(FillBlanks.expandGraph(G, connections))

  // Loop of iterative steps of expanding the graph
  var stop = false
  while(!stop){ // TODO: Replace the logical statement with something that breaks
    // Look for a path between participants A and B
    val path = FillBlanks.findPath(G.get, participantA, participantB)

    path match {
      case Some(p) =>
        // TODO: Report the result
        stop = true
      case None => Unit
    }

    if(!stop) {
      val frontierA = FillBlanks.findFrontier(G.get, participantA)
      val frontierB = FillBlanks.findFrontier(G.get, participantB)


      // TODO: make sure to expand this cross product to include more nodes if necessary (like the original participant)
      val pairs = for {l <- frontierA; r <- frontierB} yield (l, r) // These are the pairs to expand our search

      // Query the index to find the new papers to annotate
      val allDocs = pairs flatMap (p => FillBlanks.queryParticipants(p._1, p._2))

      // Filter out those papers that have been already annotated
      val docs = allDocs filter {
        d => !annotationsRecord.contains(d)
      }

      // Add the papers to the record to avoid annotating them later
      annotationsRecord ++= docs

      // Annotate the new papers
      val activations = FillBlanks.readPapers(docs)

      //TODO: Compute overlap with dyce model - Export arizona output and call my python script or reimplement here for efficiency

      // Build a set of connections out of the extractions
      val connections:Iterable[Connection] = FillBlanks.buildEdges(activations)
      //Grow the graph
      G = Some(FillBlanks.expandGraph(G, connections))
    }
  }


}

object FillBlanks{

  /***
    * Searches for a path between the participants in the graph
    * @param G Model graph
    * @param participantA Source of the path
    * @param participantB Sink of the path
    * @return Some sequence if the path exists, otherwise None
    */
  def findPath(G: DirectedGraph[Participant], participantA: Participant
               , participantB: Participant): Option[Seq[Participant]] =
  {
    None
  }


  /***
    * Finds the search frontier in the graph relative to participant
    *
    * @param G The model
    * @param participant The participant of concern
    * @return Iterable of participants that make the frontier
    */
  def findFrontier(G:DirectedGraph[Participant], participant: Participant):Iterable[Participant] = {
    Nil
  }


  /***
    * Creates a new graph with the connections of the existing graph and new connections from the second argument
    *
    * @param G existing graph, if any, to be expanded
    * @param connections New information to incorporate to the graph
    * @return Graph with the new information added
    */
  def expandGraph(G: Option[DirectedGraph[Participant]], connections: Iterable[Connection]): DirectedGraph[Participant] = {
    G.get
  }


  /***
    * Builds edges for the model graph out of raw REACH extractions
    *
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
