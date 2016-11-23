package org.clulab.reach.dyce

import java.nio.file.Paths
import java.io.File

import org.apache.commons.io.FileUtils
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.{QueryParser, QueryParserBase}
import org.apache.lucene.search.{IndexSearcher, TopScoreDocCollector}
import org.apache.lucene.store.FSDirectory
import org.clulab.reach.PaperReader
import org.slf4j.LoggerFactory
import org.clulab.reach.mentions.MentionOps

import collection.mutable
import org.clulab.utils.Serializer
import org.clulab.reach.grounding.ReachKBUtils
import org.clulab.reach.indexer.NxmlSearcher
import org.clulab.reach.mentions.CorefMention
import org.clulab.struct.DirectedGraph
import org.clulab.reach.PaperReader

import scala.collection.mutable.ListBuffer
import org.json4s._
import org.json4s.native.JsonMethods._
import org.clulab.serialization.json._

/**
  * Created by enrique on 21/11/16.
  */

case class Participant(val namespace:String, val id:String){
  lazy val synonyms =  FillBlanks.dict.lift(id);
}

case class Connection(val controller:Participant, val controlled:Participant, val sign:Boolean)

class FillBlanks {

  val totalHits = 500 // Max # of hits per query

  val participantA =  Participant("uniprot", "Q13315") // ATM, Grounding ID of the controller
  val participantB = Participant("uniprot", "P42345") // mTOR, Grounding ID of the controller


  val nxmlDir = "/home/enoriega/fillblanks/nxml"
  val reachOutputDir = "/home/enoriega/fillblanks/annotations"

  //lazy val reachSystem =

  // Load the serialized record if exists, otherwise create a new one
  val ldcFile = new File(nxmlDir, "luceneDocRecord.ser")
  val luceneDocRecord = if(ldcFile.exists()){
    Serializer.load[mutable.HashMap[Int, String]](ldcFile.getAbsolutePath)
  }
  else{
    mutable.HashMap[Int, String]()
  }
  ///////////////////////


  // Load the existing annotations
  val (annotationsRecord, annotationsCache) = FillBlanks.loadExtractions(reachOutputDir)


  var G:Option[DirectedGraph[Participant]] = None // Directed graph with the model. It is a mutable variable as it will change each step


  // First step, bootstrap the graph by querying individually the participants
  val docsA:Iterable[String] = queryIndividualParticipant(participantA)
  val docsB :Iterable[String] = queryIndividualParticipant(participantB)

  // Join them
  val paperSet:Set[String] = docsA.toSet & docsB.toSet

  // Add them to the annotations record
  annotationsRecord ++= paperSet

  // Extract them
  val activations = readPapers(paperSet) // TODO: Make sure the parameter has the right form

  //TODO: Compute overlap with dyce model - Export arizona output and call my python script or reimplement here for efficiency

  // Build a set of connections out of the extractions
  val connections:Iterable[Connection] = buildEdges(activations)
  //Grow the graph
  G = Some(expandGraph(G, connections))

  // Loop of iterative steps of expanding the graph
  var stop = false
  while(!stop){
    // Look for a path between participants A and B
    val path = findPath(G.get, participantA, participantB)

    path match {
      case Some(p) =>
        // TODO: Report the result
        stop = true
      case None => Unit
    }

    if(!stop) {
      val frontierA = findFrontier(G.get, participantA)
      val frontierB = findFrontier(G.get, participantB)


      // TODO: make sure to expand this cross product to include more nodes if necessary (like the original participant)
      val pairs = for {l <- frontierA; r <- frontierB} yield (l, r) // These are the pairs to expand our search

      // Query the index to find the new papers to annotate
      val allDocs = pairs flatMap (p => queryParticipants(p._1, p._2))

      // Filter out those papers that have been already annotated
      val docs = allDocs filter {
        d => !annotationsRecord.contains(d)
      }

      // Add the papers to the record to avoid annotating them later
      annotationsRecord ++= docs

      // Annotate the new papers
      val activations = readPapers(docs) // TODO: Make sure the parameter has the right form

      //TODO: Compute overlap with dyce model - Export arizona output and call my python script or reimplement here for efficiency

      // Build a set of connections out of the extractions
      val connections:Iterable[Connection] = buildEdges(activations)
      //Grow the graph
      G = Some(expandGraph(G, connections))
    }
  }


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
    * Serializes the relevant annotations to disk to avoid making them again
    * @param id Name of the paper
    * @param ann Mentions to save
    */
  def serializeAnnotations(id: String, ann: Seq[CorefMention]): Unit ={
    // Create the output dir
    val dir = new File(reachOutputDir, id)
    if(!dir.exists){
      dir.mkdirs()
    }

    // Serialize the mentions to json
    val json = ann.jsonAST

    // Write them to disk
    val file = new File(dir, "mentions.json")
    FileUtils.writeStringToFile(file, compact(render(json)))
  }

  /***
    * Reads the NXML files and returns events to export and build the graph
    *
    * @param paths Paths to the relevant NXML documents
    * @return The REACH events
    */
  def readPapers(paths: Iterable[String]):Iterable[CorefMention] = {

    // Find the mentions that are already in the cache
    val existing = paths.filter(annotationsRecord.contains)
    val nonExisting = paths.filter(p => !annotationsRecord.contains(p))

    // Fetch the annotations from the existing cache
    val existingAnnotations = existing flatMap getExistingAnnotations

    // Annotate the papers that haven't been so
    val newAnnotations:Seq[(String, Seq[CorefMention])] = {
      nonExisting.map{
        p =>
        val f = new File(p)
        val (id, mentions) = PaperReader.readPaper(f)
          (id, mentions.map(m => MentionOps(m).toCorefMention))
      }.toSeq.seq
    }

    // Add the new annotations to the cache here and also store them on disk
    for((id, ann) <- newAnnotations){
      annotationsCache += (id -> ann)
      try {
        serializeAnnotations(id, ann)
      }catch{
        case e:Exception => Unit // TODO: Add an alert here
      }
    }

    existingAnnotations ++ newAnnotations.flatMap(_._2)
  }

  def getExistingAnnotations(id:String):Iterable[CorefMention] = {
    // If they're loaded return them
    annotationsCache.lift(id) match {
      case Some(a) => a
      case None => Nil
    }
  }

  /***
    * Gets the synonyms from the KB files
    * @param term Grounding ID without namespace to look for
    * @return String with the disjunction of synonyms ready to be queried by lucene
    */
  def resolveParticipant(term:String) = {

    FillBlanks.dict.lift(term) match {
      case Some(l) => "(" + l.map( x => "\"" + x + "\"").mkString(" OR ") + ")"
      case None =>
        println(s"Warning: missing term in the KB: $term")
        ""
    }
  }

  /***
    * Finds papers that expand the frontier anchored on this participant
    * @param p Participant to anchor out lucene query
    * @return Iterable with the ids of the papers in the output directory
    */
  def queryIndividualParticipant(p:Participant):Iterable[String] = {

    // Build a query for lucene
    val luceneQuery = resolveParticipant(p.id)
    val hits = FillBlanks.nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    // Returns the seq with the ids to annotate
    fetchHitsWithCache(hits)

  }

  /***
    * Expands the frontier with a focus on finding info that may create a path between participants
    * @param a Participant A
    * @param b Participant B
    * @return
    */
  def queryParticipants(a:Participant, b:Participant):Iterable[String] = {
    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    val luceneQuery = "(" + aSynonyms + ") AND  (" + bSynonyms + ")~20"
    val hits = FillBlanks.nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    // Returns the seq with the ids to annotate
    fetchHitsWithCache(hits)
  }

  /***
    * Retrieves documents from lucene. If they have already been retrieved don't do it agaib
    * @param hits Set of documents coming from NxmlSearcher
    * @return list with the ids of documents already fetched from the index
    */
  def fetchHitsWithCache(hits: Set[(Int, Float)]): List[String] = {
    // Hits are tuples with (docId, score), fetch the documents from the ids if they haven't been fetched before
    val existing = new ListBuffer[String]
    val toFetch = new ListBuffer[(Int, Float)]

    for (record <- hits) {
      if (luceneDocRecord contains record._1) {
        // Get the IDs from the record
        existing += luceneDocRecord(record._1)
      }
      else {
        // Mark them for retrieval
        toFetch += record
      }
    }

    val tfs = toFetch.toSet
    // Fetch the Document objects
    val docs = FillBlanks.nxmlSearcher.docs(tfs)
    val newPapers = docs.toSeq.sortBy(-_._2).map(d => d._1.get("id"))

    // Save them to disk
    FillBlanks.nxmlSearcher.saveNxml(nxmlDir, docs)

    // Add them to the record
    for ((t, d) <- toFetch.sortBy(-_._2) zip newPapers) {
      luceneDocRecord += (t._1 -> d)
    }

    // Reserialize the record
    Serializer.save[mutable.HashMap[Int, String]](luceneDocRecord, ldcFile.getAbsolutePath)

    existing.toList ++ newPapers
  }

}

object FillBlanks{
  val indexDir = "/data/nlp/corpora/pmc_openaccess/pmc_aug2016_index"
  val nxmlSearcher = new NxmlSearcher(indexDir)

  var lines = ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("uniprot-proteins.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("GO-subcellular-locations.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PubChem.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PFAM-families.tsv.gz")).getLines.toSeq

  val dict = lines.map{ l => val t = l.split("\t"); (t(1), t(0)) }.groupBy(t=> t._1).mapValues(l => l.map(_._2).toSet.toSeq)

  // Lucene index path
  val reader = DirectoryReader.open(FSDirectory.open(Paths.get(indexDir)))
  val searcher = new IndexSearcher(reader)

  def loadExtractions(path:String):(mutable.Set[String], mutable.HashMap[String, Iterable[CorefMention]]) = {
    val record = mutable.Set[String]()
    val cache = mutable.HashMap[String, Iterable[CorefMention]]()

    val dir = new File(path)

    // If the directory exists, populate the data structures
    if(dir.exists){
      // Every directory contains a mentions.json file
      for(d <- dir.listFiles){
        if(d.isDirectory){
          val m = new File(d, "mentions.json")
          if(m.exists){
            // Add the paper to the record
            val id = d.getName
            record += id
            // Deserialize the mentions and add them to the cache
            try{
              val mentions = JSONSerializer.toMentions(m).map(x => MentionOps(x).toCorefMention)
              cache += (id -> mentions)
            }catch {
              case _:Exception => Unit // TODO: add an alert here
            }
          }
        }
      }
    }

    (record, cache)
  }
}
