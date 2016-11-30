package org.clulab.reach.dyce

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.QueryParserBase
import org.clulab.odin.{EventMention, Mention}
import org.clulab.reach.PaperReader
import org.clulab.reach.grounding.{KBResolution, ReachKBUtils}
import org.clulab.reach.indexer.NxmlSearcher
import org.clulab.reach.mentions.serialization.json.{JSONSerializer, REACHMentionSeq}
import org.clulab.reach.mentions.{BioMention, BioTextBoundMention, CorefEventMention, CorefMention, MentionOps}
import org.clulab.utils.Serializer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph // shortcuts

/**
  * Created by enrique on 21/11/16.
  */

case class Participant(namespace:String, id:String){
  lazy val synonyms =  FillBlanks.dict.lift(id);

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Participant]

  override def equals(obj: Any): Boolean = obj match {
    case that:Participant => (this.namespace == that.namespace) && (this.id == that.id)
    case _ => false
  }

  override def hashCode(): Int = (this.id + this.namespace).hashCode

  override def toString: String = s"$namespace:$id"
}

case class Connection(controller:Participant, controlled:Participant, sign:Boolean){

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Connection]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that:Connection => (this.controller== that.controller) && (this.controlled == that.controlled) && this.sign == that.sign
    case _ => false
  }

  override def hashCode(): Int = s"${controller.toString}:${controlled.toString}:$sign".hashCode

  override def toString: String = s"Controller: $controller - Controlled: $controlled - Sign: $sign"

}

object FillBlanks extends App with LazyLogging{
  var initialized = false // Flag to indicate whether reach has been initialized

  val positiveLabels = Vector("Positive_regulation", "Positive_activation", "IncreaseAmount", "AdditionEvent")
  val negativeLabels = Vector("Negative_regulation", "Negative_activation", "DecreaseAmount", "RemovalEvent", "Translocation")

  logger.info("Loading KBs...")
  var lines = ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("uniprot-proteins.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("GO-subcellular-locations.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PubChem.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PFAM-families.tsv.gz")).getLines.toSeq

  val dict = lines.map{ l => val t = l.split("\t"); (t(1), t(0)) }.groupBy(t=> t._1).mapValues(l => l.map(_._2).distinct)

  val indexDir = "/data/nlp/corpora/pmc_openaccess/pmc_aug2016_index"
  val nxmlSearcher:NxmlSearcher = new NxmlSearcher(indexDir)


  val totalHits = 100 // Max # of hits per query
  logger.info(s"Max hits for retrieval: $totalHits")

  val participantA =  Participant("uniprot", "Q13315") // ATM, Grounding ID of the controller
  val participantB = Participant("uniprot", "P42345") // mTOR, Grounding ID of the controller


  val nxmlDir = "/work/enoriega/fillblanks/nxml"
  val reachOutputDir = "/work/enoriega/fillblanks/annotations"

  logger.info("Loading lucene record...")
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
  logger.info("Loading existing annotations")
  val (annotationsRecord, annotationsCache) = FillBlanks.loadExtractions(reachOutputDir)


  val G:Graph[Participant, LDiEdge] = Graph[Participant, LDiEdge]() // Directed graph with the model.

  logger.info(s"Bootstraping step: Retrieving docs for the target participants ...")

  //// Focused query
  val docs:Iterable[String] = queryParticipants(participantA, participantB)
  val paperSet = docs.map(p => new File(nxmlDir, s"$p.nxml").getAbsolutePath)
  logger.info(s"Query returned ${docs.size} hits")

  // Extract them
  logger.info("Reading retrieved papers ...")
  val activations = readPapers(paperSet)
  logger.info("Finished reading papers")

  // Add them to the annotations record
  annotationsRecord ++= paperSet

  //TODO: Compute overlap with dyce model - Export arizona output and call my python script or reimplement here for efficiency

  // Build a set of connections out of the extractions
  logger.info("Growing the model with results ...")
  val connections:Iterable[Connection] = buildEdges(activations)
  logger.info(s"Extracted ${connections.size} connections")
  //Grow the graph
  expandGraph(connections)
  logger.info("Done growing the model")

  // Loop of iterative steps of expanding the graph
  logger.info("Starting iterative phase...")
  var stop = false


  while(!stop){
    // Look for a path between participants A and B
    logger.info("Looking for a path between the anchors ...")
    val path = findPath(participantA, participantB)

    path match {
      case Some(p) =>
        // TODO: Report the result
        stop = true
        logger.info("Path found!! Stopping")
      case None => Unit
    }

    if(!stop) {
      logger.info("Path not found. Expanding the frontier...")

      // Get the connected components of the model graph
      val components = getConnectedComponents

      // Make the pairs to query the index
      val pairs = crossProduct(components.toSet)

      // Query the index to find the new papers to annotate
      logger.info("Retrieving papers to build a path...")
      val allDocs = pairs flatMap (p => queryParticipants(p._1, p._2))
      logger.info(s"Query returned ${allDocs.size} hits")

      // Filter out those papers that have been already annotated
      val docs = allDocs diff annotationsRecord

      val paperSet = docs.map(p => new File(nxmlDir, s"$p.nxml").getAbsolutePath)

      // Add the papers to the record to avoid annotating them later
      annotationsRecord ++= paperSet

      // Annotate the new papers
      logger.info("Annotating papers ...")
      val activations = readPapers(paperSet)
      logger.info("Finished reading papers")

      //TODO: Compute overlap with dyce model - Export arizona output and call my python script or reimplement here for efficiency

      // Build a set of connections out of the extractions
      logger.info("Growing the model with results ...")
      val connections:Iterable[Connection] = buildEdges(activations)
      //Grow the graph
      expandGraph(connections)
      logger.info("Done growing the model")
    }
  }
  logger.info("Finished iterative phase")


  /***
    * Computes the pairs of entities in the connected components to be queried by lucene
    * @param components Connected components of the model graph
    * @return The pairs to be queried
    */
  def crossProduct(components: Set[Set[Participant]]):Set[(Participant, Participant)] = {
    val pairs = components flatMap {
      current =>
        val others = (components - current).flatten
        for{
          c <- current
          o <- others
        } yield (c, o)
    }

    pairs
  }

  /***
    * Gets the sets of nodes in the connected components
    * @return Iterable with a set of nodes for each connected component
    */
  def getConnectedComponents:Iterable[Set[Participant]] = {
    G.componentTraverser().map{
      c => c.nodes.map(_.value)
    }.toSeq
  }

  /***
    * Searches for a path between the participants in the graph
    * @param participantA Source of the pathŒ∏
    * @param participantB Sink of the path
    * @return Some sequence if the path exists, otherwise None
    */
  def findPath(participantA: Participant, participantB: Participant): Option[Seq[Connection]] =
  {
    (G find participantA, G find participantB) match {
      case (Some(pa), Some(pb)) =>
        pa shortestPathTo pb match{
          case Some(path) => Some{
            path.edges.map{
              e => Connection(e.source, e.target, e.label.value.asInstanceOf[Boolean])
            }.toSeq
          }
          case None => None
        }
      case _ => None
    }
  }


  /***
    * Creates a new graph with the connections of the existing graph and new connections from the second argument
    *
    * @param connections New information to incorporate to the graph
    */
  def expandGraph(connections: Iterable[Connection]){
    // How large was the graph before?
    val prevNodesCount = this.G.nodes.size
    val prevEdgesCount = this.G.edges.size
    // Make labeled directed edges out of each connection
    val edges = connections map {c => (c.controller ~+> c.controlled)(c.sign)}
    // Add them to the graph
    this.G ++= edges
    // How large is it now?
    val nodesCount = this.G.nodes.size
    val edgesCount = this.G.edges.size

    logger.info(s"Model participants; Before: $prevNodesCount\tAfter: $nodesCount")
    logger.info(s"Model connections; Before: $prevEdgesCount\tAfter: $edgesCount")
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

  /***
    * Builds edges for the model graph out of raw REACH extractions
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

        (controller, controlled) match {
          case (Some(cr), Some(cd)) =>
            val sign = getSign(event)
            Some(Connection(Participant(cr.namespace, cr.id), Participant(cd.namespace, cd.id), sign))

          case _ => None
        }
    }

    data.collect{ case Some(connection) => connection }
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

    logger.info(s"Serializing annotations of $id...")

    // Serialize the mentions to json
    //val json = ann.jsonAST

    // Write them to disk
    val file = new File(dir, "mentions.json")
    REACHMentionSeq(ann).saveJSON(file, pretty = true)
    //FileUtils.writeStringToFile(file, compact(render(json)))
    //Serializer.save[Seq[CorefMention]](ann, file.getAbsolutePath)
  }

  private def nsToS (startNS:Long, endNS:Long): Long = (endNS - startNS) / 1000000000L

  /***
    * Reads the NXML files and returns events to export and build the graph
    *
    * @param paths Paths to the relevant NXML documents
    * @return The REACH events
    */
  def readPapers(paths: Iterable[String]):Iterable[CorefMention] = {

    // Find the mentions that are already in the cache
    val existing = paths.map{
      // Get their id from the path
      p => p.split("/").last.split("\\.")(0)
    }.filter(annotationsRecord.contains)

    val nonExisting = paths.filter{p => val i = p.split("/").last.split("\\.")(0); !annotationsRecord.contains(i)}

    // Fetch the annotations from the existing cache
    val existingAnnotations = existing flatMap getExistingAnnotations

    // Annotate the papers that haven't been so
    logger.info(s"${nonExisting.size} papers to annotate ...")
    if(nonExisting.nonEmpty){
      // Initialize the reach system if necessary
      if(!this.initialized){
        val _ = PaperReader.rs.extractFrom("Blah", "", "")
        this.initialized = true
      }
    }
    val newAnnotations:Seq[(String, Seq[CorefMention])] = {
      nonExisting.par.map{
        p =>
          val f = new File(p)
          val startNS = System.nanoTime
          logger.info(s"$p: starting reading")
          val (id, mentions) = PaperReader.readPaper(f)
          logger.info(s"Finished annotating $p")
          logger.info(s"${nsToS(startNS, System.nanoTime)}s: $p")

          // Keep only the event mentions and cast to coref mention
          val ann = mentions.collect{ case e:EventMention => e}.map(m => MentionOps(m).toCorefMention)
          // Serializing annotations
          try {
            serializeAnnotations(id, ann)
          }catch{
            case e:Exception =>
              logger.error(e.getMessage)
              logger.error(e.toString)
          }

          (id, ann)
      }.toSeq.seq
    }


    existingAnnotations ++ newAnnotations.flatMap(_._2)
  }

  def getExistingAnnotations(id:String):Iterable[CorefMention] = {
    // If they're loaded return them
    annotationsCache.lift(id) match {
      case Some(a) => a
      case None =>
        // Load the annotations from disk if they exist
        val file = new File(new File(reachOutputDir, id), "mentions.json")
        val mentions = if(file.exists){
          JSONSerializer.toCorefMentions(file)
        }
        else
          Nil
        // Add them to the cache
        annotationsCache += (id -> mentions)
        mentions
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

    var luceneQuery = QueryParserBase.escape("(" + aSynonyms + " AND " + bSynonyms + ")~20")
    var hits = FillBlanks.nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants
    if(hits.nonEmpty)
      // Returns the seq with the ids to annotate
      fetchHitsWithCache(hits)
    else{
      luceneQuery = QueryParserBase.escape("(" + aSynonyms + " AND  " + bSynonyms + ")")
      hits = FillBlanks.nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits)
      if(hits.nonEmpty)
        fetchHitsWithCache(hits)
      else{
        luceneQuery = QueryParserBase.escape("(" + aSynonyms + " OR  " + bSynonyms + ")")
        hits = FillBlanks.nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits)
        fetchHitsWithCache(hits)
      }
    }
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

  /***
    * Loads REACH extractions from the disk
    * @param path Directory where they are stored
    * @return
    */
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
              //val mentions = JSONSerializer.toCorefMentions(m)
              //val mentions = Serializer.load[Seq[CorefMention]](m.getAbsolutePath)
              //cache += (id -> mentions)
            }catch {
              case e:Exception =>
                logger.error(e.getMessage)
            }
          }
        }
      }
    }

    (record, cache)
  }
}
