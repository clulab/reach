package org.clulab.reach.dyce

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.{EventMention, Mention}
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json.REACHMentionSeq
import org.clulab.reach.PaperReader
import org.clulab.reach.mentions.{BioMention, BioTextBoundMention, CorefEventMention, CorefMention}
import org.clulab.reach.mentions.serialization.json.JSONSerializer

import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Created by enrique on 20/02/17.
  */
trait IEStrategy extends LazyLogging {
  def informationExtraction(pmcids: Iterable[String]):Iterable[Connection]
}

trait REACHIEStrategy extends IEStrategy {

  val reachOutputDir = "/work/enoriega/fillblanks/annotations"
  val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(20))
  var initialized = false // Flag to indicate whether reach has been initialized
  val positiveLabels = Vector("Positive_regulation", "Positive_activation", "IncreaseAmount", "AdditionEvent")
  val negativeLabels = Vector("Negative_regulation", "Negative_activation", "DecreaseAmount", "RemovalEvent", "Translocation")
  val evidence = new mutable.HashMap[Connection, mutable.Set[String]]()

  private def nsToS (startNS:Long, endNS:Long): Long = (endNS - startNS) / 1000000000L

  // Load the existing annotations
  logger.info("Loading existing annotations")
  val (annotationsRecord, annotationsCache) = loadExtractions(reachOutputDir)

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
      val parNonExisting = nonExisting.par
      parNonExisting.tasksupport = taskSupport
      parNonExisting.par.map{
        p =>
          val f = new File(p)
          val startNS = System.nanoTime
          logger.info(s"$p: starting reading")
          val (id, mentions) = PaperReader.readPaper(f)
          logger.info(s"${nsToS(startNS, System.nanoTime)}s Finished annotating $p")

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

  override def informationExtraction(pmcids: Iterable[String]) = {

    val paperSet = pmcids.map(p => new File(LuceneQueries.nxmlDir, s"$p.nxml").getAbsolutePath)
    logger.info(s"Query returned ${paperSet.size} hits")

    // Extract them
    logger.info("Reading retrieved papers ...")
    val activations = readPapers(paperSet)
    logger.info("Finished reading papers")

    // Add them to the annotations record
    annotationsRecord ++= pmcids

    val connections:Iterable[Connection] = buildEdges(activations)
    logger.info(s"Extracted ${connections.size} connections")
    connections
  }
}
