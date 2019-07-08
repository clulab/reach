package org.clulab.reach.context.context_exec

import java.io.{File, PrintWriter}

import ai.lum.nxmlreader.NxmlReader
import com.typesafe.config.ConfigFactory
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.context.context_utils.PolarityUtils
import org.clulab.reach.mentions.BioEventMention

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.io.Source

object Polarity extends App {
  val config = ConfigFactory.load()
  val activSentPath = config.getString("polarityContext.genericFileDir").concat("activation_sentences_in_json.txt")
  val inhibSentPath = config.getString("polarityContext.genericFileDir").concat("inhibition_sentences_in_json.txt")
  //val activSentences = Source.fromFile(activSentPath).getLines()
  val activeSentences = collection.mutable.ListBuffer[String]()
  for(l <- Source.fromFile(activSentPath).getLines()) activeSentences += l

  //val inhibSentences = Source.fromFile(inhibSentPath).getLines()
  val inhibSentences = collection.mutable.ListBuffer[String]()
  for(l <- Source.fromFile(inhibSentPath).getLines()) inhibSentences += l
  println(activeSentences.size + ": number of text evidences from activation JSON")
  println(inhibSentences.size + ": number of text evidences from inhibition JSON")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val sentenceWindow = config.getString("contextEngine.params.bound")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
  //val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml")
  val fullPapers = List("PMC2958340.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml",  "PMC2686753.nxml","PMC1590014.nxml", "PMC1849968.nxml", "PMC2424011.nxml", "PMC2847694.nxml")
  //val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml", "PMC1590014.nxml")

  val fileListUnfiltered = new File(dirForType)
  val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml") && (fullPapers.contains(x.getName)))
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)

  val eventMentionsFromActivationJSONFile = collection.mutable.ListBuffer[BioEventMention]()
  activeSentences.map(line => {
    val mentions = reachSystem.extractFrom(line, "", "")
    val eventMentions = mentions.collect{ case bio: BioEventMention => bio}
    eventMentionsFromActivationJSONFile ++= eventMentions
  })

  val eventMentionsFromInhibitionJSONFile = collection.mutable.ListBuffer[BioEventMention]()
  inhibSentences.map(line => {
    val mentions = reachSystem.extractFrom(line, "", "")
    val eventMentions = mentions.collect{ case bio: BioEventMention => bio}
    eventMentionsFromInhibitionJSONFile ++= eventMentions
  })

  val activeEventsWithContext = eventMentionsFromActivationJSONFile.filter(_.hasContext()).toSet
  val inhibEventsWithContext = eventMentionsFromInhibitionJSONFile.filter(_.hasContext()).toSet

  println(activeEventsWithContext.size + " : number of unique events that have context labels in activation")
  println(inhibEventsWithContext.size + " : Number of unique events that have context labels in inhibition")

  val activeContextLabels = collection.mutable.ListBuffer[String]()
  val inhibContextLabels = collection.mutable.ListBuffer[String]()
  val contextLabelsByPaper = collection.mutable.HashMap[String, Seq[String]]()
  for(act <- activeEventsWithContext) {
    val map = act.context match {
      case Some(m) => m
      case None => Map.empty
    }

    for((_, contextLabels) <- map) {
      if(act.label.contains("Positive"))
        activeContextLabels ++= contextLabels
      else if(act.label.contains("Negative"))
        inhibContextLabels ++= contextLabels
    }

  }

  for(act <- inhibEventsWithContext) {
    val map = act.context match {
      case Some(m) => m
      case None => Map.empty
    }

    for((_, contextLabels) <- map) {
      if(act.label.contains("Positive"))
        activeContextLabels ++= contextLabels
      else if(act.label.contains("Negative"))
        inhibContextLabels ++= contextLabels
    }
  }




  val bigListOfContextMentions = collection.mutable.ListBuffer[String]()
  bigListOfContextMentions ++= activeContextLabels
  bigListOfContextMentions ++= inhibContextLabels

  val intersection = activeContextLabels.toSet.intersect(inhibContextLabels.toSet)
  val activationLabelsNotInIntersection = activeContextLabels.toSet -- intersection
  val inhibitionLabelsNotInIntersection = inhibContextLabels.toSet -- intersection
  println(intersection.size)

  println(s"Printing unique activation labels that are not in the intersection")
  activationLabelsNotInIntersection.map(println)

  println(s"Printing unique inhibition labels that are not in the intersection")
  inhibitionLabelsNotInIntersection.map(println)

  private def reFormatDocId(id: Option[String]): String = {
    val toReturn = id match {
      case Some(x) => s"PMC${x.split("_")(0)}"
      case None => "unknown"
    }
    toReturn
  }


}
