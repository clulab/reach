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
  val sentencesMappedToPaperID = collection.mutable.HashMap[String, String]()
  val activeSentences = collection.mutable.ListBuffer[String]()
  for(l <- Source.fromFile(activSentPath).getLines()) {
    val currentSent = l.split("%")(1)
    activeSentences += currentSent
    val paperID = l.split("%")(0)
    sentencesMappedToPaperID ++= Map(currentSent -> paperID)
  }

  //val inhibSentences = Source.fromFile(inhibSentPath).getLines()
  val inhibSentences = collection.mutable.ListBuffer[String]()
  for(l <- Source.fromFile(inhibSentPath).getLines()) {
    val currentSent = l.split("%")(1)
    inhibSentences += currentSent
    val paperID = l.split("%")(0)
    sentencesMappedToPaperID ++= Map(currentSent -> paperID)
  }
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
    val docId = sentencesMappedToPaperID(line)
    val mentions = reachSystem.extractFrom(line, docId, "")
    val eventMentions = mentions.collect{ case bio: BioEventMention => bio}
    eventMentionsFromActivationJSONFile ++= eventMentions
  })

  val eventMentionsFromInhibitionJSONFile = collection.mutable.ListBuffer[BioEventMention]()
  inhibSentences.map(line => {
    val docId = sentencesMappedToPaperID(line)
    val mentions = reachSystem.extractFrom(line, docId, "")
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
    val allContextLabelsInThisEvent = collection.mutable.ListBuffer[String]()
    val pmcidOfCurrentEvent = reFormatDocId(act.document.id)
    val map = act.context match {
      case Some(m) => m
      case None => Map.empty
    }
    for((_, contextLabels) <- map) {
      allContextLabelsInThisEvent ++= contextLabels
      if(act.label.contains("Positive"))
        activeContextLabels ++= contextLabels
      if(act.label.contains("Negative"))
        inhibContextLabels ++= contextLabels
    }

    contextLabelsByPaper ++= Map(pmcidOfCurrentEvent -> allContextLabelsInThisEvent)

  }

  for(act <- inhibEventsWithContext) {
    val allContextLabelsInThisEvent = collection.mutable.ListBuffer[String]()
    val pmcidOfCurrentEvent = reFormatDocId(act.document.id)
    val map = act.context match {
      case Some(m) => m
      case None => Map.empty
    }
    for((_, contextLabels) <- map) {

      allContextLabelsInThisEvent ++= contextLabels
      if(act.label.contains("Positive"))
        activeContextLabels ++= contextLabels
      if(act.label.contains("Negative"))
        inhibContextLabels ++= contextLabels
    }

    contextLabelsByPaper ++= Map(pmcidOfCurrentEvent -> allContextLabelsInThisEvent)
  }




  val bigListOfContextMentions = collection.mutable.ListBuffer[String]()
  bigListOfContextMentions ++= activeContextLabels
  bigListOfContextMentions ++= inhibContextLabels


  println("Printing non-unique labels from activation")
  activeContextLabels.map(println)

  println("Printing non-unique labels from inhibition")
  inhibContextLabels.map(println)

  val intersection = activeContextLabels.toSet.intersect(inhibContextLabels.toSet)
  val activationLabelsNotInIntersection = activeContextLabels.toSet -- intersection
  val inhibitionLabelsNotInIntersection = inhibContextLabels.toSet -- intersection

  println(s"***********Printing intersecting context labels")
  intersection.map(println)
  println(s"***********Done Printing intersecting context labels")

  println(s"Printing unique activation labels that are not in the intersection")
  activationLabelsNotInIntersection.map(println)

  println(s"Printing unique inhibition labels that are not in the intersection")
  inhibitionLabelsNotInIntersection.map(println)

  private def reFormatDocId(id: Option[String]): String = {
    val toReturn = id match {
      case Some(x) => x
      case None => "unknown"
    }
    toReturn
  }

  val freqOfActivationLabelInBigList = collection.mutable.HashMap[String, Int]()
  val freqOfActivationLabelOverPapers = collection.mutable.HashMap[String, (Int, Seq[String])]()
  val freqOfInhibitionLabelInBigList = collection.mutable.HashMap[String, Int]()
  val freqOfInhibitionLabelOverPapers = collection.mutable.HashMap[String, (Int, Seq[String])]()

  for(activ <- activationLabelsNotInIntersection) {
    val freqInBigListOfCtxLabels = PolarityUtils.countOccurrencesOfStringinList(activ, bigListOfContextMentions.toList)
    val freqOverPapers = PolarityUtils.countOccurrencesOfStringInPaper(activ, contextLabelsByPaper.toMap)
    freqOfActivationLabelInBigList ++= Map(activ -> freqInBigListOfCtxLabels)
    freqOfActivationLabelOverPapers ++= Map(activ -> freqOverPapers)
  }

  for(inhib <- inhibitionLabelsNotInIntersection) {
    val freqInBigListOfCtxLabels = PolarityUtils.countOccurrencesOfStringinList(inhib, bigListOfContextMentions.toList)
    val freqOverPapers = PolarityUtils.countOccurrencesOfStringInPaper(inhib, contextLabelsByPaper.toMap)
    freqOfInhibitionLabelInBigList ++= Map(inhib -> freqInBigListOfCtxLabels)
    freqOfInhibitionLabelOverPapers ++= Map(inhib -> freqOverPapers)
  }

  val sortedfreqOfActivationLabelInBigList = ListMap(freqOfActivationLabelInBigList.toSeq.sortWith(_._2 > _._2):_*)
  val sortedfreqOfActivationLabelOverPapers = ListMap(freqOfActivationLabelOverPapers.toSeq.sortWith(_._2._1 > _._2._1):_*)
  println(s"PRINTING FREQUENCY OF ACTIVATION LABELS NOT IN INTERSECTION")
  for((ctxLabel, freq) <- sortedfreqOfActivationLabelInBigList) {
    println(s"The activation context label ${ctxLabel} appears ${freq} times in the list of all context mentions (not including intersection)")
  }

  for((ctxLabel, freq) <- sortedfreqOfActivationLabelOverPapers) {
    println(s"The activation context label ${ctxLabel} appears in ${freq} out of ${contextLabelsByPaper.size} papers")
  }

  println("*****************************************")
  val sortedfreqOfInhibitionLabelInBigList = ListMap(freqOfInhibitionLabelInBigList.toSeq.sortWith(_._2 > _._2):_*)
  val sortedfreqOfInhibitionLabelOverPapers = ListMap(freqOfInhibitionLabelOverPapers.toSeq.sortWith(_._2._1 > _._2._1):_*)

  println(s"PRINTING FREQUENCY OF INHIBITION LABELS NOT IN INTERSECTION")
  println(s"There are ${inhibitionLabelsNotInIntersection.size} unique types of context in the inhibition set that are not in the intersection")
  for((ctxLabel, freq) <- sortedfreqOfInhibitionLabelInBigList) {
    println(s"The inhibition context label ${ctxLabel} appears ${freq} times in the list of all context mentions (not including intersection)")
  }
  for((ctxLabel, freq) <- sortedfreqOfInhibitionLabelOverPapers) {
    println(s"The inhibition context label ${ctxLabel} appears in ${freq} out of ${contextLabelsByPaper.size} papers")
  }


}
