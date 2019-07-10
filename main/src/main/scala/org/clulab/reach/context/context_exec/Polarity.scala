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

  println(s"After adding all events from activation events: ${sentencesMappedToPaperID.size}")

  //val inhibSentences = Source.fromFile(inhibSentPath).getLines()
  val inhibSentences = collection.mutable.ListBuffer[String]()
  for(l <- Source.fromFile(inhibSentPath).getLines()) {
    val currentSent = l.split("%")(1)
    inhibSentences += currentSent
    val paperID = l.split("%")(0)
    sentencesMappedToPaperID ++= Map(currentSent -> paperID)
  }
  println(s"After adding all events from inhibition events: ${sentencesMappedToPaperID.size}")
  println(activeSentences.size + ": number of text evidences from activation JSON")
  println(inhibSentences.size + ": number of text evidences from inhibition JSON")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  //val sentenceWindow = config.getString("contextEngine.params.bound")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
  //val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml")
  val fullPapers = List("PMC4497359.nxml","PMC4463612.nxml","PMC4461195.nxml","PMC4460026.nxml","PMC4449203.nxml","PMC2958340.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml",  "PMC2686753.nxml","PMC1590014.nxml", "PMC1849968.nxml", "PMC2424011.nxml", "PMC2847694.nxml", "PMC3104995.nxml", "PMC3411611.nxml", "PMC3423535.nxml", "PMC3469438.nxml", "PMC3608085.nxml")
  //val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml", "PMC1590014.nxml")

  //val fileListUnfiltered = new File(dirForType)
  //val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml") && (fullPapers.contains(x.getName)))
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)

  val eventMentionsByPaper = collection.mutable.HashMap[String, Seq[BioEventMention]]()

  val eventMentionsFromActivationJSONFile = collection.mutable.ListBuffer[BioEventMention]()
  val uniqueActivationSent = activeSentences.toSet
  uniqueActivationSent.map(line => {
    val docId = sentencesMappedToPaperID(line)
    val mentions = reachSystem.extractFrom(line, docId, "")
    val eventMentions = mentions.collect{ case bio: BioEventMention => bio}
    eventMentionsFromActivationJSONFile ++= eventMentions
    eventMentionsByPaper ++= Map(docId -> eventMentions)
  })

  val eventMentionsFromInhibitionJSONFile = collection.mutable.ListBuffer[BioEventMention]()
  val uniqueInhibSent = inhibSentences.toSet
  uniqueInhibSent.map(line => {
    val docId = sentencesMappedToPaperID(line)
    val mentions = reachSystem.extractFrom(line, docId, "")
    val eventMentions = mentions.collect{ case bio: BioEventMention => bio}
    eventMentionsFromInhibitionJSONFile ++= eventMentions
    eventMentionsByPaper ++= Map(docId -> eventMentions)
  })
  println(s"Before filtering out non-context events, I had a total of ${eventMentionsFromActivationJSONFile.size} activation events")
  println(s"Before filtering out non-context events, I had a total of ${eventMentionsFromInhibitionJSONFile.size} inhibition events")
  val activeEventsWithContext = eventMentionsFromActivationJSONFile.filter(_.hasContext()).toSet
  val inhibEventsWithContext = eventMentionsFromInhibitionJSONFile.filter(_.hasContext()).toSet

  println(activeEventsWithContext.size + " : number of unique events that have context labels in activation")
  println(inhibEventsWithContext.size + " : Number of unique events that have context labels in inhibition")

  val activeContextLabels = collection.mutable.ListBuffer[String]()
  val inhibContextLabels = collection.mutable.ListBuffer[String]()
  val contextLabelsByPaper = collection.mutable.HashMap[String, Seq[String]]()
  val contextLabelsByEvent = collection.mutable.HashMap[String, (String, Seq[String])]()
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
    val eventID = reformatEventID(act)
    if(contextLabelsByPaper.contains(pmcidOfCurrentEvent)) {
      val existingContextLabels = contextLabelsByPaper(pmcidOfCurrentEvent)
      val newContextsForPaper = existingContextLabels ++ allContextLabelsInThisEvent
      contextLabelsByPaper ++= Map(pmcidOfCurrentEvent -> newContextsForPaper)
    }
    else contextLabelsByPaper ++= Map(pmcidOfCurrentEvent -> allContextLabelsInThisEvent)
    if(contextLabelsByEvent.contains(eventID)) {
      val existingLabels = contextLabelsByEvent(eventID)
      val addedLabels = existingLabels._2 ++ allContextLabelsInThisEvent
      val entry = (existingLabels._1, addedLabels)
      contextLabelsByEvent ++= Map(eventID -> entry)
    }
    else contextLabelsByEvent ++= Map(eventID -> (pmcidOfCurrentEvent, allContextLabelsInThisEvent))
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
    val eventID = reformatEventID(act)
    if(contextLabelsByPaper.contains(pmcidOfCurrentEvent)) {
      val existingContextLabels = contextLabelsByPaper(pmcidOfCurrentEvent)
      val newContextsForPaper = existingContextLabels ++ allContextLabelsInThisEvent
      contextLabelsByPaper ++= Map(pmcidOfCurrentEvent -> newContextsForPaper)
    }
    else contextLabelsByPaper ++= Map(pmcidOfCurrentEvent -> allContextLabelsInThisEvent)
    if(contextLabelsByEvent.contains(eventID)) {
      val existingLabels = contextLabelsByEvent(eventID)
      val addedLabels = existingLabels._2 ++ allContextLabelsInThisEvent
      val entry = (existingLabels._1, addedLabels)
      contextLabelsByEvent ++= Map(eventID -> entry)
    }
    else contextLabelsByEvent ++= Map(eventID -> (pmcidOfCurrentEvent, allContextLabelsInThisEvent))
  }



  println("testing the papers that are added to labels by paper map")
  contextLabelsByPaper.keySet.map(println)

  for((eventID, (paperID, contextLabels)) <- contextLabelsByEvent) {
    println(s"The event ${eventID} has the following ${contextLabels} context labels in the paper ${paperID}")
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


  private def reformatEventID(event: BioEventMention): String = {
    val sentIndex=event.sentence.toString
    val start = event.tokenInterval.start.toString
    val end = event.tokenInterval.end.toString
    sentIndex.concat(start).concat(end)
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
  println(s"There are ${activationLabelsNotInIntersection.size} unique types of context in the activation set that are not in the intersection")
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




  println(" ----------------------- Checking the missing labels that appear in events by label but not list of context labels -----------")
  val missingLabels = collection.mutable.ListBuffer[String]()
 for((_,(_,contextLabels)) <- contextLabelsByEvent) {
   for(c <- contextLabels) {
     if((!(activeContextLabels.contains(c))) && !(inhibContextLabels.contains(c))) missingLabels += c
   }
 }
  missingLabels.map(println)




}
