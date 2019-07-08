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
  val activSentences = Source.fromFile(activSentPath).getLines()
  val inhibSentences = Source.fromFile(inhibSentPath).getLines()
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val sentenceWindow = config.getString("contextEngine.params.bound")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
  //val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml")
  //val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml", "PMC1590014.nxml", "PMC1849968.nxml", "PMC2424011.nxml", "PMC2847694.nxml")
  val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml", "PMC1590014.nxml")

  val fileListUnfiltered = new File(dirForType)
  val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml") && (fullPapers.contains(x.getName)))
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)
  val sentenceFileContentsToIntersect = collection.mutable.ListBuffer[String]()
  val sentencesByPaper = collection.mutable.HashMap[String, Array[String]]()
  val eventsByPaper = collection.mutable.HashMap[String, Array[BioEventMention]]()
  val allEvents = collection.mutable.ListBuffer[BioEventMention]()
  for(file<- fileList) {
    val pmcid = file.getName.slice(0,file.getName.length-5)
    val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${pmcid}")
    val pathForSentences = outPaperDirPath.concat("/sentences.txt")
    val linesForBigList = Source.fromFile(pathForSentences).getLines()
    val linesForMap = Source.fromFile(pathForSentences).getLines()
    sentencesByPaper ++= Map(pmcid -> linesForMap.toArray)
    sentenceFileContentsToIntersect ++= linesForBigList

    val nxmlDoc = nxmlReader.read(file)
    val document = reachSystem.mkDoc(nxmlDoc)
    val mentions = reachSystem.extractFrom(document)
    val evtMentionsOnly = mentions.collect { case evt: BioEventMention => evt }
    val eventMentionsHavingContext = evtMentionsOnly.filter(_.hasContext()).toSet
    eventsByPaper ++= Map(pmcid -> eventMentionsHavingContext.toArray)
    allEvents ++= eventMentionsHavingContext
  }

  println(s"There are ${allEvents.toSet.size} unique event mentions over all the ${eventsByPaper.size} papers")
  for((paperID, events) <- eventsByPaper) {
    println(s"The paper ${paperID} has ${events.size} unique event mentions")
  }
  val activeSentenceForIntersect = collection.mutable.ListBuffer[String]()
  for(text<-activSentences) {
    val doc = reachSystem.mkDoc(text, "", "")
    val newText = doc.sentences(0).getSentenceText
    activeSentenceForIntersect += newText
  }

  val activationIntersection = activeSentenceForIntersect.toSet.intersect(sentenceFileContentsToIntersect.toSet)
  println(s"Size of activation sentences as read from JSON file: ${activeSentenceForIntersect.size}")
  println(s"Size of activation sentences intersected with all sentences from sentences.txt of each paper: ${activationIntersection.size}")

  val inhibSentenceForIntersect = collection.mutable.ListBuffer[String]()
  for(text<-inhibSentences) {
    val doc = reachSystem.mkDoc(text, "", "")
    val newText = doc.sentences(0).getSentenceText
    inhibSentenceForIntersect += newText
  }

  val inhibitionIntersection = inhibSentenceForIntersect.toSet.intersect(sentenceFileContentsToIntersect.toSet)
  println(s"Size of inhibition sentences as read from JSON file: ${inhibSentenceForIntersect.size}")
  println(s"Size of inhibition sentences intersected with all sentences from sentences.txt of each paper: ${inhibitionIntersection.size}")
  val activationIndices = collection.mutable.HashMap[String, (String, Int)]()
  val inhibitionIndices = collection.mutable.HashMap[String, (String, Int)]()


  val collectIndicesOfValidSentences = collection.mutable.ListBuffer[Int]()
  for((paperID, sentences) <- sentencesByPaper) {
    for(a<-activationIntersection)
    {
      if(sentences.contains(a)) {
        val index = sentences.indexOf(a)
        println(s"The sentence ${a} appears in the paper ${paperID} at index ${index}")
        collectIndicesOfValidSentences += index
        activationIndices ++= Map(paperID -> (a,index))
      }
    }
    for(i<-inhibitionIntersection)
    {
      if(sentences.contains(i)) {
        val index = sentences.indexOf(i)
        println(s"The sentence ${i} appears in the paper ${paperID} at index ${index}")
        collectIndicesOfValidSentences += index
        inhibitionIndices ++= Map(paperID -> (i,index))
      }
    }
  }

  val activationEvents = collection.mutable.ListBuffer[BioEventMention]()
  val inhibitionEvents = collection.mutable.ListBuffer[BioEventMention]()
  val validEventsInPartSentences = allEvents.filter(x => collectIndicesOfValidSentences.contains(x.sentence))
  for(event <- validEventsInPartSentences) {
    println(event.label)
    if((event.label.contains("Positive")) && (!(activationEvents.contains(event)))) activationEvents += event


    else if((event.label.contains("Negative")) && (!(inhibitionEvents.contains(event)))) inhibitionEvents += event
  }


  val contextsInActivation = collection.mutable.ListBuffer[String]()
  val contextsInInhibition = collection.mutable.ListBuffer[String]()


  for(act <- activationEvents) {
    val map = act.context match {
      case Some(m) => m
      case None => Map.empty
    }
    for((_, contextLabels) <- map) {
      contextsInActivation ++= contextLabels
    }
  }

  for(inh<-inhibitionEvents) {
    val map = inh.context match {
      case Some(m) => m
      case None => Map.empty
    }

    for((_, contextLabels) <- map) {
      contextsInInhibition ++= contextLabels
    }
  }

  val contextLabelsSuperList = collection.mutable.ListBuffer[String]()

  val intersectingContextLabels = contextsInActivation.toSet.intersect(contextsInInhibition.toSet)
  println(s"There are ${intersectingContextLabels.size} context labels in common with the activation set and inhibition set for a sentence window of ${sentenceWindow}")
  intersectingContextLabels.map(println)

  val activationNoIntersection = contextsInActivation.filter(x => (!(intersectingContextLabels.contains(x)))).toSet
  println(s"There are ${activationNoIntersection.size} unique context labels in the activation set, but not in the intersection set.")
  println(s"In total, the activation set has ${(activationNoIntersection.union(intersectingContextLabels)).size} context mentions")
  val inhibitionNoIntersection = contextsInInhibition.filter(x => (!(intersectingContextLabels.contains(x)))).toSet
  println(s"There are ${inhibitionNoIntersection.size} unique context labels in the inhibition set, but not in the intersection set.")
  println(s"In total, the inhibition set has ${(inhibitionNoIntersection.union(intersectingContextLabels)).size} context mentions")




  contextLabelsSuperList ++= activationNoIntersection
  contextLabelsSuperList ++= inhibitionNoIntersection


 val contextMentionsByPaper = collection.mutable.HashMap[String, Seq[String]]()
  for((paperID, eventMentions) <- eventsByPaper) {
    val contextLabelsPerEvent = collection.mutable.ListBuffer[String]()
    for(act <- eventMentions) {
      val map = act.context match {
        case Some(m) => m
        case None => Map.empty
      }
      for((_, contextLabels) <- map) {
        contextLabelsPerEvent ++= contextLabels
      }
    }
    contextMentionsByPaper ++= Map(paperID -> contextLabelsPerEvent)
  }

  val freqOfActivationLabelInBigList = collection.mutable.HashMap[String, Int]()
  val freqOfActivationLabelOverPapers = collection.mutable.HashMap[String, (Int, Seq[String])]()
  val freqOfInhibitionLabelInBigList = collection.mutable.HashMap[String, Int]()
  val freqOfInhibitionLabelOverPapers = collection.mutable.HashMap[String, (Int, Seq[String])]()

  for(act <- activationNoIntersection) {
    val freqInBigListOfCtxLabels = PolarityUtils.countOccurrencesOfStringinList(act, contextLabelsSuperList.toList)
    val freqOverPapers = PolarityUtils.countOccurrencesOfStringInPaper(act, contextMentionsByPaper.toMap)
    freqOfActivationLabelInBigList ++= Map(act -> freqInBigListOfCtxLabels)
    freqOfActivationLabelOverPapers ++= Map(act -> freqOverPapers)

  }

  for(inh <- inhibitionNoIntersection) {
    val freqInBigList = PolarityUtils.countOccurrencesOfStringinList(inh, contextLabelsSuperList.toList)
    val freqOverPapers = PolarityUtils.countOccurrencesOfStringInPaper(inh, contextMentionsByPaper.toMap)
    freqOfInhibitionLabelInBigList ++= Map(inh -> freqInBigList)
    freqOfInhibitionLabelOverPapers ++= Map(inh -> freqOverPapers)
  }

  val sortedfreqOfActivationLabelInBigList = ListMap(freqOfActivationLabelInBigList.toSeq.sortWith(_._2 > _._2):_*)
  val sortedfreqOfActivationLabelOverPapers = ListMap(freqOfActivationLabelOverPapers.toSeq.sortWith(_._2._1 > _._2._1):_*)

  // writing labels to file.
  val labelsWrittenToFile = config.getString("polarityContext.labelsWrittenToFile")
  val contextLabelFile = new File(labelsWrittenToFile)
  if (!contextLabelFile.exists()) {
    contextLabelFile.createNewFile()
  }
  val pw = new PrintWriter(contextLabelFile)
  pw.write("WRITING CONTEXT LABELS IN ACTIVATION")
  pw.write(s"There are a total of ${contextsInActivation.toSet.size} unique context labels in the activation set")
  for(c <- contextsInActivation) {
    pw.write(c)
  }
  pw.write(s"There are a total of ${contextsInInhibition.toSet.size} unique context labels in the inhibition set")
  for(c <- contextsInInhibition) {
    pw.write(c)
  }


  println(s"PRINTING FREQUENCY OF ACTIVATION LABELS NOT IN INTERSECTION")
  println(s"There are ${activationNoIntersection.size} unique types of context in the activation set that are not in the intersection")
  pw.write(s"There are ${activationNoIntersection.size} unique types of context in the activation set that are not in the intersection")

  for((ctxLabel, freq) <- sortedfreqOfActivationLabelInBigList) {
    pw.write(s"The activation context label ${ctxLabel} appears ${freq} times in the list of all context mentions (not including intersection)")

    println(s"The activation context label ${ctxLabel} appears ${freq} times in the list of all context mentions (not including intersection)")
  }
  pw.write("\n")
  for((ctxLabel, freq) <- sortedfreqOfActivationLabelOverPapers) {
   pw.write(s"The activation context label ${ctxLabel} appears in ${freq} out of ${contextMentionsByPaper.size} papers")

    println(s"The activation context label ${ctxLabel} appears in ${freq} out of ${contextMentionsByPaper.size} papers")
  }

  println("*****************************************")
  val sortedfreqOfInhibitionLabelInBigList = ListMap(freqOfInhibitionLabelInBigList.toSeq.sortWith(_._2 > _._2):_*)
  val sortedfreqOfInhibitionLabelOverPapers = ListMap(freqOfInhibitionLabelOverPapers.toSeq.sortWith(_._2._1 > _._2._1):_*)

 pw.write(s"PRINTING FREQUENCY OF INHIBITION LABELS NOT IN INTERSECTION")
  pw.write(s"There are ${inhibitionNoIntersection.size} unique types of context in the inhibition set that are not in the intersection")
  for((ctxLabel, freq) <- sortedfreqOfInhibitionLabelInBigList) {
    pw.write(s"The inhibition context label ${ctxLabel} appears ${freq} times in the list of all context mentions (not including intersection)")
  }
  pw.write("\n")
  for((ctxLabel, freq) <- sortedfreqOfInhibitionLabelOverPapers) {
    pw.write(s"The inhibition context label ${ctxLabel} appears in ${freq} out of ${contextMentionsByPaper.size} papers")
  }





}
