package org.clulab.reach.context.context_exec

import java.io.File

import ai.lum.nxmlreader.NxmlReader
import com.typesafe.config.ConfigFactory
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.mentions.BioEventMention

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
  val fileListUnfiltered = new File(dirForType)
  val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)
  val sentenceFileContentsToIntersect = collection.mutable.ListBuffer[String]()
  val sentencesByPaper = collection.mutable.HashMap[String, Array[String]]()
  val eventsByPaper = collection.mutable.HashMap[String, Array[EventMention]]()
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
    val eventMentionsHavingContext = evtMentionsOnly.filter(_.hasContext())
    println(eventMentionsHavingContext.size)
    eventsByPaper ++= Map(pmcid -> eventMentionsHavingContext.toArray)
    allEvents ++= eventMentionsHavingContext
  }
  println(s"Sentences mega list contains: ${sentenceFileContentsToIntersect.size} sentences")
  val activeSentenceForIntersect = collection.mutable.ListBuffer[String]()
  for(text<-activSentences) {
    val doc = reachSystem.mkDoc(text, "", "")
    val newText = doc.sentences(0).getSentenceText
    activeSentenceForIntersect += newText
  }

  val activationIntersection = activeSentenceForIntersect.toSet.intersect(sentenceFileContentsToIntersect.toSet)


  val inhibSentenceForIntersect = collection.mutable.ListBuffer[String]()
  for(text<-inhibSentences) {
    val doc = reachSystem.mkDoc(text, "", "")
    val newText = doc.sentences(0).getSentenceText
    inhibSentenceForIntersect += newText
  }

  val inhibitionIntersection = inhibSentenceForIntersect.toSet.intersect(sentenceFileContentsToIntersect.toSet)

  val activationIndices = collection.mutable.HashMap[String, (String, Int)]()
  val inhibitionIndices = collection.mutable.HashMap[String, (String, Int)]()


  println(s"There are ${activationIntersection.size} sentences in the activation intersection")
  println(s"There are ${inhibitionIntersection.size} sentences in the inhibition intersection")

  for((paperID, sentences) <- sentencesByPaper) {
    for(a<-activationIntersection)
    {
      if(sentences.contains(a)) {
        val index = sentences.indexOf(a)
        activationIndices ++= Map(paperID -> (a,index))
      }
    }
    for(i<-inhibitionIntersection)
    {
      if(sentences.contains(i)) {
        val index = sentences.indexOf(i)
        inhibitionIndices ++= Map(paperID -> (i,index))
      }
    }
  }
  println(s"The activation indices map is of size: ${activationIndices.size}")
  println(s"The inhibition indices map is of size: ${inhibitionIndices.size}")

  val activationPapers = List("PMC2958340", "PMC2910130", "PMC4236140", "PMC4142739", "PMC4446607", "PMC4092102")
  val inhibitionPapers = List("PMC2587086", "PMC3138418", "PMC3666248", "PMC2636845", "PMC3635065", "PMC3640659", "PMC2686753", "PMC3119364")
  val activationEvents = collection.mutable.ListBuffer[BioEventMention]()
  val inhibitionEvents = collection.mutable.ListBuffer[BioEventMention]()
  for(event <- allEvents) {
    for((pmcid,(_, index)) <- activationIndices) {
      val eventDocId = event.document.id match {
        case Some(x) => s"PMC${x.split("_")(0)}"
        case None => "unknown"
      }
      val bool = (activationPapers.contains(eventDocId)) && (index == event.sentence)
      if(bool) activationEvents += event
    }


    for((pmcid,(_, index)) <- inhibitionIndices) {
      val eventDocId = event.document.id match {
        case Some(x) => s"PMC${x.split("_")(0)}"
        case None => "unknown"
      }
      val bool = (inhibitionPapers.contains(eventDocId)) && (index == event.sentence)
      if(bool) inhibitionEvents += event
    }
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

  for(act <- inhibitionEvents) {
    val map = act.context match {
      case Some(m) => m
      case None => Map.empty
    }
    for((_, contextLabels) <- map) {
      contextsInInhibition ++= contextLabels
    }
  }

  val intersectingContextLabels = contextsInActivation.toSet.intersect(contextsInInhibition.toSet)
  println(s"There are a total of ${contextsInActivation.size} context labels in activation")
  println(s"There are a total of ${contextsInInhibition.size} context labels in inhibition")
  println(s"There are ${intersectingContextLabels.size} context labels in common with the activation set and inhibition set for a sentence window of ${sentenceWindow}")
  intersectingContextLabels.map(println)

  val activationNoIntersection = contextsInActivation -- intersectingContextLabels
  println(s"There are ${activationNoIntersection.size} context labels in the activation set, but not in the interection set")
  val inhibitionNoIntersection = contextsInInhibition -- intersectingContextLabels
  println(s"There are ${inhibitionNoIntersection.size} context labels in the inhibition set, but not in the interection set")

}
