package org.clulab.reach.context.context_exec
import com.typesafe.config.ConfigFactory

import scala.io.Source
import scala.util.parsing.json.JSON
import java.io.{File, PrintWriter}
import org.clulab.struct.Interval
import ai.lum.nxmlreader.NxmlReader
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.context.context_exec.GenerateOutputFiles.{nxmlReader, reachSystem}
import org.clulab.reach.context.context_utils.EventContextPairGenerator
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}
object Polarity extends App {
  println("Hello world from polarity")
  val config = ConfigFactory.load()
  val activSentPath = config.getString("polarityContext.genericFileDir").concat("activation_sentences_in_json.txt")
  val inhibSentPath = config.getString("polarityContext.genericFileDir").concat("inhibition_sentences_in_json.txt")
  val activSentences = Source.fromFile(activSentPath).getLines()
  val inhibSentences = Source.fromFile(inhibSentPath).getLines()
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
  val fileListUnfiltered = new File(dirForType)
  val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
  println(fileList.size)
  val reachSystem = new ReachSystem()
  val sentenceFileContentsToIntersect = collection.mutable.ListBuffer[String]()

  for(file<- fileList) {
    val pmcid = file.getName.slice(0,file.getName.length-5)
    val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${pmcid}")
    val pathForPolarity = outPaperDirPath.concat("/sentences.txt")
    val lines = Source.fromFile(pathForPolarity).getLines()
    sentenceFileContentsToIntersect ++= lines
  }

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


  val activationIndices = activationIntersection.map(s => sentenceFileContentsToIntersect.indexOf(s))
  val inhibitionIndices = inhibitionIntersection.map(i => sentenceFileContentsToIntersect.indexOf(i))
  val activationEventIDsBySentIndex = collection.mutable.HashMap[Int, Seq[String]]()
  val inhibitionEventIDsBySentIndex = collection.mutable.HashMap[Int, Seq[String]]()
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  for(file<- fileList) {
    val nxmlDoc = nxmlReader.read(file)
    println("nxml loaded successfully")
    val document = reachSystem.mkDoc(nxmlDoc)
    println("mkDoc performed successfully")
    val mentions = reachSystem.extractFrom(document)
    println("mentions extracted successfully")
    val evtMentionsOnly = mentions.collect { case evt: EventMention => (evt.sentence, evt.tokenInterval) }
    for(a<-activationIndices) {
      val eventsPerIndex = evtMentionsOnly.filter(x => x._1 == a)
      val eventIDsPerIndex = eventsPerIndex.map(x => extractEvtId(x))
      activationEventIDsBySentIndex ++= Map(a -> eventIDsPerIndex)
      println(eventIDsPerIndex.size)
    }

    for(a<-inhibitionIndices) {
      val eventsPerIndex = evtMentionsOnly.filter(x => x._1 == a)
      val eventIDsPerIndex = eventsPerIndex.map(x => extractEvtId(x))
      inhibitionEventIDsBySentIndex ++= Map(a -> eventIDsPerIndex)
      println(eventIDsPerIndex.size)
    }

  }

  def extractEvtId(tuple: (Int, Interval)):String = {
    tuple._1+tuple._2.start.toString+tuple._2.end.toString
  }



}
