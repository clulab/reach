package org.clulab.reach.context.context_exec
import com.typesafe.config.ConfigFactory

import scala.io.Source
import scala.util.parsing.json.JSON
import java.io.{File, PrintWriter}

import ai.lum.nxmlreader.NxmlReader
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.context.context_exec.GenerateOutputFiles.{nxmlReader, reachSystem}
import org.clulab.reach.context.context_utils.EventContextPairGenerator
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}
object Polarity extends App {
  val config = ConfigFactory.load()
  val polarityDir = config.getString("polarityContext.polarityDir")
  val activationPapers = List("PMC2958340", "PMC2910130", "PMC4236140", "PMC4142739", "PMC4446607", "PMC4092102")
  val inhibitionPapers = List("PMC2587086", "PMC3138418", "PMC3666248", "PMC2636845", "PMC3635065", "PMC3640659", "PMC2686753", "PMC3119364")
  val genericFileDir = config.getString("polarityContext.genericFileDir")
  val activationSentencesPath = genericFileDir.concat("activation_sentences_in_json.txt")
  val inhibitionSentencesPath = genericFileDir.concat("inhibition_sentences_in_json.txt")
  val activationSentences = Source.fromFile(activationSentencesPath).getLines()
  val inhibitionSentences = Source.fromFile(inhibitionSentencesPath).getLines()
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)
  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  val fileListUnfiltered = new File(dirForType)
  val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
  for(file <- fileList) {
    val pmcid = file.getName.slice(0,file.getName.length-5)
    val nxmlDoc = nxmlReader.read(file)
    val document = reachSystem.mkDoc(nxmlDoc)
    val collectSent = collection.mutable.ListBuffer[String]()
    for (s <- document.sentences) {
      val currentSent = s.words.mkString(" ")
      collectSent += currentSent
    }
    val mentions = reachSystem.extractFrom(document)
    val contextMentions = mentions filter ContextEngine.isContextMention map (_.asInstanceOf[BioTextBoundMention])
    val crossProducter = new EventContextPairGenerator(mentions, contextMentions)
    val pairs = crossProducter.yieldContextEventPairs()
    if(activationPapers.contains(pmcid)) {
      for(a<-activationSentences) {
        if(collectSent.contains(a)) {
          println(pmcid)
          println(collectSent.indexOf(a))
        }
      }
    }
    else {

    }

  }

}
