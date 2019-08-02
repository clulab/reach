package org.clulab.reach.context.context_exec
import java.io.File

import ai.lum.nxmlreader.NxmlReader
import com.typesafe.config.ConfigFactory
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.mentions.BioEventMention
object Polarity2 extends App{
  val config = ConfigFactory.load()
  val papersDir = config.getString("papersDir")
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)
  val fileListUnfiltered = new File(papersDir)
  val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
  val egfDiffEvents = collection.mutable.ListBuffer[BioEventMention]()
  for(file <- fileList) {
    val nxmlDoc = nxmlReader.read(file)
    val document = reachSystem.mkDoc(nxmlDoc)
    val mentions = reachSystem.extractFrom(document)
    val evtMentionsOnly = mentions.collect { case evt: BioEventMention => evt }
    for(ev <- evtMentionsOnly) {
      val sentenceID = ev.sentence
      val sentenceContents = ev.document.sentences(sentenceID).words.mkString(" ")
      println(sentenceContents)
      if (checkAddingCondition(sentenceContents))
        egfDiffEvents += ev
    }
  }

  val egfDiffEventsWithContext = egfDiffEvents.filter(_.hasContext())
  println(egfDiffEventsWithContext.size)

  val activationContextLabels = collection.mutable.ListBuffer[String]()
  for(e <- egfDiffEventsWithContext) {

    val contextLabels = e.context match {
      case Some(x) => x
      case None => Map.empty
    }
  }


  def checkAddingCondition(sentence: String):Boolean = {
    checkEGFcase(sentence) && checkDifferentCase(sentence)
  }

  def checkEGFcase(sentence:String):Boolean = {
    sentence.contains("EGF") || sentence.contains("egf") || sentence.contains("Epidermal Growth Factor") || sentence.contains("Epidermal growth factor") || sentence.contains("epidermal growth factor")
  }

  def checkDifferentCase(sentence:String):Boolean = {
    sentence.contains("differentiation") || sentence.contains("Differentiation") || sentence.contains("cell differentiation") || sentence.contains("Cell differentiation") || sentence.contains("cell-differentiation")
  }




}
