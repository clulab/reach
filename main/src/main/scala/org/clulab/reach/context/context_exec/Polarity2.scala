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
      val tokenInterval = ev.tokenInterval
      val subsentence = ev.document.sentences(sentenceID).words.slice(tokenInterval.start,tokenInterval.end+1).mkString(" ")
      if (checkAddingCondition(subsentence))
        {egfDiffEvents += ev
        println(sentenceContents)
        println(subsentence)}
    }
  }

  val egfDiffEventsWithContext = egfDiffEvents.filter(_.hasContext())
  println(egfDiffEventsWithContext.size)

  val activationContextLabels = collection.mutable.ListBuffer[String]()
  val inhibitionContextLabels = collection.mutable.ListBuffer[String]()
  val paperByContextLabelsMap = collection.mutable.HashMap[String, Array[String]]()
  for(e <- egfDiffEventsWithContext) {

    val contextLabels = e.context match {
      case Some(x) => x
      case None => Map.empty
    }

    val contextLabelsInTheCurrentEvent = collection.mutable.ListBuffer[String]()
      contextLabels.map(x => {
        contextLabelsInTheCurrentEvent ++= x._2})

    if(e.label.contains("Positive")) activationContextLabels ++= contextLabelsInTheCurrentEvent
    else if(e.label.contains("Negative")) inhibitionContextLabels ++= contextLabelsInTheCurrentEvent
    val docID = e.document.id match {
      case Some(s) => s"PMC${s.split("_")(0)}"
      case None => "Unknown"
    }

    val entry = Map(docID -> contextLabelsInTheCurrentEvent.toArray)
    paperByContextLabelsMap ++= entry

  }

  for((paperID, contextLabels) <- paperByContextLabelsMap) {
    val str = contextLabels.mkString(",")
    println(s"The paper ${paperID} has the context labels ${str}")
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
