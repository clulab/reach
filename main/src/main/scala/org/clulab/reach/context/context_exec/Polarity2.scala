package org.clulab.reach.context.context_exec
import java.io.{File, FileOutputStream, ObjectOutputStream, PrintWriter}

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
  val dirForOutput = config.getString("polarityContext.contextLabelsOutputDir")
  for(file <- fileList) {
    val nxmlDoc = nxmlReader.read(file)
    val document = reachSystem.mkDoc(nxmlDoc)
    val mentions = reachSystem.extractFrom(document)
    val evtMentionsOnly = mentions.collect { case evt: BioEventMention => evt }
    for(ev <- evtMentionsOnly) {
      val sentenceID = ev.sentence
      val tokenInterval = ev.tokenInterval
      val subsentence = ev.document.sentences(sentenceID).words.slice(tokenInterval.start,tokenInterval.end+1).mkString(" ")
      if (checkAddingCondition(subsentence))
        {egfDiffEvents += ev
        println(subsentence)}
    }
  }

  val egfDiffEventsWithContext = egfDiffEvents.filter(_.hasContext())
  println(s"Number of events with associated context: ${egfDiffEventsWithContext.size}")

  val activationContextLabels = collection.mutable.ListBuffer[String]()
  val inhibitionContextLabels = collection.mutable.ListBuffer[String]()
  val paperByContextLabelsMap = collection.mutable.ListBuffer[(String, Array[String])]()
  val eventsByPaperID = collection.mutable.ListBuffer[(String, collection.mutable.ListBuffer[BioEventMention])]()
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

    val entry = (docID,contextLabelsInTheCurrentEvent.toArray)
    paperByContextLabelsMap += entry
    val releventEntries = eventsByPaperID.filter(_._1 == docID)
    if (releventEntries.size > 0) {
      val currentEventList = releventEntries(0)._2
      currentEventList += e
    }
    else {
      val listBuf = collection.mutable.ListBuffer[BioEventMention]()
      listBuf += e
      val entry = (docID, listBuf)
      eventsByPaperID += entry
    }


  }

  for((paperID,eventsPerPaper) <- eventsByPaperID) {
    val perPaperDir = dirForOutput.concat(paperID)
    val outputPaperDir = new File(perPaperDir)
    if(!outputPaperDir.exists()) {
      outputPaperDir.mkdirs()
    }
    val eventsPath = perPaperDir.concat("/ArrayOfEvtsByPaper.txt")
    val eventsFile = new File(eventsPath)
    if (!eventsFile.exists()) {
      eventsFile.createNewFile()
    }

    val outputStream = new ObjectOutputStream(new FileOutputStream(eventsPath))
    outputStream.writeObject(eventsPerPaper.toArray)
    outputStream.close()
  }
  for((paperID, contextLabels) <- paperByContextLabelsMap) {
    val perPaperDir = dirForOutput.concat(paperID)
    val outputPaperDir = new File(perPaperDir)
    if(!outputPaperDir.exists()) {
      outputPaperDir.mkdirs()
    }
    val contextFilePath = perPaperDir.concat("/contextLabelsPerPaper.txt")
    val contextFile = new File(contextFilePath)
    if (!contextFile.exists()) {
      contextFile.createNewFile()
    }
    val outputStream = new ObjectOutputStream(new FileOutputStream(contextFilePath))
    outputStream.writeObject(contextLabels)
    outputStream.close()
    val str = contextLabels.mkString(",")
    println(s"The paper ${paperID} has the context labels ${str}")
  }

  val pathForActivationLabels = dirForOutput.concat("activationContextLabels.txt")
  val activationLabelsFile = new File(pathForActivationLabels)
  val pathForInhibitionLabels = dirForOutput.concat("inhibitionContextLabels.txt")
  val inhibitionLabelsFile = new File(pathForInhibitionLabels)
  if (!activationLabelsFile.exists()) {
    activationLabelsFile.createNewFile()
  }
  if (!inhibitionLabelsFile.exists()) {
    inhibitionLabelsFile.createNewFile()
  }

  val actOs = new ObjectOutputStream(new FileOutputStream(pathForActivationLabels))
  val inhOs = new ObjectOutputStream(new FileOutputStream(pathForInhibitionLabels))
  val actLabelsAsArray = activationContextLabels.toArray
  val inhLabelsAsArray = inhibitionContextLabels.toArray
  actOs.writeObject(actLabelsAsArray)
  inhOs.writeObject(inhLabelsAsArray)


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
