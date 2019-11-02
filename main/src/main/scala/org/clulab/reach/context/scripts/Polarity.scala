package org.clulab.reach.context.scripts

import java.io.{File, PrintWriter}
import java.util.Date

import ai.lum.nxmlreader.NxmlReader
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.context.feature_utils.ProcessingStats
import org.clulab.reach.context.utils.polarity_analysis_utils.ContextLabelCountUtils
import org.clulab.reach.mentions.BioEventMention

import scala.collection.parallel.ForkJoinTaskSupport

object Polarity extends App {
  // Please run this script on the papers for polarity before running the PolarityAnalysis script.
  // This script generates some files that is required by the other script.
  // The purpose of this script is to extract the events that have context.
  // The events of interest are the ones where EGF is the controller and "positive" or "negative" is the polarity of the event.
  // For all such events, we want the context labels associated with them.
  // The output of this script is a directory in polarity/genericFileResources/outputForPolarityAnalysisDir/paper_dir.
  // In each paper directory, we have n text files, wherein the name of each text file is the event span that satisfies the EGF (activates/inhibits) condition.
  // The contents of the text file are the context labels that are associated with such events.
  // The PolarityAnalysis script will then read the context labels and count them.
  val config = ConfigFactory.load()
  val papersDir = config.getString("papersDir")
  val dirForOutput = config.getString("polarityContext.outputForPolarityAnalysisDir")
  val threadLimit: Int = config.getInt("threadLimit")
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)
  val egfDiffEvents = collection.mutable.ListBuffer[BioEventMention]()
  processPapers(Some(threadLimit))
  val statsKeeper: ProcessingStats = new ProcessingStats

  def processPapers(threadLimit:Option[Int]):Unit = {
    val fileList = new File(papersDir)
    val files = fileList.listFiles().toVector.par
    if (threadLimit.nonEmpty) {
      files.tasksupport =
        new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit.get))
    }

    for(file <- files) {
      processPaper(file)
    }

  }

  def processPaper(file:File):Unit = {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startTime = new Date()
    val startNS = System.nanoTime

    println(s"$startTime: Starting $paperId")
    try{
      val nxmlDoc = nxmlReader.read(file)
      val document = reachSystem.mkDoc(nxmlDoc)
      val mentions = reachSystem.extractFrom(document)
      val evtMentionsOnly = mentions.collect { case evt: BioEventMention => evt }
      for(ev <- evtMentionsOnly) {
        val sentenceID = ev.sentence
        val tokenInterval = ev.tokenInterval
        val subsentence = ev.document.sentences(sentenceID).words.slice(tokenInterval.start,tokenInterval.end+1).mkString(" ")
        if (ContextLabelCountUtils.checkAddingCondition(subsentence, ev))
        {egfDiffEvents += ev
          println(subsentence)}
      }

    }

    catch {
      case runtimeException: RuntimeException => {
        println(runtimeException)
        println(s"Skipping ${file.getName}")
      }
    }
  }


  val egfDiffEventsWithContext = egfDiffEvents.filter(_.hasContext())
  println(s"Number of events with associated context: ${egfDiffEventsWithContext.size}")

  for(e <- egfDiffEventsWithContext) {
    val docID = e.document.id match {
      case Some(x) => s"PMC${x.split("_")(0)}"
      case None => "unknown"
    }

    val path = dirForOutput.concat(docID)
    val paperIDDir = new File(path)
    if(!paperIDDir.exists()) {
      paperIDDir.mkdirs()
    }

    var polarity = "unknown"
    if(e.label.contains("Positive")) polarity = "activation"
    else if(e.label.contains("Negative")) polarity = "inhibition"
    val eventFileName = path.concat(s"/ContextsForEvent_${ContextLabelCountUtils.extractEvtId(e)}_${polarity}.txt")
    val eventFile = new File(eventFileName)
    if (!eventFile.exists()) {
      eventFile.createNewFile()
    }
    val contextLabels = collection.mutable.ListBuffer[String]()
    val map = e.context match {
      case Some(x) => x
      case None => Map.empty
    }

    for((_, labelSeq) <- map) {
      contextLabels ++= labelSeq
    }

    val stringToWrite = contextLabels.mkString(",")
    val printWriterPerEvent = new PrintWriter(eventFile)
    printWriterPerEvent.write(stringToWrite)
    printWriterPerEvent.close()

  }

}
