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
/*  val activSentPath = config.getString("polarityContext.genericFileDir").concat("activation_sentences_in_json.txt")
  val inhibSentPath = config.getString("polarityContext.genericFileDir").concat("inhibition_sentences_in_json.txt")
  val activSentences = Source.fromFile(activSentPath).getLines()
  val inhibSentences = Source.fromFile(inhibSentPath).getLines()*/
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val sentenceWindow = config.getString("contextEngine.params.bound")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
  //val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml")
  //val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml", "PMC1590014.nxml", "PMC1849968.nxml", "PMC2424011.nxml", "PMC2847694.nxml")
  val fullPapers = List("PMC2958340.nxml", "PMC2686753.nxml", "PMC4092102.nxml", "PMC4142739.nxml", "PMC4236140.nxml", "PMC4446607.nxml", "PMC1590014.nxml")

  val fileListUnfiltered = new File(dirForType)
  val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml") && (fullPapers.contains(x.getName)))
  println(fileList.size == fullPapers.size)
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)


}
