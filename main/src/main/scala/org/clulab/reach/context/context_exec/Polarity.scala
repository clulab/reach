package org.clulab.reach.context.context_exec

import java.io.{File, PrintWriter}
import java.io.{File, FileOutputStream, ObjectOutputStream, PrintWriter}
import ai.lum.nxmlreader.NxmlReader
import com.typesafe.config.ConfigFactory
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.context.context_utils.PolarityUtils
import org.clulab.reach.mentions.BioEventMention
import java.util.Date
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.io.Source

object Polarity extends App {
  val config = ConfigFactory.load()
  val papersDir = config.getString("polarityContext.temporaryRun")
  val dirForOutput = config.getString("polarityContext.contextLabelsOutputDir")
  val withAssembly:Boolean = config.getBoolean("withAssembly")
  val threadLimit: Int = config.getInt("threadLimit")

  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)
  val egfDiffEvents = collection.mutable.ListBuffer[BioEventMention]()
  def processPapers(threadLimit:Option[Int], withAssembly:Boolean):Unit = {
    val fileList = new File(papersDir)
    val files = fileList.listFiles().toVector.par

  }

  print("no errors in build sbt")

}
