package org.clulab.reach.context.scripts

import java.io.File

import ai.lum.nxmlreader.NxmlReader
import com.typesafe.config.ConfigFactory
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.context.scripts.GenerateOutputFiles.{dirForType, nxmlReader, reachSystem}
import org.clulab.reach.context.utils.io_utils.ReachSystemAnalysisIOUtils

object CatchChangingEventSpans extends App{
  val config = ConfigFactory.load()
  val dirForType = config.getString("papersDir")
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)

  val fileListUnfiltered = new File(dirForType)
  val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
  for(file <- fileList) {
    try{

      val nxmlDoc = nxmlReader.read(file)
      val document = reachSystem.mkDoc(nxmlDoc)
      reachSystem.extractFrom(document)
    }
    catch{
      case e:RuntimeException => println(e)
        println(s"Skipping ${file.getName}")
    }
  }

}
