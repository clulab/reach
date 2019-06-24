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
  val reachSystem = new ReachSystem()

  /*val text = … // A sentence that comes from the json file
  val reachSystem = new ReachSystem
  val doc = reachSystem.mkDoc(text, “”, “”)
  val newText = doc.sentences(0).getSentenceText*/
  println(fileList.size)
  for(file<- fileList) {
    val pmcid = file.getName.slice(0,file.getName.length-5)
    val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${pmcid}")
    val pathForPolarity = outPaperDirPath.concat("/sentences.txt")
    val lines = Source.fromFile(pathForPolarity).getLines()
    for(l<-lines) {
      println(l)
    }
  }


  println(activSentences.size)

  for(text<-activSentences) {
    val doc = reachSystem.mkDoc(text, "", "")
    val newText = doc.sentences(0).getSentenceText
    println(newText)
  }

}
