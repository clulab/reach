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
  val reachSystem = new ReachSystem()
  val sentenceFileContentsToIntersect = collection.mutable.ListBuffer[String]()
  val sentencesByPaper = collection.mutable.HashMap[String, Seq[String]]()
  for(file<- fileList) {
    val pmcid = file.getName.slice(0,file.getName.length-5)
    val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${pmcid}")
    val pathForPolarity = outPaperDirPath.concat("/sentences.txt")
    val lines = Source.fromFile(pathForPolarity).getLines()
    sentencesByPaper ++= Map(pmcid -> lines.toSeq)
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

  val activationIndices = collection.mutable.HashMap[String, (String, Int)]()
  val inhibitionIndices = collection.mutable.HashMap[String, (String, Int)]()


  println(activationIntersection.size)
  println(inhibitionIntersection.size)
  for(i<-activationIntersection) {
    for((pmcid,sentences) <- sentencesByPaper) {
      println(s"The current paper is: ${pmcid}")
      println(s"the current sentence is: ${i}")
      println(s"does the current sentence appear in the sentences of the current paper? ${sentences.contains(i)}")
    }
  }


  for(i<-inhibitionIntersection) {
    for((pmcid,sentences) <- sentencesByPaper) {
      if(sentences.contains(i)) {
        val index = sentences.indexOf(i)
        inhibitionIndices ++= Map(pmcid -> (i, index))
      }
    }
  }

  println(sentencesByPaper.size)
  println(activationIndices.size)
  println(inhibitionIndices.size)
  for((pmcid,(sentence, index)) <- activationIndices) {
    println(s"The sentence ${sentence} was found in the paper ${pmcid} at index ${index}")
  }


  for((pmcid,(sentence, index)) <- inhibitionIndices) {
    println(s"The sentence ${sentence} was found in the paper ${pmcid} at index ${index}")
  }





}
