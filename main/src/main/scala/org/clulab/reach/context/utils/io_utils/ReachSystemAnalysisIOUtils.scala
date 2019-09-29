package org.clulab.reach.context.utils.io_utils

import java.io.{File, PrintWriter}

import scala.io.Source

object ReachSystemAnalysisIOUtils {
  def generateLabelMap(fileName: String): Map[(String,String,String), Int] = {
    val map = collection.mutable.HashMap[(String,String,String), Int]()
    val source = Source.fromFile(fileName)
    val lines = source.getLines()
    val content = lines.drop(1)
    for(c <- content) {
      val array = c.split(",")
      val pmcid = array(0)
      val evtID = array(1)
      val ctxID = array(2)
      val label = Integer.parseInt(array(3))
      val tup = (pmcid,evtID,ctxID)
      map ++= Map(tup -> label)
    }

    map.toMap
  }

  def loadSentencesPerPaper(parentDirForPapers:String):Map[String,Seq[String]] = {
    val dirsForSentencesFile = new File(parentDirForPapers).listFiles.filter(_.isDirectory)
    val sentencesByPaper = collection.mutable.HashMap[String, Seq[String]]()
    for(paperDir <- dirsForSentencesFile) {
      val sentencesInThisFile = collection.mutable.ListBuffer[String]()
      val sentencesFile = paperDir.listFiles.filter(_.getName == "sentences.txt")(0)
      val source = Source.fromFile(sentencesFile)
      val sentences = source.getLines()
      for(s <- sentences) {
        sentencesInThisFile += s
      }

      val mapEntry = Map(paperDir.getName -> sentencesInThisFile)
      sentencesByPaper ++= mapEntry

    }

    sentencesByPaper.toMap
  }

  def writeBinaryStringsOfEventSpansByPaper(parentDirPath:String, mapOfBinaries:Map[String,Seq[String]], reachVersion:String):Unit = {
    val parentDirFileInstance = new File(parentDirPath)
    val dirsWithPaperNames = parentDirFileInstance.listFiles().filter(_.isDirectory)
    for(paperDir <- dirsWithPaperNames) {
      val pathToEventsSpanFile = s"${parentDirPath}/${paperDir.getName}/uniqueEventSpans_${reachVersion}Reach.txt"
      val eventSpanFileInstance = new File(pathToEventsSpanFile)
      if(!eventSpanFileInstance.exists())
        eventSpanFileInstance.createNewFile()
      val printWriter = new PrintWriter(eventSpanFileInstance)
      val binariesInThisPaper = mapOfBinaries(paperDir.getName)
      println(s"In write string function, the current paperID is: ${binariesInThisPaper.size}")
      val separator = binariesInThisPaper.mkString("\n")
      printWriter.write(separator)
    }
  }
}
