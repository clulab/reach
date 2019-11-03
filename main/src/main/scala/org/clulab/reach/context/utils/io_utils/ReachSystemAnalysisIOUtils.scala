package org.clulab.reach.context.utils.io_utils

import java.io.{File, PrintWriter}

import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.reach.context.feature_utils.ContextFeatureUtils

import scala.io.Source

object ReachSystemAnalysisIOUtils {


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
      for(b <- binariesInThisPaper) {
        printWriter.write(b)
        printWriter.write("\n")
      }
      printWriter.close()
    }
  }


  def writeMatchingRowFeatureValues(reach2019RootDirPath:String, matchingAnnotationsRootDirPath:String, pathToFeatureValueDataframe:String, currentPaperID:String):Int={
    val dirInstance = new File(reach2019RootDirPath)
    val currentPaperDir = dirInstance.listFiles().filter(x => x.isDirectory && x.getName.contains(currentPaperID))(0)
    val allManualAnnotations = getTransferredAnnotationsFromReach2016(matchingAnnotationsRootDirPath)
    print(s"Total number of manual annotations found: ${allManualAnnotations.size}")
    val matchingRows = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val rowFilesInCurrentPaper = currentPaperDir.listFiles()
    for (rowAsFileInstance <- rowFilesInCurrentPaper) {
      val row = ContextFeatureUtils.readAggRowFromFile(rowAsFileInstance)
      val specs = ContextFeatureUtils.createAggRowSpecsFromFile(rowAsFileInstance)
      if(allManualAnnotations.contains(specs))
        matchingRows += row
    }
    matchingRows.size
    //    val dirInstance = new File(reach2019RootDirPath)
//    val mapOfRowsByPaperID = collection.mutable.HashMap[String,Seq[AggregatedContextInstance]]()
//    val paperDirs = dirInstance.listFiles().filter(_.isDirectory)
//    val manualAnnotations = getTransferredAnnotationsFromReach2016(matchingAnnotationsRootDirPath)
//    for(currentPaperDir <- paperDirs){
//      val rowsAsFiles = currentPaperDir.listFiles()
//      val featureNames = ContextFeatureUtils.readAggRowFromFile(rowsAsFiles(0)).featureGroupNames
//      val strOfFeatureNames = featureNames.mkString(",")
//      val aggrRowsInPaper = collection.mutable.ListBuffer[AggregatedContextInstance]()
//      for(rowFileInstance<-rowsAsFiles){
//        val row = ContextFeatureUtils.readAggRowFromFile(rowFileInstance)
//        val specs = ContextFeatureUtils.createAggRowSpecsFromFile(rowFileInstance)
//        if(manualAnnotations.contains(specs)){
//          aggrRowsInPaper += row
//        }
//      }
//      mapOfRowsByPaperID ++= Map(currentPaperDir.getName -> aggrRowsInPaper)
//    }
//    mapOfRowsByPaperID.toMap
  }

  def getTransferredAnnotationsFromReach2016(pathToPredictionsDir:String):Seq[(String,String,String)] = {
    val annotationsPerPaper = collection.mutable.ListBuffer[(String,String,String)]()
    val parentDirAsFile = new File(pathToPredictionsDir)
    val papersDirs = parentDirAsFile.listFiles().filter(_.isDirectory)

    for(paperDir <- papersDirs){
      val paperID = paperDir.getName
      val annotationsFile = paperDir.listFiles()(0)
      val source = Source.fromFile(annotationsFile)
      val lines = source.getLines()
      for(l<-lines){
        val eventID = l.split(",")(0)
        val contextID = l.split(",")(1)
        val tupleEntry = (paperID, eventID,contextID)
        annotationsPerPaper += tupleEntry
      }
    }
    annotationsPerPaper
  }
}
