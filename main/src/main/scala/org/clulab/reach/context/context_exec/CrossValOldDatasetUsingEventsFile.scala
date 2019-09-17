package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import java.io.File

import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.context_utils.ContextFeatureUtils

import scala.io.Source

object CrossValOldDatasetUsingEventsFile extends App {
  val config = ConfigFactory.load()
  val parentDirForRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val annotationsFileDir = config.getString("polarityContext.eventsFilesDir")
  val labelsFromEventFiles = makeLabelMapFromEventFileDir(annotationsFileDir)
  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val labelMapFromOldDataset = CodeUtils.generateLabelMap(labelFile)
  val setOfEntriesWithAnnotations = labelsFromEventFiles.toSet.union(labelMapFromOldDataset.toSet)
  println(setOfEntriesWithAnnotations.size)
  //  val smallSetOfPapers = List("PMC2156142", "PMC2195994")
//  val dirsToUseForDebug = allPapersDirs.filter(x => smallSetOfPapers.contains(x.getName))
  val allPapersDirs = new File(parentDirForRows).listFiles().filter(x => x.isDirectory && x.getName != "newAnnotations")
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val allRowsByPaperID = collection.mutable.ListBuffer[(String, Seq[AggregatedContextInstance])]()
  for(paperDir <- allPapersDirs) {
    // In this code we won't face the double counting of two rows from a given paper, because each paper appears only once over all.
    // While analyzing the performance over sentence windows, we encountered the same paper over different values of sentence window. That's why we had the risk of adding the same row twice.
    // But we now see that each paper appears only once, and we read the rows from that paper. So we won't add the same row twice.
    // The only time we will see the same paper appear twice will be in the hold-one-out cross-validation phase, which is expected behavior.
    val rowFiles = paperDir.listFiles().filter(x => x.getName.contains("Aggregated"))
    val rowsForCurrentSent = collection.mutable.ListBuffer[AggregatedContextInstance]()
    var containsOnlyThisPapersRows = true
    for(r <- rowFiles) {
      containsOnlyThisPapersRows = containsOnlyThisPapersRows && (r.getName.contains(paperDir.getName))
      val pathToRow = parentDirForRows.concat(s"${paperDir.getName}").concat(s"/${r.getName}")
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(r)
      val row = ContextFeatureUtils.readAggRowFromFile(pathToRow)
      if(!rowsForCurrentSent.contains(row))
      {
        idMap ++= Map(rowSpecs -> row)
        keysForLabels ++= Map(row -> rowSpecs)
        rowsForCurrentSent += row
      }
    }
    val nameOfCurrentDirectory = paperDir.getName
    val entry = (nameOfCurrentDirectory,rowsForCurrentSent)

    allRowsByPaperID += entry
  }


  println(allRowsByPaperID.size)

  val precisionScoreBoardPerPaper = collection.mutable.HashMap[String, Double]()
  val recallScoreBoardPerPaper = collection.mutable.HashMap[String, Double]()
  val f1ScoreBoardPerPaper = collection.mutable.HashMap[String, Double]()
  val giantPredictedLabels = collection.mutable.ListBuffer[Int]()
  val giantTruthLabels = collection.mutable.ListBuffer[Int]()
  val quickerFixer = 1

  for((paperID, testRowsPerPaper) <- allRowsByPaperID) {
    val svmDeclaration = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
    val svmInstance = new LinearSVMContextClassifier(Some(svmDeclaration))
    val truthLabelsForThisPaper = collection.mutable.ListBuffer[Int]()
    val predictedLabelsForThisPaper = collection.mutable.ListBuffer[Int]()
    val trainingCaseRowsUnFiltered = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val notCurrentPaper = allRowsByPaperID.filter(_._1!=paperID)
    for((notThispaperId,rowsNotInThisPaper) <- notCurrentPaper) {
      if(notThispaperId != paperID) {
        for (r <- rowsNotInThisPaper) {
          if(!trainingCaseRowsUnFiltered.contains(r))
          {
            trainingCaseRowsUnFiltered += r
          }
        }
      }

    }
    val trainingRowsWithCorrectLabels = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val trainingLabels = collection.mutable.ListBuffer[Int]()
    for(t <- trainingCaseRowsUnFiltered) {
      val specForCurrentRow = keysForLabels(t)
      val evtID = Integer.parseInt(specForCurrentRow._2)
    }
  }



  def makeLabelMapFromEventFileDir(pathToDir:String):Map[(String,String,String), Int] = {
    val labelMapFromEventFile = collection.mutable.HashMap[(String,String,String), Int]()
    val dirToRead = new File(pathToDir)
    val listOfFiles = dirToRead.listFiles()
    for(file <- listOfFiles) {
      val pmcid = file.getName.split("_")(0)
      val source = Source.fromFile(file)
      val lines = source.getLines()
      for(c <- lines) {
        val array = c.split("\t")
        if(array.size > 2) {
          val sentenceIndex = array(0)
          val tokenIntervalStart = array(1).split("-")(0)
          val tokenIntervalEnd = array(1).split("-")(1)
          val eventID = sentenceIndex.concat(tokenIntervalStart.concat(tokenIntervalEnd))
          val contextIDs = array(2).split(",")
          for(cnt <- contextIDs) {
            val tupleEntry = (pmcid,eventID,cnt)
            println(tupleEntry)
            labelMapFromEventFile ++= Map(tupleEntry -> 1)
          }
        }
      }
    }

    labelMapFromEventFile.toMap
  }


}
