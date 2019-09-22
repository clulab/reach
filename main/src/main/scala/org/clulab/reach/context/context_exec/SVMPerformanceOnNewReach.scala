package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import java.io.File

import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.reach.context.context_utils.ContextFeatureUtils

object SVMPerformanceOnNewReach extends App {
  val svmWrapper = new LinearSVMContextClassifier()
  val config = ConfigFactory.load()


  val configPath = config.getString("contextEngine.params.trainedSvmPath")
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }


  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")
  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val labelMap = CodeUtils.generateLabelMap(labelFile).toSeq
  println(labelMap)
  val specsByRow = collection.mutable.HashMap[AggregatedContextInstance, (String,String,String)]()
  val pathToParentdirToLoadNewRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val parentDirfileInstanceToLoadNewRows = new File(pathToParentdirToLoadNewRows)
  val paperDirs = parentDirfileInstanceToLoadNewRows.listFiles().filter(x => x.isDirectory && x.getName.startsWith("PMC"))
  val paperIDByNewRowsMap = collection.mutable.HashMap[String, Seq[AggregatedContextInstance]]()
  for (paperDir <- paperDirs) {
    val listOfRowsInPaper = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val paperID = paperDir.getName
    val rowFilesInThisPaper = paperDir.listFiles().filter(_.getName.startsWith("Aggreg"))
    for(rowFile <- rowFilesInThisPaper) {
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(rowFile)
      val row = ContextFeatureUtils.readAggRowFromFile(rowFile)
      if(!listOfRowsInPaper.contains(row)) {
        listOfRowsInPaper += row
        specsByRow ++= Map(row -> rowSpecs)
      }
    }
    paperIDByNewRowsMap ++= Map(paperID -> listOfRowsInPaper)
  }



}
