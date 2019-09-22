package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import java.io.File

import org.clulab.context.utils.AggregatedContextInstance

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

  val pathTodirToLoadNewRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val fileInstanceToLoadNewRows = new File(pathTodirToLoadNewRows)
  val paperDirs = fileInstanceToLoadNewRows.listFiles().filter(x => x.isDirectory && x.getName.startsWith("PMC"))
  val paperIDByNewRowsMap = collection.mutable.HashMap[String, Seq[AggregatedContextInstance]]()


}
