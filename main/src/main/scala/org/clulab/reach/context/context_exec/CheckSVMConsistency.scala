package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.learning._
import java.io.{File, PrintWriter}

import org.clulab.context.utils.CodeUtils
import org.clulab.reach.context.context_utils.ContextFeatureUtils
object CheckSVMConsistency extends App{
  val config = ConfigFactory.load()
  val svmWrapper = new LinearSVMContextClassifier()
  val configPath = config.getString("contextEngine.params.trainedSvmPath")
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }
  //RVFDatasetToFile.txt
  val aggrRowsPath = config.getString(("polarityContext.attemptDir")).concat("/AggrRowPredictions.txt")
  val aggrRowsPredsFile = new File(aggrRowsPath)
  if(!aggrRowsPredsFile.exists())
    aggrRowsPredsFile.createNewFile()
  val printWriter = new PrintWriter(aggrRowsPredsFile)
  val whereAggrPath = config.getString("polarityContext.attemptDir").concat("/AggregRowsToFile.txt")
  val arrayOfAggrRows = ContextFeatureUtils.readAggRowsFromFile(whereAggrPath)
  for(((eventID, ctxID), row) <- arrayOfAggrRows) {
    val prediction = trainedSVMInstance.predict(Seq(row))
    printWriter.write(s"The pair ${eventID}, ${ctxID} has the prediction ${prediction(0)} \n")
  }

  /*val dirsInPath = inputRowsFile.listFiles.filter(_.isDirectory)

  for(d <- dirsInPath) {
    val rvfFileName = inputRowsPath.concat(d.getName.concat("/AggregRowsToFile.txt"))
    val arrayOfAggrRows = ContextFeatureUtils.readAggRowsFromFile(rvfFileName)
    for(((eventID, ctxID),row) <- arrayOfAggrRows) {
      val prediction = trainedSVMInstance.predict(Seq(row))

    }
    //trainedSVMInstance.predict(rvfDataset)
  }*/



}
