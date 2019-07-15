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
  val inputRowsPath = config.getString(("polarityContext.attemptDir"))
  val inputRowsFile = new File(inputRowsPath)
  val attemptByDataset = collection.mutable.HashMap[Int, RVFDataset[Int, String]]()
  val dirsInPath = inputRowsFile.listFiles.filter(_.isDirectory)
  for(d <- dirsInPath) {
    val rvfFileName = inputRowsPath.concat(d.getName.concat("/AggregRowsToFile.txt"))
    val ArrayOfAggrRows = ContextFeatureUtils.readAggRowsFromFile(rvfFileName)
    //trainedSVMInstance.predict(rvfDataset)
  }



}
