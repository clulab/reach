package org.clulab.reach.context.context_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}

object SVMCrossValidation extends App {

  // step 1: load trained model
  // step 2: load labels.csv i.e. "gold standard" for new annotated data
  // step 3: load aggregated rows written to file in polarityContext.aggrRowWrittenToFilePerPaperNewAnnotations
  // step 4: over each paper, find the prediction for the row. If it is 1, add to the list to find the micro-averaged precision
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

  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")
  val labelFile = config.getString("svmContext.labelFile")
  val labelMapFromOldDataset = CodeUtils.generateLabelMap(labelFile)
  val parentDirForRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaperNewAnnotations")
  val allPapersDirs = new File(parentDirForRows).listFiles().filter(_.isDirectory)
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val allRowsByPaperID = collection.mutable.HashMap[String, Seq[AggregatedContextInstance]]()

}
