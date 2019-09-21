package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier

object SVMPerformanceOnNewReach extends App {
  val svmWrapper = new LinearSVMContextClassifier()
  val config = ConfigFactory.load()


  val configPath = config.getString("contextEngine.params.trainedSvmPath")
  println(configPath)
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }

  println(classifierToUse == null)

  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")
  val labelFile = config.getString("svmContext.labelFileOldDataset")
}
