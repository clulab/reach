package org.clulab.reach.context.scripts


import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.reach.context.utils.io_utils.SVMTrainingIOUtils

object CrossValidationForSVMPerformanceOnNewReach extends App {
  val config = ConfigFactory.load()

  val pathToUntrainedSVM = config.getString("svmContext.untrainedSVMPath")
  val svmWrapper = new LinearSVMContextClassifier()
  val untrainedInstanceForCV = svmWrapper.loadFrom(pathToUntrainedSVM)
  val classifierToCheckForNull = untrainedInstanceForCV.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }


  if(classifierToCheckForNull == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")

  println(s"The SVM model has been tuned to the following settings: C: ${classifierToCheckForNull.C}, Eps: ${classifierToCheckForNull.eps}, Bias: ${classifierToCheckForNull.bias}")


  val groupedFeatures = config.getString("svmContext.groupedFeaturesTransferredAnnotations")
  val hardCodedFeaturePath = config.getString("contextEngine.params.hardCodedFeatures")
  val (allFeatures,rows) = SVMTrainingIOUtils.loadAggregatedRowsFromFile(groupedFeatures, hardCodedFeaturePath)
  println(rows.size)



}
