package org.clulab.reach.context.scripts


import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.{DummyClassifier, LinearSVMContextClassifier}
import org.clulab.reach.context.utils.io_utils.SVMTrainingIOUtils
import org.clulab.reach.context.utils.reach_performance_comparison_utils.CrossValidationUtils
import org.clulab.reach.context.utils.score_utils.ScoreMetricsOfClassifier

import scala.collection.immutable.ListMap
object PerformanceComparisonOfReachVersions extends App {
  val config = ConfigFactory.load()

  // cross validation to test accuracy of SVM

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


  val pathToReach2019Dataset = config.getString("svmContext.groupedFeaturesTransferredAnnotations")
  val hardCodedFeaturePath = config.getString("contextEngine.params.hardCodedFeatures")
  val (_,rowsFromReach2019) = SVMTrainingIOUtils.loadAggregatedRowsFromFile(pathToReach2019Dataset, hardCodedFeaturePath)
  val papersToExcludeFromCV = List("PMC2195994", "PMC2193052", "PMC2156142")

  val pathToReach2016Dataset = config.getString("svmContext.groupedFeatures")
  val (_,rowsFromReach2016) = SVMTrainingIOUtils.loadAggregatedRowsFromFile(pathToReach2016Dataset, hardCodedFeaturePath)
  val (microAccuracyReach2019, meanAccuracyReach2019, sortedMapReach2019) = CrossValidationUtils.performCVOnSelectedPapers(pathToUntrainedSVM, rowsFromReach2019, "Reach2019", Some(papersToExcludeFromCV))
  println(s"micro accuracy from 2019: ${microAccuracyReach2019}")
  println(s"Mean accuracy from 2019: ${meanAccuracyReach2019}")
  println(s"Per paper score map: ${sortedMapReach2019}")




  // Baseline comparison





}
