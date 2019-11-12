package org.clulab.reach.context.scripts


import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.reach.context.utils.io_utils.SVMTrainingIOUtils
import org.clulab.reach.context.utils.reach_performance_comparison_utils.CrossValidationUtils
import org.clulab.reach.context.utils.score_utils.ScoreMetricsOfClassifier

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

  val (allFeaturesReach2019,rowsFromReach2019) = SVMTrainingIOUtils.loadAggregatedRowsFromFile(pathToReach2019Dataset, hardCodedFeaturePath)
  val bestFeatureSet = CrossValidationUtils.getBestFeatureSet(allFeaturesReach2019)
  val papersToExcludeFromCV = List("PMC2195994", "b'PMC2195994'","PMC2193052", "b'PMC2193052'","PMC2156142", "b'PMC2156142'","b'PMC4204162'", "PMC4204162")

  val pathToReach2016Dataset = config.getString("svmContext.groupedFeatures")
  val (allFeaturesReach2016,rowsFromReach2016) = SVMTrainingIOUtils.loadAggregatedRowsFromFile(pathToReach2016Dataset, hardCodedFeaturePath)
  val bestFeatureDatasetReach2016 = CrossValidationUtils.extractDataByRelevantFeatures(bestFeatureSet, rowsFromReach2016)
  println(s"Calling CV on new reach")
  val (reach2019TrueValues, reach2019PredictedValues) = CrossValidationUtils.performCVOnSelectedPapers(pathToUntrainedSVM, rowsFromReach2019, Some(papersToExcludeFromCV),reachVersion = "reach2019")
  val microAccuracyReach2019 = ScoreMetricsOfClassifier.accuracy(reach2019TrueValues, reach2019PredictedValues)
  println(s"micro accuracy from 2019: ${microAccuracyReach2019}")
  val microPrecision2019 = ScoreMetricsOfClassifier.precision(reach2019TrueValues, reach2019PredictedValues)
  println(s"Precision: ${microPrecision2019}")
//  println(s"Mean accuracy from 2019: ${meanAccuracyReach2019}")
//  //println(s"Per paper score map: ${sortedMapReach2019}")


  println(s"Calling CV on old reach")
  val (reach2016TrueValues, reach2016PredictedValues) = CrossValidationUtils.performCVOnSelectedPapers(pathToUntrainedSVM, bestFeatureDatasetReach2016, Some(papersToExcludeFromCV), reachVersion = "reach2016")
  val microAccuracyReach2016 = ScoreMetricsOfClassifier.accuracy(reach2016TrueValues, reach2016PredictedValues)
  val microF1 = ScoreMetricsOfClassifier.f1(reach2016TrueValues, reach2016PredictedValues)
  val microPrecision = ScoreMetricsOfClassifier.precision(reach2016TrueValues, reach2016PredictedValues)
  val microRecall = ScoreMetricsOfClassifier.recall(reach2016TrueValues, reach2016PredictedValues)
  println(s"Reach 2016")
  println(s"micro accuracy ${microAccuracyReach2016}, micro precision: ${microPrecision}, recall: ${microRecall}, f1: ${microF1}")

//  println(s"Mean accuracy from 2016: ${meanAccuracyReach2016}")
  //println(s"Per paper score map: ${sortedMapReach2016}")





  val featuresMissingFromBestFeatureSet = allFeaturesReach2019.toSet -- bestFeatureSet.toSet

  println(s"${featuresMissingFromBestFeatureSet.mkString(",")}")



  // Baseline comparison





}
