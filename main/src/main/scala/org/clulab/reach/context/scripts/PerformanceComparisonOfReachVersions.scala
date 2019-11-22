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

  val pathToReach2016Dataset = config.getString("svmContext.groupedFeaturesMatchingAnnotationsReach2016")
  val (allFeaturesReach2016,rowsFromReach2016) = SVMTrainingIOUtils.loadAggregatedRowsFromFile(pathToReach2016Dataset, hardCodedFeaturePath)
  val bestFeatureDatasetReach2016 = CrossValidationUtils.extractDataByRelevantFeatures(bestFeatureSet, rowsFromReach2016)

  println(s"Calling CV on Reach 2019")
  val (reach2019TrueValues, reach2019PredictedValues) = CrossValidationUtils.performCVOnSelectedPapers(pathToUntrainedSVM, rowsFromReach2019, Some(papersToExcludeFromCV),reachVersion = "reach2019")


  println(s"Calling CV on Reach 2016")
  val (reach2016TrueValues, reach2016PredictedValues) = CrossValidationUtils.performCVOnSelectedPapers(pathToUntrainedSVM, bestFeatureDatasetReach2016, Some(papersToExcludeFromCV), reachVersion = "reach2016")

  val reach2019ScoreDict = getScores(reach2019TrueValues,reach2019PredictedValues)
  println(s"Printing cross-validation scores from Reach 2019")
  println(reach2019ScoreDict)


  val reach2016ScoreDict = getScores(reach2016TrueValues,reach2016PredictedValues)
  println(s"Printing cross-validation scores from Reach 2016")
  println(reach2016ScoreDict)



  // Baseline comparison

  val (baselineTrueLabelsReach2019,baselinePredictedLabelsReach2019) = CrossValidationUtils.performBaselineCheck(rowsFromReach2019,Some(papersToExcludeFromCV))
  val (baselineTrueLabelsReach2016,baselinePredictedLabelsReach2016) = CrossValidationUtils.performBaselineCheck(rowsFromReach2016,Some(papersToExcludeFromCV),"reach2016")

  val baselineReach2019Score = getScores(baselineTrueLabelsReach2019,baselinePredictedLabelsReach2019)
  println(s"Printing baseline score from Reach 2019")
  println(baselineReach2019Score)

  val baselineReach2016Score = getScores(baselineTrueLabelsReach2016,baselinePredictedLabelsReach2016)
  println(s"Printing baseline score from Reach 2016")
  println(baselineReach2016Score)


  def getScores(trueValues:Seq[Int],predictedValues:Seq[Int]):Map[String,Double] = {
    val accuracy = ScoreMetricsOfClassifier.accuracy(trueValues,predictedValues)
    val precision = ScoreMetricsOfClassifier.precision(trueValues,predictedValues)
    val recall = ScoreMetricsOfClassifier.recall(trueValues,predictedValues)
    val f1 = ScoreMetricsOfClassifier.f1(trueValues,predictedValues)
    Map("f1"->f1,"precision"->precision,"recall"->recall,"accuracy"->accuracy)
  }

}
