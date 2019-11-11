package org.clulab.reach.context.scripts


import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.{DummyClassifier, LinearSVMContextClassifier}
import org.clulab.reach.context.utils.io_utils.SVMTrainingIOUtils
import org.clulab.reach.context.utils.score_utils.ScoreMetricsOfClassifier

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
  val papersToExcludeFromCV = List("PMC2195994", "PMC2193052", "PMC2156142")
  val paperIDsetFromAllPapers = rows.map(x => x.PMCID).toSet
  val papersToUseForCV = paperIDsetFromAllPapers.filter(!papersToExcludeFromCV.contains(_))
  println(s"Total number of datapoints: ${rows.size}")
  val microAveragedTrueLabels = collection.mutable.ListBuffer[Int]()
  val microAveragedPredictedLabels = collection.mutable.ListBuffer[Int]()
  var totalAccuracy = 0.0
  for(paperID <- papersToUseForCV){

    val testingRowsFromCurrentPaper = rows.filter(x=>x.PMCID == paperID)
    val trainingRows = rows.filter(x=>x.PMCID!=paperID)

    val trainingfeatureValues = untrainedInstanceForCV.constructTupsForRVF(trainingRows)
    val trainingLabels = DummyClassifier.getLabelsFromDataset(trainingRows)
    val (trainingDataset,_) = untrainedInstanceForCV.mkRVFDataSet(trainingLabels.toArray,trainingfeatureValues)
    untrainedInstanceForCV.fit(trainingDataset)

    val testingLabels = DummyClassifier.getLabelsFromDataset(testingRowsFromCurrentPaper)
    val predictedValuesPerTestFold = untrainedInstanceForCV.predict(testingRowsFromCurrentPaper)
    microAveragedTrueLabels ++= testingLabels
    microAveragedPredictedLabels ++= predictedValuesPerTestFold
    println(s"The number of labels in current test fold: ")
    println(s"True labels: ${testingLabels.size}, predicted labels: ${predictedValuesPerTestFold.size}")
    val accuracyPerPaper = ScoreMetricsOfClassifier.accuracy(testingLabels.toArray, predictedValuesPerTestFold)
    println(s"the current test case is ${paperID}")
    println(s"The accuracy for this paper is: ${accuracyPerPaper}")
    totalAccuracy += accuracyPerPaper
  }

  println(s"We have a total of ${microAveragedTrueLabels.size} true labels")
  println(s"We have a total of ${microAveragedPredictedLabels.size} predicted labels")
  val microAveragedAccuracy = ScoreMetricsOfClassifier.accuracy(microAveragedTrueLabels, microAveragedPredictedLabels)
  println(s"Micro averaged accuracy: ${microAveragedAccuracy}")
  val arithmeticMeanAccuracy = totalAccuracy/microAveragedPredictedLabels.size
  println(s"The arithmetic mean accuracy is ${arithmeticMeanAccuracy}")



}
