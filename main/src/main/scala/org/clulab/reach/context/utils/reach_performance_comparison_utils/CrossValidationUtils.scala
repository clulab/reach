package org.clulab.reach.context.utils.reach_performance_comparison_utils

import org.clulab.context.classifiers.{DummyClassifier, LinearSVMContextClassifier}
import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.reach.context.utils.score_utils.ScoreMetricsOfClassifier

import scala.collection.immutable.ListMap

object CrossValidationUtils {

  def performCVOnSelectedPapers(pathToUntrainedSVMInstance:String, rowsOfAggrRows:Seq[AggregatedContextInstance], reachVersion:String="Reach2019", papersToExclude:Option[List[String]]=None):(Double,Double,ListMap[String,Double])={

    val svmWrapper = new LinearSVMContextClassifier()
    val untrainedInstanceForCV = svmWrapper.loadFrom(pathToUntrainedSVMInstance)
    val classifierToCheckForNull = untrainedInstanceForCV.classifier match {
      case Some(x) => x
      case None => {
        null
      }
    }

    if(classifierToCheckForNull == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")

    println(s"The SVM model has been tuned to the following settings: C: ${classifierToCheckForNull.C}, Eps: ${classifierToCheckForNull.eps}, Bias: ${classifierToCheckForNull.bias}")
    val availablePaperIDs = rowsOfAggrRows.map(x => x.PMCID).toSet
    val papersToUseForCV = papersToExclude match {
      case Some(x) => availablePaperIDs.filter(a => !x.contains(a))
      case None => availablePaperIDs
    }

    val microAveragedTrueLabels = collection.mutable.ListBuffer[Int]()
    val microAveragedPredictedLabels = collection.mutable.ListBuffer[Int]()
    var totalAccuracy = 0.0
    val perPaperAccuracyMap = collection.mutable.HashMap[String,Double]()

    for(paperID <- papersToUseForCV){

      val testingRowsFromCurrentPaper = rowsOfAggrRows.filter(x=>x.PMCID == paperID)
      val trainingRows = rowsOfAggrRows.filter(x=>x.PMCID!=paperID)

      val trainingfeatureValues = untrainedInstanceForCV.constructTupsForRVF(trainingRows)
      val trainingLabels = DummyClassifier.getLabelsFromDataset(trainingRows)
      val (trainingDataset,_) = untrainedInstanceForCV.mkRVFDataSet(trainingLabels.toArray,trainingfeatureValues)
      untrainedInstanceForCV.fit(trainingDataset)

      val testingLabels = DummyClassifier.getLabelsFromDataset(testingRowsFromCurrentPaper)
      val predictedValuesPerTestFold = untrainedInstanceForCV.predict(testingRowsFromCurrentPaper)
      microAveragedTrueLabels ++= testingLabels
      microAveragedPredictedLabels ++= predictedValuesPerTestFold

      val accuracyPerPaper = ScoreMetricsOfClassifier.accuracy(testingLabels.toArray, predictedValuesPerTestFold)

      totalAccuracy += accuracyPerPaper
      perPaperAccuracyMap ++= Map(paperID -> accuracyPerPaper)
    }
    val microAveragedAccuracy = ScoreMetricsOfClassifier.accuracy(microAveragedTrueLabels, microAveragedPredictedLabels)

    val arithmeticMeanAccuracy = totalAccuracy/papersToUseForCV.size



    val sortedPerPaperAccuracyMap = ListMap(perPaperAccuracyMap.toSeq.sortWith(_._2>_._2):_*)


    (microAveragedAccuracy, arithmeticMeanAccuracy, sortedPerPaperAccuracyMap)
  }

}
