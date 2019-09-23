package org.clulab.reach.context.svm_scripts
import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.context.classifiers.{DummyClassifier, LinearSVMContextClassifier}
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.utils.svm_training_utils.{DatatypeConversionUtils, IOUtilsForFeatureName}
import org.clulab.reach.context.utils.feature_utils.FeatureNameProcessor
class TrainSVMContextClassifier(pathToDataframe:String, pathToFileToSaveSVMModel: String, pathToSpecificFeaturenames:String){

  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val svmInstance = new LinearSVMContextClassifier(Some(SVMClassifier))


  val (allFeatures, dataPoints) = IOUtilsForFeatureName.loadAggregatedRowsFromDataFrame(pathToDataframe, pathToSpecificFeaturenames)
  val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
  val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
  val featureDict = FeatureNameProcessor.createFeatureTypeDictionary(numericFeatures.toSeq)
  val bestFeatureSet = featureDict("NonDep_Context")
  val trainingDataPrior = dataPoints.filter(_.PMCID != "b'PMC4204162'")
  val trainingData = extractDataByRelevantFeatures(bestFeatureSet, trainingDataPrior)
  svmInstance.fit(trainingData)
  svmInstance.saveModel(pathToFileToSaveSVMModel)


  def extractDataByRelevantFeatures(featureSet:Seq[String], data:Seq[AggregatedContextInstance]):Seq[AggregatedContextInstance] = {
    val result = data.map(d => {
      val currentSent = d.sentenceIndex
      val currentPMCID = d.PMCID
      val currentEvtId = d.EvtID
      val currentContextID = d.CtxID
      val currentLabel = d.label
      val currentFeatureName = d.featureGroupNames
      val currentFeatureValues = d.featureGroups
      val indexList = collection.mutable.ListBuffer[Int]()
      // we need to check if the feature is present in the current row. Only if it is present should we try to access its' value.
      // if not, i.e. if the feature is not present and we try to access it, then we get an ArrayIndexOutOfBound -1 error/
      featureSet.map(f => {
        if(currentFeatureName.contains(f)) {
          val tempIndex = currentFeatureName.indexOf(f)
          indexList += tempIndex
        }
      })
      val valueList = indexList.map(i => currentFeatureValues(i))
      AggregatedContextInstance(currentSent, currentPMCID, currentEvtId, currentContextID, currentLabel, valueList.toArray, featureSet.toArray)
    })
    result
  }

  println("Done training the SVM")
}


object TrainSVMContextClassifier extends App {
    if(args.length == 0)
      throw new IllegalArgumentException("This script takes arguments from the command line to run, but none were received. Please peruse examples of usage of this script")
    val cmndLinePathToDataFrame = args(0)
    val cmndLinePathToWriteSVMTo = args(1)
    val cmndLinePathToSpecificFeatures = args(2)
    val trainSVMContextClassifier: TrainSVMContextClassifier = new TrainSVMContextClassifier(cmndLinePathToDataFrame, cmndLinePathToWriteSVMTo, cmndLinePathToSpecificFeatures)

}
