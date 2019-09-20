package org.clulab.reach.context.svm_scripts
import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.context.classifiers.{LinearSVMContextClassifier, DummyClassifier}
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.utils.svm_training_utils.IOUtilsForFeatureName
import org.clulab.reach.context.utils.feature_utils.FeatureNameProcessor
class TrainSVMInstance extends App {

  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val svmInstance = new LinearSVMContextClassifier(Some(SVMClassifier))
  val resourcesPath = "/org/clulab/context/svmFeatures"
  val resourcesPathToGroupedFeatures = s"${resourcesPath}/grouped_features.csv.gz"
  val resourcesPathToSpecificFeatures = s"${resourcesPath}/specific_nondependency_featurenames.txt"
  val resourcesPathToSVMModel = s"${resourcesPath}/svm_model.dat"
  val urlPathToSVMModel = getClass.getResource(resourcesPathToSVMModel)
  val truncatedPathToSVM = urlPathToSVMModel.toString.replace("file:","")
  val urlPathToGroupedFeatures = getClass.getResource(resourcesPathToGroupedFeatures)
  val urlPathToSpecificFeatures = getClass.getResource(resourcesPathToSpecificFeatures)
  val truncatedPathToGroupedFeatures = urlPathToGroupedFeatures.toString.replace("file:","")
  val truncatedPathToSpecificFeatures = urlPathToSpecificFeatures.toString.replace("file:","")
  val (allFeatures, dataPoints) = IOUtilsForFeatureName.loadAggregatedRowsFromFile(truncatedPathToGroupedFeatures, truncatedPathToSpecificFeatures)
  val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
  val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
  val featureDict = FeatureNameProcessor.createFeatureTypeDictionary(numericFeatures.toSeq)
  val bestFeatureSet = featureDict("NonDep_Context")
  val trainingDataPrior = dataPoints.filter(_.PMCID != "b'PMC4204162'")
  val trainingData = extractDataByRelevantFeatures(bestFeatureSet, trainingDataPrior)
  val trainingLabels = DummyClassifier.convertOptionalToBool(trainingData)
  val labelsToInt = DummyClassifier.convertBooleansToInt(trainingLabels)
  val tups = svmInstance.constructTupsForRVF(trainingData)
  val (trainDataSet, _) = svmInstance.mkRVFDataSet(labelsToInt,tups)
  svmInstance.fit(trainDataSet)
  svmInstance.saveModel(truncatedPathToSVM)


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
}
