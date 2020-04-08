package org.clulab.reach.context.svm_scripts
import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.context.classifiers.{DummyClassifier, LinearSVMContextClassifier}
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.utils.svm_training_utils.{DatatypeConversionUtils, IOUtilsForFeatureName}
import org.clulab.reach.context.utils.feature_utils.FeatureNameProcessor
class TrainSVMContextClassifier(pathToDataframe:String, pathToFileToSaveSVMModel: String, pathToSpecificFeaturenames:String){

  // The SVM instance is set up for these hyper-parameters, the values for which were found by the Reach team through trial and error, to yield the best scores without losing generalizability.
  // You can change the values of the hyper-parameters to this classifier instance before you run the script
  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val svmInstance = new LinearSVMContextClassifier(Some(SVMClassifier))


  val (allFeatures, dataPoints) = IOUtilsForFeatureName.loadAggregatedRowsFromDataFrame(pathToDataframe, pathToSpecificFeaturenames)
  val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
  val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
  val featureDict = FeatureNameProcessor.createFeatureTypeDictionary(numericFeatures.toSeq)
  // the best feature set was found through an ablation study, wherein we ran all possible combinations of features, and examining what combination yielded the best scores.
  // The resulting best feature set was to be the collection of non-dependency and context dependency features.
  val bestFeatureSet = featureDict("NonDep_Context")


  // It was found that the paper PMC4204162 had no "root" to tree as per the stanford.nlp  package, and hence filtered the rows out.
  // if you choose to remove this filter, please note your SVM might have a different performance than observed by the team.
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


object TrainSVMContextClassifier extends App {
    if(args.length == 0)
      throw new IllegalArgumentException("This script takes arguments from the command line to run, but none were received. Please peruse examples of usage of this script")

    // The purpose of this script is to train an SVM instance on a dataset chosen by the user, and write to file the trained version of the SVM instance.

    // The SVM instance has been tuned to LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
    // If you wish to change it, you can change it in the script before running it.

    // To run this script, you will need three files:
    // 1) the data set you want to use for training (please note that the code is set up to take .csv.gz file format for the dataset)
    // 2) The path to the file where you want the trained SVM instance to be written to file. The file written will be in the format .dat
    // 3) The list of specific feature names that identify your datapoint, and any other numerical features that have more specific feature values, such as classification of a given datapoint.


    // usage of script:
    // You can run the following line as an sbt command directly in your terminal, while you're in the root directory of the project
    // sbt 'run-main org.clulab.reach.context.svm_scripts.TrainSVMContextClassifier /../..path/to/dataset.csv.gz ../../path/to/output_model.dat ../path/to/specific_features.txt'
    // The dataset.csv.gz file needs to exist. dataset is a custom name to the file, you can name your dataset how you choose.
    // The output_model.dat file does *not* need to exist, this script will create the file for you with that name. You can name the file to your liking, but the output format will always be a .dat file
    // The specific_features.txt *needs* to exist. The typical entries that go into this file are any identifiers of your datapoint, or any features that are *non-numeric*, such as identifiers.
    // This 3rd file is a .txt file, that has the names of the "specific" features, separated by commas

    val cmndLinePathToDataFrame = args(0)
    val cmndLinePathToWriteSVMTo = args(1)
    val cmndLinePathToSpecificFeatures = args(2)
    val trainSVMContextClassifier: TrainSVMContextClassifier = new TrainSVMContextClassifier(cmndLinePathToDataFrame, cmndLinePathToWriteSVMTo, cmndLinePathToSpecificFeatures)


    // If you choose to use this script programmatically, i.e., manually create the instance of the training code, you can do so using the following line:
    // val svmTrainingInstance = new TrainSVMContextClassifier(dataset.csv.gz, output_model.dat, specific_features.txt)
    // Please refer to the corresponding test script for a more detailed example on the programmatic usage of the trainer.
}
