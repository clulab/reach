package org.clulab.reach.context.scripts

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.{DummyClassifier, LinearSVMContextClassifier}
import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.utils.io_utils.SVMTrainingIOUtils
import org.clulab.reach.context.utils.reach_performance_comparison_utils.CrossValidationUtils

object SVMTrainSaveInstance extends App {
  //data preprocessing
  // This script allows you to train your SVM on your dataset. You can adjust the hyperparameters to get different performances.
  val config = ConfigFactory.load()
  val fileName = config.getString("contextEngine.params.trainedSvmPath")
  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val svmInstance = new LinearSVMContextClassifier(Some(SVMClassifier))
  val groupedFeatures = config.getString("svmContext.groupedFeatures")
  val hardCodedFeaturePath = config.getString("contextEngine.params.hardCodedFeatures")
  val (allFeatures,rows) = SVMTrainingIOUtils.loadAggregatedRowsFromFile(groupedFeatures, hardCodedFeaturePath)
  val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
  val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
  val featureDict = CrossValidationUtils.createFeaturesLists(numericFeatures.toSeq)
  //val bestFeatureSet = featureDict("NonDep_Context")
  val bestFeatureSet = featureDict("NonDep_Context")
  val trainingDataPrior = rows.filter(_.PMCID != "b'PMC4204162'")
  val trainingData = CrossValidationUtils.extractDataByRelevantFeatures(bestFeatureSet, trainingDataPrior)

  // training the machine learning model and writing it to file
  val trainingLabels = DummyClassifier.convertOptionalToBool(trainingData)
  val labelsToInt = DummyClassifier.convertBooleansToInt(trainingLabels)
  val tups = svmInstance.constructTupsForRVF(trainingData)
  val (trainDataSet, _) = svmInstance.mkRVFDataSet(labelsToInt,tups)
  svmInstance.fit(trainDataSet)
  svmInstance.saveModel(fileName)


}
