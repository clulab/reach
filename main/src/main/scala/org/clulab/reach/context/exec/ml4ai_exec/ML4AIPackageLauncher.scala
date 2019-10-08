package org.clulab.context.exec

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.FoldMaker
import org.clulab.context.utils.CrossValidationUtils
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.utils.io_utils.{ReachPredictionDataTypeIOUtils, SVMTrainingIOUtils}
import org.clulab.reach.context.utils.score_utils.ScoreMetricsOfClassifier

import scala.io.Source

object ML4AIPackageLauncher extends App {

  // *** DATA LOADING *****===========
  val config = ConfigFactory.load()
  val foldsPath = config.getString("svmContext.folds")
  val allFeatsPath = config.getString("contextEngine.params.allFeatures")
  val groupedFeaturesPath = config.getString("svmContext.groupedFeatures")
  val hardCodedFeaturePath = config.getString("contextEngine.params.hardCodedFeatures")
  SVMTrainingIOUtils.writeHardcodedFeaturesToFile(hardCodedFeaturePath)
  val (allFeatures,rows) = SVMTrainingIOUtils.loadAggregatedRowsFromFile(groupedFeaturesPath, hardCodedFeaturePath)
  ReachPredictionDataTypeIOUtils.writeAllFeaturesToFile(allFeatures, allFeatsPath)
  val rows2 = rows.filter(_.PMCID != "b'PMC4204162'")
  val bufferedFoldIndices = Source.fromFile(foldsPath)
  val foldsFromCSV = FoldMaker.getFoldsPerPaper(bufferedFoldIndices)
  val trainValCombined = CrossValidationUtils.combineTrainVal(foldsFromCSV)

  // =========================== BASELINE RESULTS ===========================
  // baseline results
  // The F1 score seems to have changed, perform debugging. It should be 0.5333
  var scoreDictionary = collection.mutable.Map[String, (String, Double, Double, Double)]()
  /*val (truthTest, predTest) = FoldMaker.baselineController(foldsFromCSV, rows2)
  val baselineResults = Utils.scoreMaker("baseline", truthTest, predTest)
  scoreDictionary ++= baselineResults*/
  //========================== CONCLUDING BASELINE RESULTS ==========================


  // =========================== LINEAR SVM RESULTS ===========================
  val fileName = config.getString("svmContext.untrainedSVMPath")

  // svm instance using liblinear
  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val svmInstance = new LinearSVMContextClassifier(Some(SVMClassifier))
  svmInstance.saveModel(fileName)
  val loadedModelWrapper = svmInstance.loadFrom(fileName)
  // the loadedModel variable is an instance of LinearSVMWrapper. If you want access to the LinearSVMClassifier instance,
  // you need to just call loadedModel.classifier.
  val (truthTestSVM, predTestSVM) = FoldMaker.svmControllerLinearSVM(loadedModelWrapper, trainValCombined, rows2)
  val svmResult = ScoreMetricsOfClassifier.scoreMaker("Linear SVM", truthTestSVM, predTestSVM)
  scoreDictionary ++= svmResult
  //========================== CONCLUDING LINEAR SVM RESULTS ==========================

  println("size of score dictionary: " + scoreDictionary.size)
  println(scoreDictionary)


}
