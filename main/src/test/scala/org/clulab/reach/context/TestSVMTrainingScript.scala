package org.clulab.reach.context
import java.io.{File, FileInputStream, ObjectInputStream}

import sys.process._
import org.clulab.reach.context.svm_scripts.TrainSVMContextClassifier

import scala.language.postfixOps

import org.clulab.context.utils.AggregatedContextInstance
import org.scalatest.{FlatSpec, Matchers}
class TestSVMTrainingScript extends FlatSpec with Matchers {




  "SVM training script" should "create a .dat file to save the trained SVM instance to" in {
    val resourcesPath = "/inputs/aggregated-context-instance"
    val resourcePathToDataFrame = s"${resourcesPath}/grouped_features.csv.gz"
    val urlPathToDataframe = readFileNameFromResource(resourcePathToDataFrame)
    val resourcePathToSpecificFeatures = s"${resourcesPath}/specific_nondependency_featurenames.txt"
    val urlPathToSpecificFeaturenames = readFileNameFromResource(resourcePathToSpecificFeatures)
    // creating the path to where the output svm_model should be, using the ready URL path from another file in the same location
    val pathToSVMModelToTest = urlPathToSpecificFeaturenames.replace("specific_nondependency_featurenames.txt","svm_model_temp.dat")
    new TrainSVMContextClassifier(urlPathToDataframe,pathToSVMModelToTest,urlPathToSpecificFeaturenames)
    val svmModelFile = new File(pathToSVMModelToTest)
    val checkIfDatFileExists = svmModelFile.exists()
    checkIfDatFileExists should be (true)
    svmModelFile.deleteOnExit()
  }

  def readFileNameFromResource(resourcePath: String):String = {
    val url = getClass.getResource(resourcePath)
    val truncatedPathToSVM = url.toString.replace("file:","")
    truncatedPathToSVM
  }

}
