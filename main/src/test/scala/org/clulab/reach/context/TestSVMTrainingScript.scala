package org.clulab.reach.context
import java.io.{FileInputStream, ObjectInputStream, File}
import sys.process._
import org.clulab.reach.context.svm_scripts.TrainSVMContextClassifier
import scala.language.postfixOps
import org.clulab.utils.Files

import org.clulab.context.utils.AggregatedContextInstance
import org.scalatest.{FlatSpec, Matchers}
class TestSVMTrainingScript extends FlatSpec with Matchers {




  "SVM training script" should "create a .dat file to save the trained SVM instance to" in {
    val resourcesPath = "/inputs/aggregated-context-instance"
    val resourcePathToDataFrame = s"${resourcesPath}/grouped_features.csv.gz"
    val urlPathToDataframe = readFileNameFromResource(resourcePathToDataFrame)
    val resourcePathToSpecificFeatures = s"${resourcesPath}/specific_nondependency_featurenames.txt"
    val urlPathToSpecificFeaturenames = readFileNameFromResource(resourcePathToSpecificFeatures)



    val tempDirToTestDat = new File(Files.mkTmpDir(s"testFileCreationByTrain",deleteOnExit=true))
    val absolutePathToTempDir = tempDirToTestDat.getAbsolutePath
    // creating the path to where the output svm_model should be
    val pathToSVMModelToTest = urlPathToSpecificFeaturenames.replace("specific_nondependency_featurenames.txt","svm_model_temp.dat")
    val trainSVMContextInstance = new TrainSVMContextClassifier(urlPathToDataframe,pathToSVMModelToTest,urlPathToSpecificFeaturenames)
    val checkIfDatFileExists = new File(pathToSVMModelToTest).exists()
    checkIfDatFileExists should be (true)
  }

  "SVM training script" should "throw an exception if no arguments are passed" in {
    val commandLineScriptWithoutParams = s"'run-main org.clulab.reach.context.svm_scripts.TrainSVMInstance'"
    val resultThrowsException = Seq("sbt",commandLineScriptWithoutParams).!
    resultThrowsException should be (1)
  }


  def readFileNameFromResource(resourcePath: String):String = {
    val url = getClass.getResource(resourcePath)
    val truncatedPathToSVM = url.toString.replace("file:","")
    truncatedPathToSVM
  }

  def readAggRowFromFile(fileName: String):AggregatedContextInstance = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[AggregatedContextInstance]
    is.close()
    c
  }
}
