package org.clulab.reach.context
import java.io.{FileInputStream, ObjectInputStream}
import sys.process._
import org.clulab.reach.context.svm_scripts.TrainSVMInstance
import scala.language.postfixOps
import org.clulab.context.utils.AggregatedContextInstance
import org.scalatest.{FlatSpec, Matchers}
class TestSVMTrainingScript extends FlatSpec with Matchers {
  val resourcesPath = "/inputs/aggregated-context-instance"
  val resourcePathToDataFrame = s"${resourcesPath}/grouped_features.csv.gz"
  val urlPathToDataframe = readFileNameFromResource(resourcePathToDataFrame)
  val resourcePathToSpecificFeatures = s"${resourcesPath}/specific_nondependency_featurenames.txt"
  val urlPathToSpecificFeaturenames = readFileNameFromResource(resourcePathToSpecificFeatures)
//  val resourcesPathToSVMOutFile = s"${resourcesPath}/svm_model_from_train_script.dat"
//  val urlPathToWriteSVMOutFile = readFileNameFromResource(resourcesPathToSVMOutFile)

  val params = Seq(s"${urlPathToDataframe}", s"${urlPathToSpecificFeaturenames}")
  val commandLineScriptWithoutParams = s"'run-main org.clulab.reach.context.svm_scripts.TrainSVMInstance'"

  "SVM training script" should "create a .dat file to save the trained SVM model to" in {
    TrainSVMInstance.insidePrint()
    val seqOfCommandsToTrain = Seq("sbt",commandLineScriptWithoutParams) ++ params
    val listOfFilesFromScriptRun = seqOfCommandsToTrain.!
    println(listOfFilesFromScriptRun)
    val seqOfCommandsToListFile = Seq("ls","grep","svm_model_from_train_script.dat").!
    seqOfCommandsToListFile should be (0)

  }

  "SVM training script" should "throw an exception if no arguments are passed" in {
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
