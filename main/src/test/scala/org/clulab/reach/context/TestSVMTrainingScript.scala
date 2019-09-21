package org.clulab.reach.context
import java.io.{FileInputStream, ObjectInputStream}
import sys.process._
import org.clulab.context.utils.AggregatedContextInstance
import org.scalatest.{FlatSpec, Matchers}
class TestSVMTrainingScript extends FlatSpec with Matchers {
  val resourcesPath = "/inputs/aggregated-context-instance"

  val resourcesPathToDataframe = readFileNameFromResource(s"${resourcesPath}/grouped_features.csv.gz")
  val resourcePathToSpecificFeaturenames = readFileNameFromResource(s"${resourcesPath}/specific_nondependency_featurenames.txt")
  val resourcesPathToWriteSVMTo = readFileNameFromResource(s"${resourcesPath}/svm_model.dat")


  "SVM training script" should "take 3 command line parameters" in {
    //TODO
  }

  "SVM training script" should "create a .dat file to save the trained SVM model to" in {
    //TODO
  }

  "Default training dataset" should "not contain degenerate papers" in {
    //TODO
  }

  "All datapoints in the frame" should "have the same number of features" in {
    //TODO
  }

  "SVM training script" should "throw an exception if no arguments are passed" in {
    //TODO
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
