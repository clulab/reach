package org.clulab.reach


import org.clulab.reach.context.ContextEngineFactory.Engine.{Engine, SVMPolicy}
import org.clulab.context.utils.AggregatedContextInstance
import org.scalatest.{FlatSpec, Matchers}
import java.io.{FileInputStream, ObjectInputStream}

import org.clulab.context.classifiers.LinearSVMContextClassifier

class TestSVMContext extends FlatSpec with Matchers {


  val resourcesPath = "/inputs/aggregated-context-instance"


  val svmWrapper = new LinearSVMContextClassifier()
  val svmInstancePath = s"${resourcesPath}/svm_model.dat"
  val urlPathToSVMModel = readAndTruncateFileName(svmInstancePath)
  val trainedSVMInstance = svmWrapper.loadFrom(urlPathToSVMModel)
  val pair1 = "PMC3411611,in233from9to11,tissuelist:TS-1224" //prediction is 1
  val resourcesPathToPair1 = s"${resourcesPath}/PMC3411611/AggregatedRow_PMC3411611_in233from9to11_tissuelist:TS-1224.txt"
  val urlPathToPair1 = readAndTruncateFileName(resourcesPathToPair1)
  val rowForPair1 = readAggRowFromFile(urlPathToPair1)


  pair1 should "have prediction 1" in {
    val pred = trainedSVMInstance.predict(Seq(rowForPair1))(0)
    pred should be (1)
  }

  pair1 should "have sentenceDistance_min of 2" in {
    val sentenceDistance_min = rowForPair1.featureGroups(rowForPair1.featureGroupNames.indexOf("sentenceDistance_min"))
    sentenceDistance_min should be (2.0)
  }

  pair1 should "have dependencyDistance_min of 6" in {
    val sentenceDistance_min = rowForPair1.featureGroups(rowForPair1.featureGroupNames.indexOf("dependencyDistance_min"))
    sentenceDistance_min should be (11.0)
  }


  pair1 should "have contextFrequency_min of 33" in {
    val sentenceDistance_min = rowForPair1.featureGroups(rowForPair1.featureGroupNames.indexOf("context_frequency_min"))
    sentenceDistance_min should be (33.0)
  }

  pair1 should "have closestContextOfClass_min of 0" in {
    val sentenceDistance_min = rowForPair1.featureGroups(rowForPair1.featureGroupNames.indexOf("closesCtxOfClass_min"))
    sentenceDistance_min should be (0.0)
  }




  val pair2 = "PMC3411611,in15from22to23,tissuelist:TS-0649"
  val resourcesPathToPair2 = s"${resourcesPath}/PMC3411611/AggregatedRow_PMC3411611_in15from22to23_tissuelist:TS-0649.txt"
  val urlPathToPair2 = readAndTruncateFileName(resourcesPathToPair2)
  val rowForPair2 = readAggRowFromFile(urlPathToPair2)

  pair2 should "have prediction 0" in {
    val pred = trainedSVMInstance.predict(Seq(rowForPair2))(0)
    pred should be (0)
  }


  pair2 should "have sentenceDistance_min of 1" in {
    val sentenceDistance_min = rowForPair2.featureGroups(rowForPair2.featureGroupNames.indexOf("sentenceDistance_min"))
    sentenceDistance_min should be (1.0)
  }

  pair2 should "have dependencyDistance_min of 2" in {
    val sentenceDistance_min = rowForPair2.featureGroups(rowForPair2.featureGroupNames.indexOf("dependencyDistance_min"))
    sentenceDistance_min should be (2.0)
  }


  pair2 should "have contextFrequency_min of 1" in {
    val sentenceDistance_min = rowForPair2.featureGroups(rowForPair2.featureGroupNames.indexOf("context_frequency_min"))
    sentenceDistance_min should be(1.0)

  }

  pair2 should "have closestContextOfClass_min of 1" in {
    val sentenceDistance_min = rowForPair2.featureGroups(rowForPair2.featureGroupNames.indexOf("closesCtxOfClass_min"))
    sentenceDistance_min should be (1.0)
  }


  val pair3 = "PMC3608085,in195from6to12,tissuelist:TS-0500"
  val resourcesPathToPair3 = s"${resourcesPath}/PMC3608085/AggregatedRow_PMC3608085_in195from6to12_tissuelist:TS-0500.txt"
  val urlPathToPair3 = readAndTruncateFileName(resourcesPathToPair3)
  val rowForPair3 = readAggRowFromFile(urlPathToPair3)

  pair3 should "have prediction 1" in {
    val pred = trainedSVMInstance.predict(Seq(rowForPair3))(0)
    pred should be (1)
  }


  pair3 should "have sentenceDistance_min of 2" in {
    val sentenceDistance_min = rowForPair3.featureGroups(rowForPair3.featureGroupNames.indexOf("sentenceDistance_min"))
    sentenceDistance_min should be (2.0)
  }

  pair3 should "have dependencyDistance_min of 2" in {
    val sentenceDistance_min = rowForPair3.featureGroups(rowForPair3.featureGroupNames.indexOf("dependencyDistance_min"))
    sentenceDistance_min should be (2.0)
  }


  pair3 should "have contextFrequency_min of 26" in {
    val sentenceDistance_min = rowForPair3.featureGroups(rowForPair3.featureGroupNames.indexOf("context_frequency_min"))
    sentenceDistance_min should be(26.0)

  }

  pair3 should "have closestContextOfClass_min of 1" in {
    val sentenceDistance_min = rowForPair3.featureGroups(rowForPair3.featureGroupNames.indexOf("closesCtxOfClass_min"))
    sentenceDistance_min should be (1.0)
  }


  val pair4 = "PMC3608085,in195from6to12,uberon:UBERON:0000105"
  val resourcesPathToPair4 = s"${resourcesPath}/PMC3608085/AggregatedRow_PMC3608085_in195from6to12_uberon:UBERON:0000105.txt"
  val urlPathToPair4 = readAndTruncateFileName(resourcesPathToPair4)
  val rowForPair4 = readAggRowFromFile(urlPathToPair4)
  pair4 should "have prediction 0" in {
    val pred = trainedSVMInstance.predict(Seq(rowForPair4))(0)
    pred should be (0)
  }


  pair4 should "have sentenceDistance_min of 1" in {
    val sentenceDistance_min = rowForPair4.featureGroups(rowForPair4.featureGroupNames.indexOf("sentenceDistance_min"))
    sentenceDistance_min should be (1.0)
  }

  pair4 should "have dependencyDistance_min of 0" in {
    val sentenceDistance_min = rowForPair4.featureGroups(rowForPair4.featureGroupNames.indexOf("dependencyDistance_min"))
    sentenceDistance_min should be (0.0)
  }


  pair4 should "have contextFrequency_min of 1" in {
    val sentenceDistance_min = rowForPair4.featureGroups(rowForPair4.featureGroupNames.indexOf("context_frequency_min"))
    sentenceDistance_min should be(1.0)

  }

  pair4 should "have closestContextOfClass_min of 1" in {
    val sentenceDistance_min = rowForPair4.featureGroups(rowForPair4.featureGroupNames.indexOf("closesCtxOfClass_min"))
    sentenceDistance_min should be (1.0)
  }




  def readAndTruncateFileName(resourcePath: String):String = {
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
