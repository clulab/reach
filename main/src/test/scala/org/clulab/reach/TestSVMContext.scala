package org.clulab.reach
import java.io.{FileInputStream, ObjectInputStream}

import TestUtils._
import org.scalatest.{FlatSpec, Matchers}
import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.reach.context.hasSpeciesContext

class TestSVMContext extends FlatSpec with Matchers {
  val config = ConfigFactory.load()


  val text2 = "Rat AKT1 is different from rice AKT1."
  val docID = "testSVMDoc"
  val chunkID = "1"
  val mentions2 = getBioMentions(text2)
  val overrideSpecies = config.getBoolean("grounding.overrideSpecies")
  "Text2 mentions" should "may or may not have all rice groundings" in {
    // This test depends on the setting of grounding.overrideSpecies flag in application.conf:
    mentions2.filter(m => hasSpeciesContext(m)).forall(m => m.isGrounded && m.grounding.get.species == "rice") should be (!overrideSpecies)
    // Using species, this will no longer be true when Reach issue #152 is implemented:
    // mentions2.filter(m => hasSpeciesContext(m)).forall(m => m.isGrounded && m.grounding.get.species == "rice") should be (true)
    // This will only pass when Reach issue #152 is implemented:
    // mentions2.filter(m => hasSpeciesContext(m)).forall(m => m.isGrounded && m.grounding.get.species == "rice") should be (false)
  }



//
//
//  // ************ TESTS AND VARIABLES FOR ACTIVATION PAPER: PMC2910130 ************
//  val activevtCtxPair1 = "41520,tissuelist:TS-0500" // expected prediction: 1 //dmax7 //conmax 2 //closmax 1
//  val activevtCtxPair2 = "41520,cl:CL:0000312" // expected prediction: 0 //dmax7 //conmax 4 //closmax 1
//  val activevtCtxPair3 = "51618,tissuelist:TS-0500" // expected prediction: 1 //dmax8 //conmax 2 //closmax 1
//  val pair4 = "41820,cl:CL:0000312" // expected: 0
//
//
//  //val outPaperDirPathActiv1 = rootDir.concat("/main/src/test/resources/SVMContext/PMC2910130/")
//  val outPaperDirPathActiv1 = "main/src/test/resources/SVMContext/PMC2910130/"
//
//
//  // Pair 1 tests start
//  activevtCtxPair1 should "have prediction 1" in {
//    val evtID = activevtCtxPair1.split(",")(0)
//    val ctxID = activevtCtxPair1.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//
//
//
//  activevtCtxPair1 should "have min sentence distance of 1" in {
//    val evtID = activevtCtxPair1.split(",")(0)
//    val ctxID = activevtCtxPair1.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (1)
//  }
//
//  activevtCtxPair1 should "have maximum sentence distance of 4" in {
//    val evtID = activevtCtxPair1.split(",")(0)
//    val ctxID = activevtCtxPair1.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (4)
//  }
//
//
//  activevtCtxPair1 should "have maximum dependency distance of 7" in {
//    val evtID = activevtCtxPair1.split(",")(0)
//    val ctxID = activevtCtxPair1.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (7)
//  }
//
//  activevtCtxPair1 should "have maximum context frequency of 2" in {
//    val evtID = activevtCtxPair1.split(",")(0)
//    val ctxID = activevtCtxPair1.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("context_frequency_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (2)
//  }
//
//  activevtCtxPair1 should "have maximum closest context of 1" in {
//    val evtID = activevtCtxPair1.split(",")(0)
//    val ctxID = activevtCtxPair1.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (1)
//  }
//  // pair 1 tests end
//
//
//  // pair 2 tests start
//  activevtCtxPair2 should "have prediction 0" in {
//    val evtID = activevtCtxPair2.split(",")(0)
//    val ctxID = activevtCtxPair2.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//
//  activevtCtxPair2 should "have minimum sentence distance of 0" in {
//    val evtID = activevtCtxPair2.split(",")(0)
//    val ctxID = activevtCtxPair2.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (0)
//  }
//
//  activevtCtxPair2 should "have max sentence distance of 3" in {
//    val evtID = activevtCtxPair2.split(",")(0)
//    val ctxID = activevtCtxPair2.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (3)
//  }
//
//  activevtCtxPair2 should "have max dep distance of 7" in {
//    val evtID = activevtCtxPair2.split(",")(0)
//    val ctxID = activevtCtxPair2.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (7)
//  }
//
//  activevtCtxPair2 should "have maximum context frequency of 4" in {
//    val evtID = activevtCtxPair2.split(",")(0)
//    val ctxID = activevtCtxPair2.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("context_frequency_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (4)
//  }
//  activevtCtxPair2 should "have maximum closest context of 1" in {
//    val evtID = activevtCtxPair2.split(",")(0)
//    val ctxID = activevtCtxPair2.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (1)
//  }
//  // pair 2 tests end
//
//
//  // pair 3 tests start
//  activevtCtxPair3 should "have min sentence distance of 2" in {
//    val evtID = activevtCtxPair3.split(",")(0)
//    val ctxID = activevtCtxPair3.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (2)
//  }
//
//  activevtCtxPair3 should "have max sentence distance of 5" in {
//    val evtID = activevtCtxPair3.split(",")(0)
//    val ctxID = activevtCtxPair3.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (5)
//  }
//
//  activevtCtxPair3 should "have max dep distance of 8" in {
//    val evtID = activevtCtxPair3.split(",")(0)
//    val ctxID = activevtCtxPair3.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (8)
//  }
//
//  activevtCtxPair3 should "have maximum context frequency of 2" in {
//    val evtID = activevtCtxPair3.split(",")(0)
//    val ctxID = activevtCtxPair3.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("context_frequency_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (2)
//  }
//
//
//  activevtCtxPair3 should "have prediction 1" in {
//    val evtID = activevtCtxPair3.split(",")(0)
//    val ctxID = activevtCtxPair3.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//  activevtCtxPair3 should "have maximum closest context of 1" in {
//    val evtID = activevtCtxPair3.split(",")(0)
//    val ctxID = activevtCtxPair3.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
//    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
//    valueSentMin should be (1)
//  }
//  // pair 3 tests end
//
//  //val pair4
//  pair4 should "have prediction 0" in {
//    val evtID = pair4.split(",")(0)
//    val ctxID = pair4.split(",")(1)
//    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//
//  // ************ CONCLUDING TESTS AND VARIABLES FOR ACTIVATION PAPER: PMC2910130 ************
//
//
//
//
//
//
//
//  // ************ STARTING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2636845  ************
//
//  //val outPaperDirPathInhib1 = rootDir.concat("/main/src/test/resources/SVMContext/PMC2636845/")
//  val outPaperDirPathInhib1 = "/main/src/test/resources/SVMContext/PMC2636845/"
//
//  val inhibitPair1 = "52831,cl:CL:0000056" // expected prediction: 1
//  val inhibitPair2 = "71114,cl:CL:0000056" // expected prediction: 1
//  val inhibitPair3 = "5331,tissuelist:TS-0725" // expected prediction: 0
//
//
//  inhibitPair1 should "have prediction 1" in {
//    val evtID = inhibitPair1.split(",")(0)
//    val ctxID = inhibitPair1.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//
//
//  inhibitPair1 should "have minimum sentence distance of 1" in {
//    val evtID = inhibitPair1.split(",")(0)
//    val ctxID = inhibitPair1.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (1)
//  }
//
//  inhibitPair1 should "have max sentence distance of 1" in {
//    val evtID = inhibitPair1.split(",")(0)
//    val ctxID = inhibitPair1.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (1)
//  }
//
//  inhibitPair1 should "have max dep distance of 14" in {
//    val evtID = inhibitPair1.split(",")(0)
//    val ctxID = inhibitPair1.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (14)
//  }
//
//  inhibitPair1 should "have max context frequency of 2" in {
//    val evtID = inhibitPair1.split(",")(0)
//    val ctxID = inhibitPair1.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("context_frequency_max")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (2)
//  }
//
//
//  inhibitPair1 should "have max closest context of 1" in {
//    val evtID = inhibitPair1.split(",")(0)
//    val ctxID = inhibitPair1.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (1)
//  }
//  // pair1 tests end
//
//
//  // pair 2 tests begin
//  inhibitPair2 should "have prediction 1" in {
//    val evtID = inhibitPair2.split(",")(0)
//    val ctxID = inhibitPair2.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//
//  inhibitPair2 should "have min sentence distance of 3" in {
//    val evtID = inhibitPair2.split(",")(0)
//    val ctxID = inhibitPair2.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (3)
//  }
//
//  //sentenceDistance_max
//  inhibitPair2 should "have max sentence distance of 3" in {
//    val evtID = inhibitPair2.split(",")(0)
//    val ctxID = inhibitPair2.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (3)
//  }
//
//  //dependencyDistance_max
//  inhibitPair2 should "have max dep distance of 16" in {
//    val evtID = inhibitPair2.split(",")(0)
//    val ctxID = inhibitPair2.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (16)
//  }
//
//  //context_frequency_max
//  inhibitPair2 should "have max context frequency of 2" in {
//    val evtID = inhibitPair2.split(",")(0)
//    val ctxID = inhibitPair2.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("context_frequency_max")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (2)
//  }
//
//  //closesCtxOfClass_max
//  inhibitPair2 should "have max closest context of 0" in {
//    val evtID = inhibitPair2.split(",")(0)
//    val ctxID = inhibitPair2.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val ind = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
//    val value = activeRow3.featureGroups(ind).toInt
//    value should be (0)
//  }
//  //pair 2 tests end
//
//
//  // pair 3 tests begin
//  inhibitPair3 should "have prediction 0" in {
//    val evtID = inhibitPair3.split(",")(0)
//    val ctxID = inhibitPair3.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//  // pair 3 tests end
//
//
//  val inhibitPair4 = "71114,cl:CL:0002372" // expected prediction: 1
//  val inhibitPair5 = "52831,cl:CL:0000187" // expected prediction: 1
//  val inhibitPair6 = "52831,cl:CL:0002372" // expected prediction: 0
//  val inhibitPair7 = "42225,cl:CL:0000056" // expected prediction: 0
//
//  inhibitPair4 should "have prediction 1" in {
//    val evtID = inhibitPair4.split(",")(0)
//    val ctxID = inhibitPair4.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//  inhibitPair5 should "have prediction 1" in {
//    val evtID = inhibitPair5.split(",")(0)
//    val ctxID = inhibitPair5.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//  inhibitPair6 should "have prediction 0" in {
//    val evtID = inhibitPair6.split(",")(0)
//    val ctxID = inhibitPair6.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//  inhibitPair7 should "have prediction 0" in {
//    val evtID = inhibitPair7.split(",")(0)
//    val ctxID = inhibitPair7.split(",")(1)
//    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//
//  // CONCLUDING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2636845
//
//
// // STARTING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2587086
//  //val outPaperDirPathInhib2 = rootDir.concat("/main/src/test/resources/SVMContext/PMC2587086/")
//  val outPaperDirPathInhib2 = "/main/src/test/resources/SVMContext/PMC2587086/"
//
//  val inhibitP1 = "6024,taxonomy:9606" // expected prediction: 1
//  val inhibitP2 = "314,tissuelist:TS-1047" // expected prediction: 0
//  val inhibitP3 = "606,go:GO:0005777" // expected prediction: 1
//
//
//  inhibitP1 should "have prediction 1" in {
//    val evtID = inhibitP1.split(",")(0)
//    val ctxID = inhibitP1.split(",")(1)
//    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//
//  inhibitP2 should "have prediction 0" in {
//    val evtID = inhibitP2.split(",")(0)
//    val ctxID = inhibitP2.split(",")(1)
//    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//
//  inhibitP3 should "have prediction 1" in {
//    val evtID = inhibitP3.split(",")(0)
//    val ctxID = inhibitP3.split(",")(1)
//    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//
//  val inhibitP4 = "549,go:GO:0005777" // expected prediction: 1
//  val inhibitP5 = "3120,tissuelist:TS-0229" // expected prediction: 1
//  val inhibitP6 = "3120,tissuelist:TS-0923" // expected prediction: 0
//  val inhibitP7 = "606,taxonomy:10090" // expected prediction: 0
//  val inhibitP8 = "41518,tissuelist:TS-1047" // expected prediction: 0
//
//  inhibitP4 should "have prediction 1" in {
//    val evtID = inhibitP4.split(",")(0)
//    val ctxID = inhibitP4.split(",")(1)
//    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//  inhibitP5 should "have prediction 1" in {
//    val evtID = inhibitP5.split(",")(0)
//    val ctxID = inhibitP5.split(",")(1)
//    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (1)
//  }
//  inhibitP6 should "have prediction 0" in {
//    val evtID = inhibitP6.split(",")(0)
//    val ctxID = inhibitP6.split(",")(1)
//    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//  inhibitP7 should "have prediction 0" in {
//    val evtID = inhibitP7.split(",")(0)
//    val ctxID = inhibitP7.split(",")(1)
//    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//  inhibitP8 should "have prediction 0" in {
//    val evtID = inhibitP8.split(",")(0)
//    val ctxID = inhibitP8.split(",")(1)
//    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
//    val activeRow3 = readAggRowFromFile(filePath)
//    val pred = trainedSVMInstance.predict(Seq(activeRow3))
//    pred(0) should be (0)
//  }
//
//  // CONCLUDING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2587086
//
//
//
//
//
//
//
//  def readAggRowFromFile(fileName: String):AggregatedContextInstance = {
//    val is = new ObjectInputStream(new FileInputStream(fileName))
//    val c = is.readObject().asInstanceOf[AggregatedContextInstance]
//    is.close()
//    c
//  }


}
