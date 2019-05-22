package org.clulab.reach
import java.io.{FileInputStream, ObjectInputStream}

import org.scalatest.{FlatSpec, Matchers}
import org.clulab.reach.context.{ContextEngine, SVMContextEngine}

import scala.util.Try
import ai.lum.nxmlreader.NxmlReader
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import com.typesafe.config.ConfigFactory
import org.clulab.odin.EventMention
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}
import org.ml4ai.data.classifiers.LinearSVMWrapper
import org.ml4ai.data.utils.AggregatedRow

class TestSVMContext extends FlatSpec with Matchers {
  val config = ConfigFactory.load()

  val configPath = config.getString("contextEngine.params.svmPath")
  val svmWrapper = new LinearSVMWrapper(null)
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)

  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams)

  // In this test suite, I'm testing more on inhibition papers rather than activation papers, since they are shorter in length, and thus take lesser time to run through reach.



  // ************ TESTS AND VARIABLES FOR ACTIVATION PAPER: PMC2910130 ************
  val testingActivationPapers = "activation_papers_for_testing"
  val activationPath1 = config.getString("papersDir").concat(s"/${testingActivationPapers}/PMC2910130.nxml")
  val nxmlAct1 = nxmlReader.read(activationPath1)
  val docAct1 = reachSystem.mkDoc(nxmlAct1)
  val mentionsActivPaper1 = reachSystem.extractFrom(docAct1)
  // convert resultant from extractEvtId to string when you check against this
  val activevtCtxPair1 = "41520,tissuelist:TS-0500" // expected prediction: 1 //dmax7 //conmax 2 //closmax 1
  val activevtCtxPair2 = "41520,cl:CL:0000312" // expected prediction: 0 //dmax7 //conmax 4 //closmax 1
  val activevtCtxPair3 = "51618,tissuelist:TS-0500" // expected prediction: 1 //dmax8 //conmax 2 //closmax 1
  val pair4 = "41820,cl:CL:0000312" // expected: 0

  val outPaperDirPathActiv1 = config.getString("contextEngine.params.contextOutputDir").concat(s"activation/PMC2910130/")


  // Pair 1 tests start
  activevtCtxPair1 should "have prediction 1" in {
    val evtID = activevtCtxPair1.split(",")(0)
    val ctxID = activevtCtxPair1.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }



  activevtCtxPair1 should "have min sentence distance of 1" in {
    val evtID = activevtCtxPair1.split(",")(0)
    val ctxID = activevtCtxPair1.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (1)
  }

  activevtCtxPair1 should "have maximum sentence distance of 4" in {
    val evtID = activevtCtxPair1.split(",")(0)
    val ctxID = activevtCtxPair1.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (4)
  }


  activevtCtxPair1 should "have maximum dependency distance of 7" in {
    val evtID = activevtCtxPair1.split(",")(0)
    val ctxID = activevtCtxPair1.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (7)
  }

  activevtCtxPair1 should "have maximum context frequency of 2" in {
    val evtID = activevtCtxPair1.split(",")(0)
    val ctxID = activevtCtxPair1.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("context_frequency_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (2)
  }

  activevtCtxPair1 should "have maximum closest context of 1" in {
    val evtID = activevtCtxPair1.split(",")(0)
    val ctxID = activevtCtxPair1.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (1)
  }
  // pair 1 tests end


  // pair 2 tests start
  activevtCtxPair2 should "have prediction 0" in {
    val evtID = activevtCtxPair2.split(",")(0)
    val ctxID = activevtCtxPair2.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }

  activevtCtxPair2 should "have minimum sentence distance of 0" in {
    val evtID = activevtCtxPair2.split(",")(0)
    val ctxID = activevtCtxPair2.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (0)
  }

  activevtCtxPair2 should "have max sentence distance of 3" in {
    val evtID = activevtCtxPair2.split(",")(0)
    val ctxID = activevtCtxPair2.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (3)
  }

  activevtCtxPair2 should "have max dep distance of 7" in {
    val evtID = activevtCtxPair2.split(",")(0)
    val ctxID = activevtCtxPair2.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (7)
  }

  activevtCtxPair2 should "have maximum context frequency of 4" in {
    val evtID = activevtCtxPair2.split(",")(0)
    val ctxID = activevtCtxPair2.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("context_frequency_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (4)
  }
  activevtCtxPair2 should "have maximum closest context of 1" in {
    val evtID = activevtCtxPair2.split(",")(0)
    val ctxID = activevtCtxPair2.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (1)
  }
  // pair 2 tests end


  // pair 3 tests start
  activevtCtxPair3 should "have min sentence distance of 2" in {
    val evtID = activevtCtxPair3.split(",")(0)
    val ctxID = activevtCtxPair3.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (2)
  }

  activevtCtxPair3 should "have max sentence distance of 5" in {
    val evtID = activevtCtxPair3.split(",")(0)
    val ctxID = activevtCtxPair3.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (5)
  }

  activevtCtxPair3 should "have max dep distance of 8" in {
    val evtID = activevtCtxPair3.split(",")(0)
    val ctxID = activevtCtxPair3.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (8)
  }

  activevtCtxPair3 should "have maximum context frequency of 2" in {
    val evtID = activevtCtxPair3.split(",")(0)
    val ctxID = activevtCtxPair3.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("context_frequency_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (2)
  }

  activevtCtxPair3 should "not have empty mentions" in {
    mentionsActivPaper1 should not be (empty)
  }

  activevtCtxPair3 should "have prediction 1" in {
    val evtID = activevtCtxPair3.split(",")(0)
    val ctxID = activevtCtxPair3.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }
  activevtCtxPair3 should "have maximum closest context of 1" in {
    val evtID = activevtCtxPair3.split(",")(0)
    val ctxID = activevtCtxPair3.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val indexOfSentMin = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
    val valueSentMin = activeRow3.featureGroups(indexOfSentMin).toInt
    valueSentMin should be (1)
  }
  // pair 3 tests end

  //val pair4
  pair4 should "have prediction 0" in {
    val evtID = pair4.split(",")(0)
    val ctxID = pair4.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }

  // ************ CONCLUDING TESTS AND VARIABLES FOR ACTIVATION PAPER: PMC2910130 ************







  // ************ STARTING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2636845  ************
  val testingInhibitionPapers = "inhibition_papers_for_testing"
  val inhibitionPath2 = config.getString("papersDir").concat(s"/${testingInhibitionPapers}/PMC2636845.nxml")
  val outPaperDirPathInhib1 = config.getString("contextEngine.params.contextOutputDir").concat(s"inhibition/PMC2636845/")
  val nxmlInhib2 = nxmlReader.read(inhibitionPath2)
  val docInhib2 = reachSystem.mkDoc(nxmlInhib2)
  val mentions3 = reachSystem.extractFrom(docInhib2)
  val inhibitPair1 = "52831,cl:CL:0000056" // expected prediction: 1
  val inhibitPair2 = "71114,cl:CL:0000056" // expected prediction: 1
  val inhibitPair3 = "5331,tissuelist:TS-0725" // expected prediction: 0


  // pair 1 tests begin
  inhibitPair1 should "not have empty mentions" in {
    mentions3 should not be (empty)
  }

  inhibitPair1 should "have prediction 1" in {
    val evtID = inhibitPair1.split(",")(0)
    val ctxID = inhibitPair1.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }


  inhibitPair1 should "have minimum sentence distance of 1" in {
    val evtID = inhibitPair1.split(",")(0)
    val ctxID = inhibitPair1.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (1)
  }

  inhibitPair1 should "have max sentence distance of 1" in {
    val evtID = inhibitPair1.split(",")(0)
    val ctxID = inhibitPair1.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (1)
  }

  inhibitPair1 should "have max dep distance of 14" in {
    val evtID = inhibitPair1.split(",")(0)
    val ctxID = inhibitPair1.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (14)
  }

  inhibitPair1 should "have max context frequency of 2" in {
    val evtID = inhibitPair1.split(",")(0)
    val ctxID = inhibitPair1.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("context_frequency_max")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (2)
  }


  inhibitPair1 should "have max closest context of 1" in {
    val evtID = inhibitPair1.split(",")(0)
    val ctxID = inhibitPair1.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (1)
  }
  // pair1 tests end


  // pair 2 tests begin
  inhibitPair2 should "have prediction 1" in {
    val evtID = inhibitPair2.split(",")(0)
    val ctxID = inhibitPair2.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }

  inhibitPair2 should "have min sentence distance of 3" in {
    val evtID = inhibitPair2.split(",")(0)
    val ctxID = inhibitPair2.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("sentenceDistance_min")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (3)
  }

  //sentenceDistance_max
  inhibitPair2 should "have max sentence distance of 3" in {
    val evtID = inhibitPair2.split(",")(0)
    val ctxID = inhibitPair2.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("sentenceDistance_max")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (3)
  }

  //dependencyDistance_max
  inhibitPair2 should "have max dep distance of 16" in {
    val evtID = inhibitPair2.split(",")(0)
    val ctxID = inhibitPair2.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("dependencyDistance_max")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (16)
  }

  //context_frequency_max
  inhibitPair2 should "have max context frequency of 2" in {
    val evtID = inhibitPair2.split(",")(0)
    val ctxID = inhibitPair2.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("context_frequency_max")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (2)
  }

  //closesCtxOfClass_max
  inhibitPair2 should "have max closest context of 0" in {
    val evtID = inhibitPair2.split(",")(0)
    val ctxID = inhibitPair2.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val ind = activeRow3.featureGroupNames.indexOf("closesCtxOfClass_max")
    val value = activeRow3.featureGroups(ind).toInt
    value should be (0)
  }
  //pair 2 tests end


  // pair 3 tests begin
  inhibitPair3 should "have prediction 0" in {
    val evtID = inhibitPair3.split(",")(0)
    val ctxID = inhibitPair3.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }
  // pair 3 tests end


  val inhibitPair4 = "71114,cl:CL:0000187" // expected prediction: 1
  val inhibitPair5 = "52831,cl:CL:0000187" // expected prediction: 1
  val inhibitPair6 = "5331,tissuelist:TS-0725" // expected prediction: 0
  val inhibitPair7 = "42225,cl:CL:0000056" // expected prediction: 0
  val inhibitPair8 = "52831,cl:CL:0002372" // expected prediction: 0
  inhibitPair4 should "have prediction 1" in {
    val evtID = inhibitPair3.split(",")(0)
    val ctxID = inhibitPair3.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }
  inhibitPair5 should "have prediction 1" in {
    val evtID = inhibitPair5.split(",")(0)
    val ctxID = inhibitPair5.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }
  inhibitPair6 should "have prediction 0" in {
    val evtID = inhibitPair6.split(",")(0)
    val ctxID = inhibitPair6.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }
  inhibitPair7 should "have prediction 0" in {
    val evtID = inhibitPair7.split(",")(0)
    val ctxID = inhibitPair7.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }
  inhibitPair8 should "have prediction 0" in {
    val evtID = inhibitPair8.split(",")(0)
    val ctxID = inhibitPair8.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }
  // CONCLUDING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2636845


 // STARTING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2587086

  val inhibitionPath1 = config.getString("papersDir").concat(s"/${testingInhibitionPapers}/PMC2587086.nxml")
  val outPaperDirPathInhib2 = config.getString("contextEngine.params.contextOutputDir").concat(s"inhibition/PMC2587086/")
  val nxmlInhib1 = nxmlReader.read(inhibitionPath1)
  val docInhib1 = reachSystem.mkDoc(nxmlInhib1)
  val mentions4 = reachSystem.extractFrom(docInhib1)
  val inhibitP1 = "6024,taxonomy:9606" // expected prediction: 1
  val inhibitP2 = "314,tissuelist:TS-1047" // expected prediction: 0
  val inhibitP3 = "606,go:GO:0005777" // expected prediction: 1

  inhibitP1 should "not have empty mentions" in {
    mentions4 should not be (empty)
  }

  inhibitP1 should "have prediction 1" in {
    val evtID = inhibitP1.split(",")(0)
    val ctxID = inhibitP1.split(",")(1)
    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }

  inhibitP2 should "have prediction 0" in {
    val evtID = inhibitP2.split(",")(0)
    val ctxID = inhibitP2.split(",")(1)
    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }

  inhibitP3 should "have prediction 1" in {
    val evtID = inhibitP3.split(",")(0)
    val ctxID = inhibitP3.split(",")(1)
    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }

  val inhibitP4 = "549,go:GO:0005777" // expected prediction: 1
  val inhibitP5 = "3120,tissuelist:TS-0229" // expected prediction: 1
  val inhibitP6 = "3120,tissuelist:TS-0923" // expected prediction: 0
  val inhibitP7 = "606,taxonomy:10090" // expected prediction: 0
  val inhibitP8 = "41518,tissuelist:TS-1047" // expected prediction: 0

  inhibitP4 should "have prediction 1" in {
    val evtID = inhibitP4.split(",")(0)
    val ctxID = inhibitP4.split(",")(1)
    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }
  inhibitP5 should "have prediction 1" in {
    val evtID = inhibitP5.split(",")(0)
    val ctxID = inhibitP5.split(",")(1)
    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }
  inhibitP6 should "have prediction 0" in {
    val evtID = inhibitP6.split(",")(0)
    val ctxID = inhibitP6.split(",")(1)
    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }
  inhibitP7 should "have prediction 0" in {
    val evtID = inhibitP7.split(",")(0)
    val ctxID = inhibitP7.split(",")(1)
    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }
  inhibitP8 should "have prediction 0" in {
    val evtID = inhibitP8.split(",")(0)
    val ctxID = inhibitP8.split(",")(1)
    val filePath = outPaperDirPathInhib2.concat(s"AggregatedRow_PMC2587086_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }

  // CONCLUDING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2587086







  def readAggRowFromFile(fileName: String):AggregatedRow = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[AggregatedRow]
    is.close()
    c
  }


}
