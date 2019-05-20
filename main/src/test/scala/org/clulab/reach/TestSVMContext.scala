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

  // TESTS AND VARIABLES FOR ACTIVATION PAPER: PMC2910130
  val activationPath1 = config.getString("papersDir").concat(s"/activation/PMC2910130.nxml")
  val nxmlAct1 = nxmlReader.read(activationPath1)
  val docAct1 = reachSystem.mkDoc(nxmlAct1)
  val mentionsActivPaper1 = reachSystem.extractFrom(docAct1)
  // convert resultant from extractEvtId to string when you check against this
  val activevtCtxPair1 = "41520,tissuelist:TS-0500" // expected prediction: 1
  val activevtCtxPair2 = "41520,cl:CL:0000312" // expected prediction: 0
  val activevtCtxPair3 = "51618,tissuelist:TS-0500" // expected prediction: 1
  val outPaperDirPathActiv1 = config.getString("contextEngine.params.contextOutputDir").concat(s"activation/PMC2910130/")

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

  activevtCtxPair2 should "have prediction 0" in {
    val evtID = activevtCtxPair2.split(",")(0)
    val ctxID = activevtCtxPair2.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }

  activevtCtxPair1 should "have prediction 1" in {
    val evtID = activevtCtxPair1.split(",")(0)
    val ctxID = activevtCtxPair1.split(",")(1)
    val filePath = outPaperDirPathActiv1.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }
  // CONCLUDING TESTS AND VARIABLES FOR ACTIVATION PAPER: PMC2910130




  // STARTING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2636845

  val inhibitionPath2 = config.getString("papersDir").concat(s"/inhibition/PMC2636845.nxml")
  val outPaperDirPathInhib1 = config.getString("contextEngine.params.contextOutputDir").concat(s"inhibition/PMC2636845/")
  val nxmlInhib2 = nxmlReader.read(inhibitionPath2)
  val docInhib2 = reachSystem.mkDoc(nxmlInhib2)
  val mentions3 = reachSystem.extractFrom(docInhib2)
  val inhibitPair1 = "52831,cl:CL:0000056" // expected prediction: 1
  val inhibitPair2 = "71114,cl:CL:0000056" // expected prediction: 1
  val inhibitPair3 = "5331,tissuelist:TS-0725" // expected prediction: 0

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

  inhibitPair2 should "have prediction 1" in {
    val evtID = inhibitPair2.split(",")(0)
    val ctxID = inhibitPair2.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }

  inhibitPair3 should "have prediction 0" in {
    val evtID = inhibitPair3.split(",")(0)
    val ctxID = inhibitPair3.split(",")(1)
    val filePath = outPaperDirPathInhib1.concat(s"AggregatedRow_PMC2636845_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }
  // CONCLUDING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2636845


 // STARTING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2587086
  val inhibitionPath1 = config.getString("papersDir").concat(s"/inhibition/PMC2587086.nxml")
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
  // CONCLUDING TESTS AND VARIABLES FOR INHIBITION PAPER: PMC2587086


  def readAggRowFromFile(fileName: String):AggregatedRow = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[AggregatedRow]
    is.close()
    c
  }


}
