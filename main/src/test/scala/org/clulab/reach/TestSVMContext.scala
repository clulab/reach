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


  // TESTS AND VARIABLES FOR 1ST ACTIVATION PAPER: PMC2910130
  val activationPath1 = config.getString("papersDir").concat(s"/activation/PMC2910130.nxml")
  val nxmlAct1 = nxmlReader.read(activationPath1)
  val docAct1 = reachSystem.mkDoc(nxmlAct1)
  val mentionsActivPaper1 = reachSystem.extractFrom(docAct1)
  // convert resultant from extractEvtId to string when you check against this
  val activevtCtxPair1 = "41520,tissuelist:TS-0500" // expected prediction: 1
  val activevtCtxPair2 = "41520,cl:CL:0000312" // expected prediction: 0
  val activevtCtxPair3 = "51618,tissuelist:TS-0500" // expected prediction: 1
  val outPaperDirPathActiv1 = config.getString("contextEngine.params.contextOutputDir").concat("PMC2910130/")

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
  // CONCLUDING TESTS AND VARIABLES FOR 1ST ACTIVATION PAPER: PMC2910130



  // TESTS AND VARIABLES FOR 2ND ACTIVATION PAPER: PMC4446607
  val activationPath2 = config.getString("papersDir").concat(s"/activation/PMC4446607.nxml")
  val nxmlAct2 = nxmlReader.read(activationPath2)
  val docAct2 = reachSystem.mkDoc(nxmlAct2)
  val mentionsActivPaper2 = reachSystem.extractFrom(docAct2)
  val activPaper2Pair1 = "872526,uberon:UBERON:0000376" // expected output : 0
  val activPaper2Pair2 = "8637,tissuelist:TS-0362" // expected output: 1
  val activPaper2Pair3 = "96424,tissuelist:TS-0574.txt" // expected output: 0
  val outPaperDirPathActiv2 = config.getString("contextEngine.params.contextOutputDir").concat("PMC4446607/")


  activPaper2Pair1 should "not have empty mentions" in {
    mentionsActivPaper2 should not be (empty)
  }

  activPaper2Pair1 should "have prediction 0" in {
    val evtID = activPaper2Pair1.split(",")(0)
    val ctxID = activPaper2Pair1.split(",")(1)
    val filePath = outPaperDirPathActiv2.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }

  activPaper2Pair2 should "have prediction 1" in {
    val evtID = activPaper2Pair2.split(",")(0)
    val ctxID = activPaper2Pair2.split(",")(1)
    val filePath = outPaperDirPathActiv2.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (1)
  }

  activPaper2Pair3 should "have prediction 0" in {
    val evtID = activPaper2Pair3.split(",")(0)
    val ctxID = activPaper2Pair3.split(",")(1)
    val filePath = outPaperDirPathActiv2.concat(s"AggregatedRow_PMC2910130_${evtID}_${ctxID}.txt")
    val activeRow3 = readAggRowFromFile(filePath)
    val pred = trainedSVMInstance.predict(Seq(activeRow3))
    pred(0) should be (0)
  }

  // CONCLUDING TESTS AND VARIABLES FOR 2ND ACTIVATION PAPER: PMC4446607








  /*

  val inhibitionPath1 = config.getString("papersDir").concat(s"/inhibition/PMC2587086.nxml")
  val inhibevtCtxPairSeq1 = Seq(("6024", "taxonomy:9606"), ("314", "taxonomy:10090"), ("606", "tissuelist:TS-1047"))
  val inhibexpectedPredSeq1 = Seq(1,0,1)
  val nxmlInhib1 = nxmlReader.read(inhibitionPath1)
  val docInhib1 = reachSystem.mkDoc(nxmlInhib1)

  val inhibitionPath2 = config.getString("papersDir").concat(s"/inhibition/PMC2636845.nxml")
  val inhibevtCtxPairSeq2 = Seq(("52831","cl:CL:0000056"), ("71114","cl:CL:0000056"), ("5331","cl:CL:0000187"))
  val inhibexpectedPredSeq2 = Seq(1,1,0)
  val nxmlInhib2 = nxmlReader.read(inhibitionPath2)
  val docInhib2 = reachSystem.mkDoc(nxmlInhib2)
*/









  def readAggRowFromFile(fileName: String):AggregatedRow = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[AggregatedRow]
    is.close()
    c
  }


}
