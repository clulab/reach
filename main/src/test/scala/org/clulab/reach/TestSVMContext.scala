package org.clulab.reach
import org.scalatest.{FlatSpec, Matchers}
import org.clulab.reach.context.{SVMContextEngine, ContextEngine}

import scala.util.Try
import ai.lum.nxmlreader.NxmlReader
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import com.typesafe.config.ConfigFactory
import org.clulab.odin.EventMention
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}
import org.ml4ai.data.classifiers.LinearSVMWrapper

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
  val activationPath1 = config.getString("papersDir").concat(s"/activation/PMC2910130.nxml")
  val nxmlAct1 = nxmlReader.read(activationPath1)
  val docAct1 = reachSystem.mkDoc(nxmlAct1)
  val mentions = reachSystem.extractFrom(docAct1)
  // convert resultant from extractEvtId to string when you check against this
  val activevtCtxPair1 = "41520,tissuelist:TS-0500" // expected prediction: 1
  val activevtCtxPair2 = "41520,cl:CL:0000312" // expected prediction: 0
  val activevtCtxPair3 = "51618,tissuelist:TS-0500" // expected prediction: 1

/*
  val activationPath2 = config.getString("papersDir").concat(s"/activation/PMC4446607.nxml")
  val activevtCtxPairSeq2 = Seq(("872526",	"uberon:UBERON:0000376"), ("8637", "tissuelist:TS-0362"), ("872526","uberon:UBERON:0000376"))
  val activexpectedPredSeq2 = Seq(0,1,0)
  val nxmlAct2 = nxmlReader.read(activationPath2)
  val docAct2 = reachSystem.mkDoc(nxmlAct2)


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



  activevtCtxPair1 should "not have empty mentions" in {
    mentions should not be (empty)
  }

  activevtCtxPair1 should "have prediction 1" in {
    val evtID = activevtCtxPair1.split(",")(0)
    val ctxID = activevtCtxPair1.split(",")(1)
    val evtMentionsOnly = mentions.collect { case evt: BioEventMention => (evt.sentence, evt.tokenInterval) }

  }


}
