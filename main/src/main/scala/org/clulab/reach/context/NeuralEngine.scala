package org.clulab.reach.context

import collection.immutable
import org.clulab.reach.mentions._

import py4j.GatewayServer
import py4j.Gateway
import py4j.ClientServer

import java.util.List

/***
  * This class is used to store a single event-context pair. Usually an bio instance has multiple event-context pairs.
  * @param text: the text as a single string
  * @param eventSpan: a tuple indicating the start and end position of the event (i.e., starting character position and ending character position)
  * @param contextSpan: a tuple indicating the start and end position of the context
  * @param evtCtxDist: a single Int number indicating the distance between the event and the context.
  */
case class BioEventContextPair(text: String, eventSpan: (Int, Int), contextSpan: (Int, Int), evtCtxDist: Int)

/***
  * This is one bio event for classification. It is composed of multiple event-context pairs. The classification result
  * is given by the voting of the predicted label of each event-context pair.
  * @param bioEvtCtxInstance
  */
case class BioEventContextInstance(bioEvtCtxInstance: Seq[BioEventContextPair])

/***
  * This trait defines the interface that communicates between scala and python. When the following functions are called, the
  * corresponding python functions are called to get the results.
  */
trait NeuralContextEnginePythonInterface {

  /***
    * This function is for benchmarking the validation results. It runs the trained model from the python side on the
    * validation set and get the validation results. It should have an f1 of about 0.507.
    * @return
    */
  def runValidation(): Float

  /***
    * This function
    * @param bioEvtCtxInstances
    * @return
    */
  def forwardInstances(texts: Seq[Seq[String]], evtStarts: Seq[Seq[Int]], evtEnds: Seq[Seq[Int]],
                       ctxStarts: Seq[Seq[Int]], ctxEnds: Seq[Seq[Int]], evtCtxDists: Seq[Seq[Int]]): java.util.List[Int]

}

class NeuralContextEngine extends ContextEngine {
  /***
    * This class implements a neural-based context classifier. The backbone network is a RoBERTa transformer, and the
    * backbone is implemented in Pytorch.
    *
    * To use this context engine, one must start the corresponding python code at first (e.g., using python XXX.py in
    * the directory, with the correct python interpreter and all needed python libraries). Running that python code will start a
    * local server, and this scala side function will act as a client. Whenever there is a request from the client
    * (i.e., the "assign" function is called from the scala side), it will create a request to the running python code
    * (i.e., the server). The input examples will be processed in the python code by the RoBERTa transformer neural
    * model. When the processing is done, the results will be returned by the python service to the scala side.
    *
    */

  val scalaPythonClientServer = new ClientServer()
  val interface: NeuralContextEnginePythonInterface = scalaPythonClientServer.getPythonServerEntryPoint(Array[Class[_]](classOf[NeuralContextEnginePythonInterface])).asInstanceOf[NeuralContextEnginePythonInterface]

  def infer(mentions: Seq[BioMention]): Unit = {}

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]): Unit = {}

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = {Seq()}

  def runValidation(): Float = {

    val valF1 = interface.runValidation()
    valF1
  }

  /***
    * This function takes a sequence of bio event context instances and get the predictions.
    * It passes the instances to python and get the predictions using the neural model from the python side.
    */
  def forwardInstances(bioEvtCtxInstances: Seq[BioEventContextInstance]): java.util.List[Int] = {

    val (texts, evtStarts, evtEnds, ctxStarts, ctxEnds, evtCtxDists) = convertDataFormatForPython(bioEvtCtxInstances)

    val preds = interface.forwardInstances(texts, evtStarts, evtEnds, ctxStarts, ctxEnds, evtCtxDists)

    preds
  }

  /***
    * Becuase it is a little hard to process complex scala data structures (such as dict or tuple) in python, we
    * need to convert all needed arguements to sequences.
    */
  def convertDataFormatForPython(bioEvtCtxInstances: Seq[BioEventContextInstance]): (Seq[Seq[String]], Seq[Seq[Int]],
    Seq[Seq[Int]], Seq[Seq[Int]], Seq[Seq[Int]], Seq[Seq[Int]]) = {

    val texts = bioEvtCtxInstances.map{instance => instance.bioEvtCtxInstance.map{evtCtxPair => evtCtxPair.text}}
    val evtStarts = bioEvtCtxInstances.map{instance => instance.bioEvtCtxInstance.map{evtCtxPair => evtCtxPair.eventSpan._1}}
    val evtEnds = bioEvtCtxInstances.map{instance => instance.bioEvtCtxInstance.map{evtCtxPair => evtCtxPair.eventSpan._2}}
    val ctxStarts = bioEvtCtxInstances.map{instance => instance.bioEvtCtxInstance.map{evtCtxPair => evtCtxPair.contextSpan._1}}
    val ctxEnds = bioEvtCtxInstances.map{instance => instance.bioEvtCtxInstance.map{evtCtxPair => evtCtxPair.contextSpan._2}}
    val evtCtxDists = bioEvtCtxInstances.map{instance => instance.bioEvtCtxInstance.map{evtCtxPair => evtCtxPair.evtCtxDist}}

    (texts, evtStarts, evtEnds, ctxStarts, ctxEnds, evtCtxDists)

  }

}

/***
  * This function is used to test run the context classifier and for debugging.
  * The runValidation function should yield a validation f1 of about 0.507.
  * The forwardInstances should return a sequence of predictions. Here we only have one instance, so only one prediction.
  *
  * sbt "runMain org.clulab.reach.context.BenchmarkNeuralContextEngine"
  */
object BenchmarkNeuralContextEngine extends App {

  // Build one bio event to test the code
  val bioEvtCtxInstance = BioEventContextInstance(
    Seq(
      BioEventContextPair(text="Phospholipase C delta-4 overexpression upregulates ErbB1/2 expression , Erk signaling pathway , and proliferation in MCF-7 cells .",
        eventSpan=(51, 69), contextSpan=(117, 122), evtCtxDist=0),
      BioEventContextPair(text="Phospholipase C delta-4 overexpression upregulates ErbB1/2 expression , Erk signaling pathway , and proliferation in MCF-7 cells . Background . The expression of t$e rodent phosphoinositide specific phospholipase C delta-4 ( PLCdelta4 ) has been found to be elevated upon mitogenic stimulation and expression analysis have linked the $pregulation of <EVENT> <EVENT> with rapid proliferation in certain <CONTEXT> transformed cell lines . The <CONTEXT> homologue of PLCdelta4 has not been extensively charac$erized . Accordingly , we investigate the effects of <EVENT> <EVENT> <CONTEXT> <EVENT> on cell signaling and proliferation in this study . Results . The cDNA for <CONTEXT$ PLCdelta4 has been isolated and expressed ectopically in <CONTEXT> <CONTEXT> MCF-7 cells .",
        eventSpan=(51, 69), contextSpan=(755, 760), evtCtxDist=6),
      BioEventContextPair(text="Phospholipase C delta-4 overexpression upregulates ErbB1/2 expression , Erk signaling pathway , and proliferation in MCF-7 cells . Background . The expression of t$e rodent phosphoinositide specific phospholipase C delta-4 ( PLCdelta4 ) has been found to be elevated upon mitogenic stimulation and expression analysis have linked the $pregulation of <EVENT> <EVENT> with rapid proliferation in certain <CONTEXT> transformed cell lines . The <CONTEXT> homologue of PLCdelta4 has not been extensively charac$erized . Accordingly , we investigate the effects of <EVENT> <EVENT> <CONTEXT> <EVENT> on cell signaling and proliferation in this study . Results . The cDNA for <CONTEXT$ PLCdelta4 has been isolated and expressed ectopically in <CONTEXT> <CONTEXT> MCF-7 cells . <EVENT> <EVENT> <EVENT> <EVENT> <EVENT> protein kinase <EVENT> <EVENT> <EVENT>\n<EVENT> <EVENT> <EVENT> <EVENT> <EVENT> <EVENT> <EVENT> EGFR and erbB1 and HER2 and erbB2 , leading to constitutive activation of extracellular signal regulated kinases 1 and 2 ( ERK1/2 ) pathway in MCF-7 cells .",
        eventSpan=(51, 69), contextSpan=(1047, 1052), evtCtxDist=7)
    )
  )

  val neuralContextEngine = new NeuralContextEngine()


  val preds = neuralContextEngine.forwardInstances(Seq(bioEvtCtxInstance))

  // val f1 = neuralContextEngine.runValidation()


}