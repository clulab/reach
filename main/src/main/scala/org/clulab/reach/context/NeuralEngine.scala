package org.clulab.reach.context

import collection.immutable
import org.clulab.reach.mentions._
import py4j.ClientServer


trait NeuralContextEnginePythonInterface {

  def runValidation(): Float

  def forwardOneInstance(scalaInstance: Seq[(String, (Int, Int), (Int, Int), Int)]): Int
}

abstract class NeuralContextEngine extends ContextEngine {
  /***
    * This class implements a neural-based context classifier. The backbone network is a RoBERTa transformer, and the
    * backbone is implemented in Pytorch.
    *
    * To use this context engine, one must start the corresponding python code. Running that python code will start a
    * local server, and this scala side function will act as a client. Whenever there is a request from the client
    * (i.e., the "assign" function is called from the scala side), it will create a request to the running python code
    * (i.e., the server). The input examples will be processed in the python code by the RoBERTa transformer neural
    * model. When the processing is done, the results will be returned by the python service to the scala side.
    *
    * When the methods in this class are called,
    */

  def infer(mentions: Seq[BioMention]): Unit

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]): Unit

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]): Seq[BioMention]

  def predict_one_example(instances:Seq[Int]): Boolean = {
    /***
      * This function takes one input instance example as the input and make the prediction.
      *
      * TODO: specify the format of the input and the output
      */

    false
  }


}

object BenchmarkNeuralContextEngine extends App {

  // val f1 = runValidation()
  // println("final f1 (scala side):", f1)

  val scalaInstance = Seq(
    ("\"Phospholipase C delta-4 overexpression upregulates ErbB1/2 expression , Erk signaling pathway , and proliferation in MCF-7 cells .",
      (51, 69), (117, 122), 0),
    ("Phospholipase C delta-4 overexpression upregulates ErbB1/2 expression , Erk signaling pathway , and proliferation in MCF-7 cells . Background . The expression of t$e rodent phosphoinositide specific phospholipase C delta-4 ( PLCdelta4 ) has been found to be elevated upon mitogenic stimulation and expression analysis have linked the $pregulation of <EVENT> <EVENT> with rapid proliferation in certain <CONTEXT> transformed cell lines . The <CONTEXT> homologue of PLCdelta4 has not been extensively charac$erized . Accordingly , we investigate the effects of <EVENT> <EVENT> <CONTEXT> <EVENT> on cell signaling and proliferation in this study . Results . The cDNA for <CONTEXT$ PLCdelta4 has been isolated and expressed ectopically in <CONTEXT> <CONTEXT> MCF-7 cells .",
      (51, 69), (755, 760), 6),
    ("Phospholipase C delta-4 overexpression upregulates ErbB1/2 expression , Erk signaling pathway , and proliferation in MCF-7 cells . Background . The expression of t$e rodent phosphoinositide specific phospholipase C delta-4 ( PLCdelta4 ) has been found to be elevated upon mitogenic stimulation and expression analysis have linked the $pregulation of <EVENT> <EVENT> with rapid proliferation in certain <CONTEXT> transformed cell lines . The <CONTEXT> homologue of PLCdelta4 has not been extensively charac$erized . Accordingly , we investigate the effects of <EVENT> <EVENT> <CONTEXT> <EVENT> on cell signaling and proliferation in this study . Results . The cDNA for <CONTEXT$ PLCdelta4 has been isolated and expressed ectopically in <CONTEXT> <CONTEXT> MCF-7 cells . <EVENT> <EVENT> <EVENT> <EVENT> <EVENT> protein kinase <EVENT> <EVENT> <EVENT>\n<EVENT> <EVENT> <EVENT> <EVENT> <EVENT> <EVENT> <EVENT> EGFR and erbB1 and HER2 and erbB2 , leading to constitutive activation of extracellular signal regulated kinases 1 and 2 ( ERK1/2 ) pathway in MCF-7 cells .",
      (51, 69), (1047, 1052), 7)
  )
  val pred = forwardOneInstance(scalaInstance)
  println("final pred (scala):", pred)

  def runValidation(): Float = {
    val scalaPythonClientServer = new ClientServer()
    val interface: NeuralContextEnginePythonInterface = scalaPythonClientServer.getPythonServerEntryPoint(Array[Class[_]]( classOf[NeuralContextEnginePythonInterface])).asInstanceOf[NeuralContextEnginePythonInterface]
    // some usage of getPythonServerEntryPoint:
    // https://programtalk.com/vs/py4j/py4j-java/src/test/java/py4j/instrumented/InstrumentedApplication.java/#

    // The final method that works comes from here:
    // https://github.com/timsetsfire/urban-barnacle/blob/383ab8412391cabe4dc8ec565890a8b13db4be3c/src/main/scala/WandB.scala

    val valF1 = interface.runValidation()

    valF1
  }

  def forwardOneInstance(scalaInstance: Seq[(String, (Int, Int), (Int, Int), Int)]): Int = {

    val scalaPythonClientServer = new ClientServer()
    val interface: NeuralContextEnginePythonInterface = scalaPythonClientServer.getPythonServerEntryPoint(Array[Class[_]]( classOf[NeuralContextEnginePythonInterface])).asInstanceOf[NeuralContextEnginePythonInterface]
    val pred = interface.forwardOneInstance(scalaInstance)

    pred
  }

  // sbt "runMain org.clulab.reach.context.BenchmarkNeuralContextEngine"

}