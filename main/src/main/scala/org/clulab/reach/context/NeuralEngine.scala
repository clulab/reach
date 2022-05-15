package org.clulab.reach.context

import collection.immutable
import org.clulab.reach.mentions._
import py4j.GatewayServer
import py4j.Gateway
import py4j.ClientServer
import java.util.List

import org.json4s.JsonAST.JArray
import org.json4s.{DefaultFormats, JString, JValue}
import org.json4s.jackson.JsonMethods._

import scala.collection.JavaConverters._

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
    * Forward a few instances, and each instance consists of a few event context pairs.
    * @param texts:
    * @param evtStarts
    * @param evtEnds
    * @param ctxStarts
    * @param ctxEnds
    * @param evtCtxDists
    * @return
    */
  def forwardInstances(texts: Seq[Seq[String]], evtStarts: Seq[Seq[Int]], evtEnds: Seq[Seq[Int]],
                       ctxStarts: Seq[Seq[Int]], ctxEnds: Seq[Seq[Int]], evtCtxDists: Seq[Seq[Int]]): java.util.List[Int]

}

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
  * The function that actually interacts with the python side is the "forwardInstances" function. All other functions
  * should be implemented based this "forwardInstances" function.
  *
  * The data and model can be found at: https://drive.google.com/drive/folders/17Mqvco9UbPA0mCVmgfpG4w77-7GFehru?usp=sharing
  */
class NeuralContextEngine extends ContextEngine {

  val scalaPythonClientServer = new ClientServer()
  val interface: NeuralContextEnginePythonInterface = scalaPythonClientServer.getPythonServerEntryPoint(Array[Class[_]](classOf[NeuralContextEnginePythonInterface])).asInstanceOf[NeuralContextEnginePythonInterface]

  /***
    * This is used to shutdown the scala service. It is not used currently, but the user can choose to shutdown the service
    * in their code.
    */
  def shutdownInterface(): Unit = {
    scalaPythonClientServer.shutdown()
  }

  def infer(mentions: Seq[BioMention]): Unit = {}

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]): Unit = {}

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = {Seq()}

  /***
    * This function takes a sequence of bio event context instances and get the predictions.
    * It passes the instances to python and get the predictions using the neural model from the python side.
    */
  def forwardInstances(bioEvtCtxInstances: Seq[BioEventContextInstance]): Seq[Int] = {

    val (texts, evtStarts, evtEnds, ctxStarts, ctxEnds, evtCtxDists) = convertDataFormatForPython(bioEvtCtxInstances)

    val preds = interface.forwardInstances(texts, evtStarts, evtEnds, ctxStarts, ctxEnds, evtCtxDists)

    preds.asScala.toSeq
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

  def runValidation(): Float = {
    val valF1 = interface.runValidation()
    valF1
  }

  /***
    * This function loads the validation data in scala and run validation
    * using the forwardInstances method. This is different from the previous runValidation function where the
    * validation data is processed in python. It should yield a 0.507 dev f1.
    *
    * One needs to change this validation data dir accordingly before running this.
    *
    * @return
    */
  def runValidationScala():Float = {

    val valDataPath = "/home/zhengzhongliang/CLU_Projects/2022_ASKE/model_n_data/context_validation_data.json"
    // Validation data can be found at: https://drive.google.com/drive/folders/17Mqvco9UbPA0mCVmgfpG4w77-7GFehru?usp=sharing

    // Read file: https://stackoverflow.com/questions/40172313/scala-read-and-parse-json
    val jsonString = scala.io.Source.fromFile(valDataPath).mkString

    // Parse json in scala:
    // https://stackoverflow.com/questions/4170949/how-to-parse-json-in-scala-using-standard-scala-classes

    // This usage is from another file in the reach project
    // Format: https://stackoverflow.com/questions/32378429/extract-string-value-using-json4s
    implicit val formats = DefaultFormats
    val parsedJsonAllInstances = parse(jsonString).extract[Seq[Map[String, JValue]]]


    // Build the event context instances for prediction
    val allParsedInstances = scala.collection.mutable.ArrayBuffer[BioEventContextInstance]()
    val allLabels = scala.collection.mutable.ArrayBuffer[Int]()

    for (oneInstance <- parsedJsonAllInstances) {
      val label = {
        if (oneInstance("label").extract[Boolean]) 1 else 0}

      val bioEventContextInstanceJson = oneInstance("data").extract[Seq[JArray]]

      val bioEventContextInstance = BioEventContextInstance(
        bioEventContextInstanceJson.map{evtCtxPair =>
          val sent = evtCtxPair(0).extract[String]
          val evtSpanSeq = evtCtxPair(1).extract[Seq[Int]]
          val evtSpan = (evtSpanSeq(0), evtSpanSeq(1))
          val ctxSpanSeq = evtCtxPair(2).extract[Seq[Int]]
          val ctxSpan = (ctxSpanSeq(0), ctxSpanSeq(1))
          val dist = evtCtxPair(3).extract[Int]

          BioEventContextPair(text=sent, eventSpan = evtSpan, contextSpan = ctxSpan, evtCtxDist = dist)
        }
      )

      allParsedInstances.append(bioEventContextInstance)
      allLabels.append(label)
    }

    val allPreds = scala.collection.mutable.ArrayBuffer[Int]()
    val batchSize = 50
    val numBatches = (allParsedInstances.length.toFloat / batchSize.toFloat).ceil.toInt


    for (batchIdx <- Range(0, numBatches)) {
      val batchStart = batchIdx * batchSize
      val batchEnd = ((batchIdx + 1) * batchSize).min(allParsedInstances.length)
      val batchPreds = forwardInstances(allParsedInstances.slice(batchStart, batchEnd))
      val batchLabels = allLabels.slice(batchStart, batchEnd)

      val (bp, br, bf1) = calculate_p_r_f1(batchPreds, batchLabels)

      println("\tprocessing batch ", batchIdx, " f1:", bf1)

      allPreds.appendAll(batchPreds)
    }
    val (p, r, f1) = calculate_p_r_f1(allPreds, allLabels)

    f1.toFloat
  }

  def calculate_p_r_f1(preds: Seq[Int], labels: Seq[Int]): (Double, Double, Double) = {
    var tp = 0f
    var fp = 0f
    var fn = 0f
    val epsilon = 1e-5

    for (idx <- preds.indices) {
      if (preds(idx) == 1) {
        if (labels(idx) == 1) {
          tp += 1
        }
        else {
          fp += 1
        }
      }
      else{
        if (labels(idx) == 1) {
          fn += 1
        }
      }
    }

    val p = tp / (tp + fp + epsilon)
    val r = tp / (tp + fn + epsilon)
    val f1 = 2 * p * r / (p + r + epsilon)

    (p, r, f1)
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
//
  println("start checking processing one example ...")
  val preds = neuralContextEngine.forwardInstances(Seq(bioEvtCtxInstance))
  println("prediction result:", preds)

  // Uncomment this part to run the validation using the full python service
  // println("start running validation ...")
  // val f1 = neuralContextEngine.runValidation()

  // Uncomment this part to run the validation using the forwardInstances function in the scala class
//  val f1 = neuralContextEngine.runValidationScala()
//  println("validation finished! val f1 (should be around 0.507):", f1)

}