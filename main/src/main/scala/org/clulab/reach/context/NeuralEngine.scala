package org.clulab.reach.context

import collection.immutable
import org.clulab.reach.mentions._

trait NeuralContextEnginePythonInterface {

  def run_validation(): Unit
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



}