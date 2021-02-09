package org.clulab.reach.context.engine

import org.clulab.reach.context.ContextEngine
import org.clulab.reach.mentions.BioMention

import java.io.File

/**
  * Create SVM based context engine from model file
  * @param modelFile File object with the model's parameters
  */
class SVMContextEngine(modelFile: File) extends ContextEngine {

  /**
    * Create context engine from model path
    * @param modelPath path to the file with the model's parameters
    */
  def this(modelPath: String) = this(new File(modelPath))

  // TODO: Load the contents of modelFile into whatever wrapper/facade structure to the svm classifier

  /** initializes any data structure that needs to be initialized */
  override def infer(mentions: Seq[BioMention]): Unit = ???

  /** updates those data structures with any new info */
  override def update(mentions: Seq[BioMention]): Unit = ???

  /** assigns context to mentions given current state of the engine */
  override def assign(mentions: Seq[BioMention]): Seq[BioMention] = ???
}
