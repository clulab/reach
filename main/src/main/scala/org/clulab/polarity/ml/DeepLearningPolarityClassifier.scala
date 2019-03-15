package org.clulab.polarity.ml

import org.clulab.polarity.Polarity
import org.clulab.reach.mentions.BioEventMention

class DeepLearningPolarityClassifier extends PolarityClassifier{

  private var isFitted = false
  /**
    * Trains the classifier. This method is meant to have side effects by fitting the parameters
    *
    * @param events Training data
    * @param labels Training labels
    */
  override def fit(events: Seq[BioEventMention], labels: Seq[Polarity]): Unit = {
    //...
    isFitted = true

    Seq()
  }

  /**
    * Returns whether fit has been called before. Mostly for control
    *
    * @return True if the parameters of the model have been fit previously
    */
  override def isFitted: Boolean = isFitted

  /**
    * Gets the predicted polarity for the provided argument
    *
    * @param events Ordered sequence of events to get their polarity from
    * @return Predictions of Polarity subclasses
    */
  override def predict(events: Seq[BioEventMention]): Seq[Polarity] = ???

  /**
    * Saves the model parameter's to a file
    *
    * @param modelPath file path to save the model to.
    */
  override def save(modelPath: String): Unit = ???

  def runInstace(evt:BioEventMention) = {
    val lemmas = evt.lemmas.get
    val trigger = evt.trigger.lemmas.get
    val rulePolarity = evt.label match {
      case label if label startsWith "Positive" => true
      case label if label startsWith "Negative" => false
      case _  => ???
    }

    var rulePolarity2 = ""
    if
  }
}


object DeepLearningPolarityClassifier {
  def load(path:String):DeepLearningPolarityClassifier = ???
}
