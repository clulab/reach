package org.clulab.polarity.ml

import org.clulab.polarity.Polarity
import org.clulab.reach.mentions.BioEventMention
import com.typesafe.scalalogging.LazyLogging


/**
  * Base trait for an ML-based polarity classifier
  */
trait PolarityClassifier extends LazyLogging{

  /**
    * Trains the classifier. This method is meant to have side effects by fitting the parameters
    * @param events Training data
    * @param labels Training labels
    */
  def fit(trainingPath:String, trainRatio:Float, saveFlag:Boolean)


  /**
    * Returns whether fit has been called before. Mostly for control
    * @return True if the parameters of the model have been fit previously
    */
  def isFitted:Boolean

  /**
    * Gets the predicted polarity for the provided argument
    * @param events Ordered sequence of events to get their polarity from
    * @return Predictions of Polarity subclasses
    */
  def predict(events:Seq[BioEventMention]):Seq[Polarity]

  /**
    * Convenience overload to predict a single instance
    * @param event to get its polarity predicted
    * @return polarity of the argument
    */
  def predict(event:BioEventMention):Polarity

  /**
    * Saves the model parameter's to a file
    * @param modelPath file path to save the model to.
    */
  def save(modelPath:String)
}

