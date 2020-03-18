package org.clulab.polarity.ml

import org.clulab.polarity.{Polarity, PolarityEngine}
import org.clulab.reach.mentions.BioEventMention

class MLPolarityEngine(classifier:PolarityClassifier) extends PolarityEngine{

  /**
    * Predicts the polarity of an event with an ML classifier instance
    *
    * @param evt BioEventMention to be operated on
    * @return Outcome of polarity classification
    */
  override def computePolarity(evt: BioEventMention): Polarity = classifier.predict(evt)
}
