package org.clulab.reach

import org.clulab.polarity.ml.DeepLearningPolarityClassifier
import org.scalatest.{FlatSpec, Matchers}

/**
 * Unit tests to ensure Activation event rules are matching correctly
 * User: mihais
 * Date: 5/19/15
 */
class TestDeepLearningPolarityEngine extends FlatSpec with Matchers {
  val lstmClassifier = new DeepLearningPolarityClassifier()

  "Trained LSTM polarity classifier" should " have >0.87 f1 score on dev set. " in {
    lstmClassifier.loadModelEval()>0.9 should be (true)
  }
}
