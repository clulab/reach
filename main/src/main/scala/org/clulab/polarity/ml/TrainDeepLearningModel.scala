
package org.clulab.polarity.ml

object TrainDeepLearningModel extends App {
//use this object to train the deep learning model. This will the parameter of the model in the root directory of reach.
  val lstmClassifier = new DeepLearningPolarityClassifier()
  lstmClassifier.fit()
 }


object EvalDeepLearningModel extends App {
  val lstmClassifier = new DeepLearningPolarityClassifier()
  lstmClassifier.loadModelEval()
}
