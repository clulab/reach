
package org.clulab.polarity.ml

object TrainDeepLearningModel extends App {
//def load(path:String):DeepLearningPolarityClassifier = ???
   val lstmClassifier = new DeepLearningPolarityClassifier()
   //lstmClassifier.fit()
   lstmClassifier.loadModelEval()
 }
