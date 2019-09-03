
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

object ManualCheckModel extends App{
  val eventsList = List("inhibition of __controller__ increases the phosphorylation of __controlled__",
  "__controller__ blocked the serum-stimulated __controlled__ __controlled__ __controlled__",
    "decreased __controller__ __controller__ enhances __controlled__ __controlled__ __controlled__",
    "decreased __controller__ expression enhances __controlled__ __controlled__ __controlled__ __controlled__ __controlled__",
    "Cells expressing __controlled__ __controlled__ __controlled__ __controlled__ in response to __controller__ inhibition",
    " __controller__ reduced __controlled__",
    "__controller__ inhibits __controlled__",
    "__controller increases __controlled__ __controlled__ __controlled__ __controlled__",
    "inhibition of __controller__ increases the  __controlled__  __controlled__ __controlled__"
  )

  val polarityRule = List(1, 0, 1, 1, 0, 0, 0, 1, 1)

  val eventsList2 = List("__controller__ increases the __controlled__ __controlled__ __controlled__",
    "inhibition of __controller__ increases the __controlled__ __controlled__ __controlled__",
    "__controller__ blocked the serum-stimulated __controlled__ __controlled__ __controlled__",
    "__controller__ blocked the serum stimulated __controlled__ __controlled__ __controlled__",
    "__controller__ __controller__ enhances EphrinB1 and __controlled__ __controlled__ __controlled__",
    "__controller__ __controller__ enhances __controlled__ __controlled__ __controlled__ __controlled__ __controlled__",
    "__controller__ reduced __controlled__",
    "__controller__ reduced __controlled__ depletion",
    "__controller__ inhibits __controlled__",
    "__controller__ increases the inhibition of __controlled__",
    "suppression of __controller__ increases the inhibition of __controlled__",
    "__controller__ inhibits __controlled__",
    "presence of __controller__ inhibits __controlled__"

  )

  val polarityRule2 = List(1,1,0, 0, 1, 1,0,0,0, 1, 1,0,0)

  val lstmClassifier = new DeepLearningPolarityClassifier()
  for (index <- eventsList.indices){
    lstmClassifier.predictManual(eventsList2(index), polarityRule2(index))
  }
}
