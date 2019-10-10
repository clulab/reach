
package org.clulab.polarity.ml
import scala.collection.mutable.ListBuffer



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
    "inhibition of __controller__ increases the phosphorylation of __controlled__",
    "__controller__ blocked the serum-stimulated __controlled__ __controlled__ __controlled__",
    "__controller__ blocked the serum stimulated __controlled__ __controlled__ __controlled__",
    "__controller__ __controller__ enhances EphrinB1 and __controlled__ __controlled__ __controlled__",
    "__controller__ __controller__ enhances __controlled__ __controlled__ __controlled__ __controlled__ __controlled__",
    "decreased __controller__ __controller__ enhances EphrinB1 and __controlled__ __controlled__ __controlled__",
    "decreased __controller__ __controller__ enhances __controlled__ __controlled__ __controlled__ __controlled__ __controlled__",
    "__controller__ expression enhances EphrinB1 and __controlled__ and phosphorylation",
    "__controller__ expression enhances __controlled__ and Erk1 and phosphorylation",
    "decreased __controller__ expression enhances EphrinB1 and __controlled__ and phosphorylation",
    "decreased __controller__ expression enhances __controlled__ and Erk1 and phosphorylation",
    "__controller__ reduced __controlled__",
    "__controller__ reduced __controlled__ depletion",
    "__controller__ inhibits __controlled__",
    "__controller__ increases the inhibition of __controlled__",
    "suppression of __controller__ increases the inhibition of __controlled__",
    "__controller__ inhibits __controlled__",
    "presence of __controller__ inhibits __controlled__"

  )

  val polarityRule2 = List(1,1,1,0, 0, 1, 1,1,1,1,1,1,1, 0,0,0, 1, 1,0,0)

  val evalList_regulation_20191009 = List(
    "inhibition of __controller__ increases the phosphorylation of __controlled__",
    "__controller__ blocked the serum stimulated phosphorylation of __controlled__",
    "phosphorylation of AKT1 following inhibition of MEK",
    //"",
  )
  val polarityRule_regulation_20191009 = List(1,0,1
  )
  val polarity_regulation_20191009 = List(0,0,0

  )

  val lstmClassifier = new DeepLearningPolarityClassifier()
  for (index <- eventsList2.indices){
    lstmClassifier.predictManual(eventsList2(index), polarityRule2(index))
  }

}

object TestFunc extends App {
  //var A = (("A",Seq(1,2,3)), ("B",2), ("C",(4,5)))
  val A = new ListBuffer[Int]()

  A.append(1)
  A.append(2)

  println(A)

  val B = A.toList
  println(B)

  var a = 1

//  println(testfunc(a))
//  def testfunc(x:Int):Int = {
//    x= x+1
//    x
//  }
}