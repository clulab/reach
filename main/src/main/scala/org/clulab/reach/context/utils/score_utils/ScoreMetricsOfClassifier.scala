package org.clulab.reach.context.utils.score_utils


object ScoreMetricsOfClassifier {
  def argMax(values:Map[Int, Double]):Int = {
    var bestK = Integer.MIN_VALUE
    var bestF1 = Double.MinValue
    values.map(x => {if (x._2 > bestF1) {bestK = x._1; bestF1 = x._2}})
    bestK
  }

  def f1(yTrue: Seq[Int], yPred: Seq[Int]): Double = {
    val p = precision(yTrue, yPred)
    val r = recall(yTrue, yPred)
    if (p + r == 0) 0.0
    else ((2 * (p * r))/(p + r))
  }

  def precision(yTrue: Seq[Int], yPred: Seq[Int]): Double = {
    val predictsMap = predictCounts(yTrue, yPred)
    if(!(predictsMap("TP").toDouble + predictsMap("FP").toDouble == 0.toDouble)) predictsMap("TP").toDouble / (predictsMap("TP") + predictsMap("FP")).toDouble
    else 0.0
  }

  def recall(yTrue: Seq[Int], yPred: Seq[Int]): Double = {
    val predictsMap = predictCounts(yTrue, yPred)
    if (!(predictsMap("TP").toDouble + predictsMap("FN").toDouble == 0)) predictsMap("TP").toDouble/(predictsMap("TP") + predictsMap("FN")).toDouble
    else 0.0
  }

  def accuracy(yTrue: Seq[Int], yPred: Seq[Int]): Double = {
    val predictsMap = predictCounts(yTrue, yPred)
    if (!((predictsMap("TP") + predictsMap("FP") + predictsMap("FN") + predictsMap("TN").toDouble) == 0)) (predictsMap("TP") + predictsMap("TN")).toDouble/(predictsMap("TP") + predictsMap("TN") + predictsMap("FP") + predictsMap("FN")).toDouble
    else 0.0
  }

  def arithmeticMeanScore(scores:Seq[Double]):Double = {
    val sum = scores.foldLeft(0.0)(_ + _)
    sum/scores.size
  }

  def predictCounts(yTrue: Seq[Int], yPred: Seq[Int]): Map[String, Int] = {
    val indexValuePair = yTrue zip yPred
    var TP = 0; var FP = 0; var TN = 0; var FN = 0
    for((gt,pr) <- indexValuePair) {
      if (gt == 1 && pr == 1) TP+=1
      if (gt == 1 && pr == 0) FN +=1
      if (gt == 0 && pr == 0) TN +=1
      if (gt == 0 && pr == 1) FP +=1
    }
    Map(("TP" -> TP), ("FP" -> FP), ("TN" -> TN), ("FN" -> FN))
  }

  def scoreMaker(name: String, truthTest:Array[Int], predTest:Array[Int]):Map[String, (String, Double, Double, Double)] = {
    val precTest = ScoreMetricsOfClassifier.precision(truthTest, predTest)
    val recallTest = ScoreMetricsOfClassifier.recall(truthTest, predTest)
    val f1Test = ScoreMetricsOfClassifier.f1(truthTest, predTest)
    val accuracy = ScoreMetricsOfClassifier.accuracy(truthTest,predTest)
    println(s"The accuracy of the original ICDM 2018 dataset is: ${accuracy}")
    val testTup = ("test", precTest, recallTest, f1Test)
    val mapToReturn = Map(name -> testTup)
    mapToReturn
  }
}
