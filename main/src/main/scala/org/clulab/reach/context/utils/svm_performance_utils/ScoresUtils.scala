package org.clulab.reach.context.utils.svm_performance_utils


object ScoresUtils {
  def argMax(values:Map[Int, Double]):Int = {
    var bestK = Integer.MIN_VALUE
    var bestF1 = Double.MinValue
    values.map(x => {if (x._2 > bestF1) {bestK = x._1; bestF1 = x._2}})
    bestK
  }

  def f1(preds: Map[String, Int]): Double = {
    val p = precision(preds)
    val r = recall(preds)
    if (p + r == 0) 0.0
    else ((2 * (p * r))/(p + r))
  }

  def precision(preds: Map[String, Int]): Double = {
    if(!(preds("TP").toDouble + preds("FP").toDouble == 0.toDouble)) preds("TP").toDouble / (preds("TP") + preds("FP")).toDouble
    else 0.0
  }

  def recall(preds: Map[String, Int]): Double = {
    if (!(preds("TP").toDouble + preds("FN").toDouble == 0)) preds("TP").toDouble/(preds("TP") + preds("FN")).toDouble
    else 0.0
  }


  def accuracy(preds:Map[String, Int]): Double = {
    if (!((preds("TP") + preds("FP") + preds("FN") + preds("TN").toDouble) == 0)) (preds("TP") + preds("TN")).toDouble/(preds("TP") + preds("TN") + preds("FP") + preds("FN")).toDouble
    else 0.0
  }

  def arithmeticMeanScore(scores:Seq[Double]):Double = {
    val sum = scores.foldLeft(0.0)(_ + _)
    sum/scores.size
  }

  def predictCounts(yTrue: Array[Int], yPred: Array[Int]): Map[String, Int] = {
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


  def createStats(nums: Iterable[Double]): (Double, Double, Double) = {
    val min = nums.min
    val max = nums.max
    val avg = nums.sum / nums.size
    (min, max, avg)
  }


  def findAggrMetrics(seq:Seq[Double]): (Double,Double,Double) = {
    val min = seq.foldLeft(Double.MaxValue)(Math.min(_,_))
    val max = seq.foldLeft(Double.MinValue)(Math.max(_,_))
    val sum = seq.foldLeft(0.0)(_+_)
    val avg = sum.toDouble/seq.size.toDouble
    (min,max,avg)
  }


}
