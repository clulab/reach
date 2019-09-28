package org.clulab.reach.context.utils.score_utils

import org.clulab.context.utils.CodeUtils.{precision, recall}

object ScoreMetricsOfClassifier {
  def argMax(values:Map[Int, Double]):Int = {
    var bestK = Integer.MIN_VALUE
    var bestF1 = Double.MinValue
    values.map(x => {if (x._2 > bestF1) {bestK = x._1; bestF1 = x._2}})
    bestK
  }

  def f1(yTrue: Array[Int], yPred: Array[Int]): Double = {
    val p = precision(yTrue, yPred)
    val r = recall(yTrue, yPred)
    if (p + r == 0) 0.0
    else ((2 * (p * r))/(p + r))
  }
}
