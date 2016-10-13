package org.clulab.context.ml

import scala.math.pow

case class BinaryClassificationResults(val truth:Seq[Boolean], val predictions:Seq[Boolean]){

    def this(results:Seq[(Boolean, Boolean)]){ this(results.map(_._1), results.map(_._2))}

    // Make sure that the sequences have the same length
    assert(truth.size == predictions.size)

    val size = truth.size

    lazy val truePositives = truth.zip(predictions).filter{
        case (t, p) => (t == p) && (t == true)
    }.size

    lazy val trueNegatives = truth.zip(predictions).filter{
        case (t, p) => (t == p) && (t == false)
    }.size

    lazy val falsePositives = truth.zip(predictions).filter{
        case (t, p) => (t == false) && (t != p)
    }.size

    lazy val falseNegatives = truth.zip(predictions).filter{
        case (t, p) => (t == true) && (t != p)
    }.size

    lazy val hits:Int = truth.zip(predictions).filter{ case(t, p) => t == p}.size

    lazy val positiveHits:Int = truth.zip(predictions).filter{ case(t, p) => (t == p) && (t == true)}.size

    lazy val misses:Int = size - hits

    lazy val accuracy:Double = hits / size.toDouble

    lazy val precision:Double = if((truePositives + falsePositives) > 0) truePositives.toDouble / (truePositives + falsePositives) else 0

    lazy val recall:Double = if((truePositives + falseNegatives) > 0) truePositives.toDouble / (truePositives + falseNegatives) else 0

    def fBScore(beta:Double) = (1 + pow(beta, 2))*((precision*recall)/(pow(beta, 2)*(precision + recall)))

    def f1Score = fBScore(1)

    override def toString = {
        s"P:$precision\tR:$recall\tF1:$f1Score\tHits:$hits\tPositive hits:$positiveHits\tMisses:$misses"
    }
}
