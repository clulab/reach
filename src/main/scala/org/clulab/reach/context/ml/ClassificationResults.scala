package org.clulab.reach.context.ml

import scala.math.pow

case class BinaryClassificationResults(val truth:Seq[Boolean], val predictions:Seq[Boolean]){

    def this(results:Seq[(Boolean, Boolean)]){ this(results.map(_._1), results.map(_._2))}

    // Make sure that the sequences have the same length
    assert(truth.size == predictions.size)

    val size = truth.size

    lazy val truePositives = truth.zip(predictions).filter{
        case (t, p) => t == p == true
    }.size

    lazy val trueNegatives = truth.zip(predictions).filter{
        case (t, p) => t == p == false
    }.size

    lazy val falsePositives = truth.zip(predictions).filter{
        case (t, p) => t == false && t != p
    }.size

    lazy val falseNegatives = truth.zip(predictions).filter{
        case (t, p) => t == true && t != p
    }.size

    lazy val hits = truth.zip(predictions).filter{ case(t, p) => t == p}.size

    lazy val misses = size - hits

    lazy val accuracy = hits / size

    lazy val precision = if((truePositives + falsePositives) > 0) truePositives / (truePositives + falsePositives) else 0

    lazy val recall = if((truePositives + falseNegatives) > 0) truePositives / (truePositives + falseNegatives) else 0

    def fBScore(beta:Double) = (1 + pow(beta, 2))*((precision*recall)/(pow(beta, 2)*(precision + recall)))

    def f1Score = fBScore(1)

    override def toString = {
        s"P:$precision\tR:$recall\tF1:$f1Score"
    }
}
