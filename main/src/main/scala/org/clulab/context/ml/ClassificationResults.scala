package org.clulab.context.ml

import scala.math.pow

case class BinaryClassificationResults(truth:Seq[Boolean], predictions:Seq[Boolean]){

    def this(results:Seq[(Boolean, Boolean)]){ this(results.map(_._1), results.map(_._2))}

    // Make sure that the sequences have the same length
    assert(truth.size == predictions.size)

    val size:Int = truth.size

    lazy val truePositives:Int = truth.zip(predictions).count{
        case (t, p) => (t == p) && (t == true)
    }

    lazy val trueNegatives:Int = truth.zip(predictions).count{
        case (t, p) => (t == p) && (t == false)
    }

    lazy val falsePositives:Int = truth.zip(predictions).count{
        case (t, p) => (t == false) && (t != p)
    }

    lazy val falseNegatives:Int = truth.zip(predictions).count{
        case (t, p) => (t == true) && (t != p)
    }

    lazy val hits:Int = truth.zip(predictions).count{ case(t, p) => t == p}

    lazy val positiveHits:Int = truth.zip(predictions).count{ case(t, p) => (t == p) && (t == true)}

    lazy val misses:Int = size - hits

    lazy val accuracy:Double = hits / size.toDouble

    lazy val precision:Double = if((truePositives + falsePositives) > 0) truePositives.toDouble / (truePositives + falsePositives) else 0

    lazy val recall:Double = if((truePositives + falseNegatives) > 0) truePositives.toDouble / (truePositives + falseNegatives) else 0


    def fBScore(beta:Double):Double = (1 + pow(beta, 2))*((precision*recall)/(pow(beta, 2)*(precision + recall)))

    def f1Score:Double = fBScore(1)

    def confusionMatrix:String = s"\t\tPredicted P\t|\tPredicted N\t|\t\nActual P\t|\t$truePositives\t|\t$falseNegatives\nActual N\t|\t$falsePositives\t|\t$trueNegatives\nTotal items:\t|\t${trueNegatives+truePositives+falseNegatives+falsePositives}"

    override def toString:String = {
        s"P:$precision\tR:$recall\tF1:$f1Score\tHits:$hits\tPositive hits:$positiveHits\tMisses:$misses\b\n$confusionMatrix\n"
    }
}
