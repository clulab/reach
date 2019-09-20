package org.clulab.context.utils

import java.io._
import java.util.zip._

import org.clulab.learning.{RVFDataset, RVFDatum}

import scala.collection.mutable
import scala.io.Source

object Scores_IO_Utils {
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

  def extendFeatureName(f:String):(String, String, String) = {

    val feat_min = s"${f}_min"
    val feat_max = s"${f}_max"
    val feat_avg = s"${f}_avg"
    (feat_min, feat_max, feat_avg)

  }

  def resolveUnaggregatedFeatureName(seq: Seq[String], take: Int):Seq[String] = {
    val result = collection.mutable.ListBuffer[String]()
    val ids = seq.take(take)
    val numericalFeatureNames = seq.drop(take)
    result ++= ids
    val miniList = collection.mutable.ListBuffer[String]()
    numericalFeatureNames.map(m => {
      val lim = m.length-4
      val slice = m.slice(0,lim)
      miniList += slice
    })
    result ++=miniList.toSet.toSeq
    result
  }


  def readHardcodedFeaturesFromFile(fileName: String):Array[String] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val headers = is.readObject().asInstanceOf[Array[String]]
    headers
  }


//  def featureConstructor(file:String):Map[String, Seq[String]] = {
//    val is = new ObjectInputStream(new FileInputStream(file))
//    val headers = is.readObject().asInstanceOf[Array[String]]
//    val rectifiedHeaders = rectifyWrongFeatures(headers)
//    is.close()
//    createBestFeatureSet(rectifiedHeaders)
//  }
//
// private def rectifyWrongFeatures(headers:Seq[String]): Seq[String] = {
//    val result = collection.mutable.ListBuffer[String]()
//    headers.map(h => if(headers.indexOf(h) == 1) result += "PMCID" else result += h)
//    result
//  }
//
//  def createBestFeatureSet(allFeatures:Seq[String]):Map[String, Seq[String]] = {
//    val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
//    val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
//    val featureDict = Scores_IO_Features_Utils.createFeatureDictionary(numericFeatures.toSeq)
//    featureDict
//  }
//
//  def createFeatureDictionary(numericFeatures: Seq[String]):Map[String, Seq[String]] = {
//    val contextDepFeatures = numericFeatures.filter(_.startsWith("ctxDepTail"))
//    val eventDepFeatures = numericFeatures.filter(_.startsWith("evtDepTail"))
//    val nonDepFeatures = numericFeatures.toSet -- (contextDepFeatures.toSet ++ eventDepFeatures.toSet)
//    val map = collection.mutable.Map[String, Seq[String]]()
//    map += ("All_features" -> numericFeatures)
//    map += ("Non_Dependency_Features" -> nonDepFeatures.toSeq)
//    map += ("NonDep_Context" -> (nonDepFeatures ++ contextDepFeatures.toSet).toSeq)
//    map += ("NonDep_Event" -> (nonDepFeatures ++ eventDepFeatures.toSet).toSeq)
//    map += ("Context_Event" -> (contextDepFeatures.toSet ++ eventDepFeatures.toSet).toSeq)
//    map.toMap
//  }


  def findAggrMetrics(seq:Seq[Double]): (Double,Double,Double) = {
    val min = seq.foldLeft(Double.MaxValue)(Math.min(_,_))
    val max = seq.foldLeft(Double.MinValue)(Math.max(_,_))
    val sum = seq.foldLeft(0.0)(_+_)
    val avg = sum.toDouble/seq.size.toDouble
    (min,max,avg)
  }


}
