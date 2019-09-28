package org.clulab.reach.context.utils.feature_utils

import java.io.{FileInputStream, ObjectInputStream}


object FeatureNameCruncher {
  def createMinMaxAvgStatsOfFeatures(nums: Iterable[Double]): (Double, Double, Double) = {
    val min = nums.min
    val max = nums.max
    val avg = nums.sum / nums.size
    (min, max, avg)
  }

  def extendFeatureNameToMinMaxAvg(f:String):(String, String, String) = {

    val feat_min = s"${f}_min"
    val feat_max = s"${f}_max"
    val feat_avg = s"${f}_avg"
    (feat_min, feat_max, feat_avg)

  }

  def resolveUnaggregatedFeatureNameForContextPairInstance(seq: Seq[String], take: Int):Seq[String] = {
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

}
