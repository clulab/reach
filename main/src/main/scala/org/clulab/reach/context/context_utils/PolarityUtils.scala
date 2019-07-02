package org.clulab.reach.context.context_utils

object PolarityUtils {

  def countFrequencyOfString(seq: Seq[String]): Map[String, Int] = {
    val map = collection.mutable.HashMap[String, Int]()
    for(s <- seq) {
      if(map.contains(s)) {
        var freq = map(s)
        freq += 1
        map ++= Map(s -> freq)
      }
      else {
        map ++= Map(s -> 1)
      }
    }
    map.toMap
  }

  def addAllFreq(tups: Seq[(String, Int)]):Seq[(String, Int)] = {
    val groupedByCtxLabel = tups.groupBy(x => x._1)
    val toReturn = collection.mutable.ListBuffer[(String, Int)]()
    for((name, list) <- groupedByCtxLabel) {
      val nums = list.map(_._2)
      val sumPerPaper = nums.foldLeft(0)(_ + _)
      val entry = (name, sumPerPaper)
      toReturn += entry
    }
    toReturn
  }

  def countOccurrencesOfStringinList(string: String, list: List[String]):Int = {
    println(s"Current search string: ${string}")
    var freq = 0
    list.map(l => {
      println(s"Current string in list of all context mentions: ${l}")
      if(l == string) freq += 1
    })
    freq
  }

  def countOccurrencesOfStringInPaper(string: String, paperCtxs: Map[String, Seq[String]]): (Int, Seq[String]) = {
    var freq = 0
    val listOfPapers = collection.mutable.ListBuffer[String]()
    for((paperID, ctxLabels) <- paperCtxs) {
      if(ctxLabels.contains(string)) {
        freq += 1
        listOfPapers += paperID
      }
    }
    (freq, listOfPapers)
  }
}
