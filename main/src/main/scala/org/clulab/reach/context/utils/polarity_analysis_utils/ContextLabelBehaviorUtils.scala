package org.clulab.reach.context.utils.polarity_analysis_utils

object ContextLabelBehaviorUtils {

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
    var freq = 0
    list.map(l => {
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

  def countLabelFrequencyInList(listOfLabels:Array[String]):Map[String, Int] = {
    val toReturn = collection.mutable.HashMap[String,Int]()
    for(l <- listOfLabels) {
      if(toReturn.contains(l)) {
        var existingCount = toReturn(l)
        existingCount += 1
        toReturn(l) = existingCount
      }

      else {
        val entry = Map(l -> 1)
        toReturn ++= entry
      }
    }
    toReturn.toMap
  }

  def countPapersUsingLabelsInList(listOfLabels: Array[String], mapOfLabelsInPaper:Map[String,collection.mutable.ListBuffer[String]]):Map[String,(Int, Array[String])] = {
    val mapToReturn = collection.mutable.HashMap[String,(Int, Array[String])]()
    for(l <- listOfLabels) {
      val paperlist = collection.mutable.ListBuffer[String]()
      var papercount = 0
      for((paperID,listOfLabelsInPaper) <- mapOfLabelsInPaper) {
        if(listOfLabelsInPaper.contains(l)) {
          papercount +=1
          paperlist += paperID
        }
      }

      val tup = (papercount, paperlist.toArray)
      val entry = Map(l -> tup)
      mapToReturn ++= entry
    }

    mapToReturn.toMap
  }

  def countLabelsPerPaper(labelsPerPaperMap: collection.mutable.HashMap[String, collection.mutable.ListBuffer[String]], uniquelyActivation:Set[String], uniquelyInhibition:Set[String], intersection:Set[String]):Map[String, (Int, Int, Array[String], Int, Array[String], Int, Array[String])] = {
    val perPaperLabelSpecs = collection.mutable.HashMap[String, (Int, Int, Array[String], Int, Array[String], Int, Array[String])]()
    for((paperID, labelList) <- labelsPerPaperMap) {
      var uniqueActivationCount = 0
      var uniqueInhibitionCount = 0
      var intersectionCount = 0
      val activationLabelsPerPaper = collection.mutable.ListBuffer[String]()
      val inhibitionLabelsPerPaper = collection.mutable.ListBuffer[String]()
      val intersectionLabelsPerPaper = collection.mutable.ListBuffer[String]()
      val labelSet = labelList.toSet
      for(l<-labelSet) {
        if(uniquelyActivation.contains(l)) {
          uniqueActivationCount += 1
          activationLabelsPerPaper += l
        }

        else if(uniquelyInhibition.contains(l)) {
          uniqueInhibitionCount += 1
          inhibitionLabelsPerPaper += l
        }

        else if(intersection.contains(l)){
          intersectionCount += 1
          intersectionLabelsPerPaper += l
        }
      }
      val frequencyOfPaperOverAllLabels = uniqueActivationCount + uniqueInhibitionCount + intersectionCount
      val tupleEntry = (frequencyOfPaperOverAllLabels, uniqueActivationCount, activationLabelsPerPaper.toArray, uniqueInhibitionCount, inhibitionLabelsPerPaper.toArray, intersectionCount, intersectionLabelsPerPaper.toArray)
      val mapEntry = Map(paperID -> tupleEntry)
      perPaperLabelSpecs ++= mapEntry
    }
    perPaperLabelSpecs.toMap
  }

  def constructAllPairsTwoSets(set1: Set[String], type1: String, set2: Set[String], type2: String): Array[(String, String, String, String)] = {
    val listOfPairs = collection.mutable.ListBuffer[(String, String, String, String)]()
    for(s1 <- set1) {
      for(s2 <- set2) {
        val entry = (s1,type1,s2,type2)
        listOfPairs += entry
      }
    }

    listOfPairs.toArray
  }

  def countCoOccurrenceOfAllPairs(arrayOfPairs:Array[(String, String, String, String)], labelsPerPaperMap: collection.mutable.HashMap[String, collection.mutable.ListBuffer[String]]):Map[(String, String, String, String), (Int, Array[String])] = {
    val coOccurrenceMap = collection.mutable.HashMap[(String, String, String, String), (Int, Array[String])]()
    for(pair <- arrayOfPairs) {
      var paperCount = 0
      val listOfPapers = collection.mutable.ListBuffer[String]()
      for((paperID, labelsInPaper) <- labelsPerPaperMap) {
        if(labelsInPaper.contains(pair._1) && labelsInPaper.contains(pair._3) && (pair._1 != pair._3)) {
          paperCount += 1
          listOfPapers += paperID
        }
      }

      if(paperCount > 0) {
        val entry = Map(pair -> (paperCount, listOfPapers.toArray))
        coOccurrenceMap ++= entry
      }

    }
    coOccurrenceMap.toMap
  }
}
