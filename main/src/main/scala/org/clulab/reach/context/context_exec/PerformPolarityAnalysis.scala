package org.clulab.reach.context.context_exec

import java.io.{File, FileInputStream, ObjectInputStream, PrintWriter}

import scala.collection.immutable.ListMap
import scala.io
import com.typesafe.config.ConfigFactory
import org.clulab.reach.mentions.BioEventMention

import scala.io.Source

object PerformPolarityAnalysis extends App {

  val config = ConfigFactory.load()
  val operatingDirPath = config.getString("polarityContext.outputForPolarityAnalysisDir")
  val operatingDirFile = new File(operatingDirPath)
  val paperDirs = operatingDirFile.listFiles().filter(_.isDirectory)
  val contextsPerPaperMap = collection.mutable.HashMap[String, collection.mutable.ListBuffer[String]]()
  val activationLabelsNonUnique = collection.mutable.ListBuffer[String]()
  val inhibitionLabelsNonUnique = collection.mutable.ListBuffer[String]()
  for(pDir <- paperDirs) {
    val contextsFiles = pDir.listFiles().filter(x => x.getName().contains("ContextsForEvent"))
    for(contextsFile <- contextsFiles) {
      val contextFileName = contextsFile.getName()
      val polarity1 = contextFileName.split("_")(2)
      val polarity = polarity1.slice(0, polarity1.length - 4)
      val paperID = pDir.getName()
      val fileContents = Source.fromFile(contextsFile).getLines().toSeq
      val labelsTemp = fileContents(0).split(",")
      val labels = collection.mutable.ListBuffer[String]()
      labelsTemp.map(x => labels += x)
      if(contextsPerPaperMap.contains(paperID)) {
        val currentList = contextsPerPaperMap(paperID)
        currentList ++= labels
      }
      else {
        val entry = Map(paperID -> labels)
        contextsPerPaperMap ++= entry
      }

      if(polarity == "activation") activationLabelsNonUnique ++= labels
      else inhibitionLabelsNonUnique ++= labels
    }
  }
  val allNonUniqueLabels = collection.mutable.ListBuffer[String]()
  allNonUniqueLabels ++= activationLabelsNonUnique
  allNonUniqueLabels ++= inhibitionLabelsNonUnique
  val frequencyOfAllNonUniqueLabels = countLabelFrequencyInList(allNonUniqueLabels.toArray)
  val parentPapersCountAllNonUniqueLabels = countPapersUsingLabelsInList(allNonUniqueLabels.toArray, contextsPerPaperMap.toMap)
  val composeAllLabelsResult = collection.mutable.HashMap[String,(Int, Int, Array[String])]()
  for(a <- allNonUniqueLabels) {
    val frequency = frequencyOfAllNonUniqueLabels(a)
    val paperMetrics = parentPapersCountAllNonUniqueLabels(a)
    val tup = (frequency, paperMetrics._1, paperMetrics._2)
    val entry = Map(a -> tup)
    composeAllLabelsResult ++= entry
  }
  val sortedParentPaperMapPart1 = ListMap(composeAllLabelsResult.toSeq.sortWith(_._2._2 > _._2._2):_*)
  val uniqueActivationIntersectIncluded = activationLabelsNonUnique.toSet
  val uniqueInhibitionIntersectIncluded = inhibitionLabelsNonUnique.toSet
  val commonLabels = uniqueActivationIntersectIncluded.intersect(uniqueInhibitionIntersectIncluded)
  val exclusivelyActivation = uniqueActivationIntersectIncluded -- commonLabels
  val exclusivelyInhibition = uniqueInhibitionIntersectIncluded -- commonLabels
  val activationParentPaperCountMap = sortedParentPaperMapPart1.filterKeys(exclusivelyActivation.contains(_))
  val inhibitionParentPaperCountMap = sortedParentPaperMapPart1.filterKeys(exclusivelyInhibition.contains(_))
  val intersectionParentPaperCountMap = sortedParentPaperMapPart1.filterKeys(commonLabels.contains(_))
//
//
//
//  println("\n ****** PRINTING COMMON LABELS ******")
//  println(s"There are ${commonLabels.size} common labels")
//  commonLabels.map(println)
//
//


//  println(s"PRINTING ALL NON-UNIQUE ACTIVATION LABELS")
//  println(s"${activationLabelsNonUnique.mkString("*activation*")}")
//
//
//  println(s"PRINTING ALL NON-UNIQUE INHIBITION LABELS")
//  println(s"${inhibitionLabelsNonUnique.mkString("*inhibition*")}")

  //println(s"\n ************ There are ${exclusivelyActivation.size} unique activation labels (not including intersection), and they are:  ************ ")
  val outputFilePath = config.getString("polarityContext.outputForPolarityAnalysisFile")
  val outputFile = new File(outputFilePath)
  if(!outputFile.exists())
    outputFile.createNewFile()
  val printWriter = new PrintWriter(outputFile)
  println(s"Total number of papers:${contextsPerPaperMap.size}")
  printWriter.append(s"Total number of papers: ${contextsPerPaperMap.size}\n")
  for((excAct, (frequency, paperCount, paperList)) <- activationParentPaperCountMap) {
    println(s"activation,${excAct},${frequency},${paperCount},papers:${paperList.mkString("*")}")
    printWriter.append(s"activation,${excAct},${frequency},${paperCount},papers:${paperList.mkString("*")}\n")
  }



  //println(s"\n ************ There are ${exclusivelyInhibition.size} unique inhibition labels (not including intersection), and they are:  ************ ")
  for((excInh, (frequency, paperCount, paperList)) <- inhibitionParentPaperCountMap) {
    println(s"inhibition,${excInh},${frequency},${paperCount},papers:${paperList.mkString("*")}")
    printWriter.append(s"inhibition,${excInh},${frequency},${paperCount},papers:${paperList.mkString("*")}\n")
  }


  for((intersectingLabel, (frequency, paperCount, paperList)) <- intersectionParentPaperCountMap) {
    println(s"intersection,${intersectingLabel},${frequency},${paperCount},papers:${paperList.mkString("*")}")
    printWriter.append(s"intersection,${intersectingLabel},${frequency},${paperCount},papers:${paperList.mkString("*")}\n")
  }

  println(s"There are ${commonLabels.size} common labels, but ${intersectionParentPaperCountMap.size} common labels are detected in the map")
  println(s"There are ${exclusivelyActivation.size} unique activation labels, but ${activationParentPaperCountMap.size} labels are detected in the map")
  println(s"There are ${exclusivelyInhibition.size} unique inhibition labels, but ${inhibitionParentPaperCountMap.size} labels are detected in the map")
  val labelDistributionPerPaper = countLabelsPerPaper(contextsPerPaperMap, exclusivelyActivation, exclusivelyInhibition, commonLabels)
  //val sortedParentPaperMapPart1 = ListMap(composeAllLabelsResult.toSeq.sortWith(_._2._2 > _._2._2):_*)

  val sortedLabelDistributionPerPaper = ListMap(labelDistributionPerPaper.toSeq.sortWith(_._2._1 > _._2._1):_*)

  for((paperID, labelTup) <- sortedLabelDistributionPerPaper) {
    val totalCount = labelTup._1
    val activCount = labelTup._2
    val activList = labelTup._3.mkString("*")
    val inhibCount = labelTup._4
    val inhibList = labelTup._5.mkString("*")
    val intersectCount = labelTup._6
    val intersectList = labelTup._7.mkString("*")
    println(s"${paperID},${totalCount},${activCount},${activList},${inhibCount},${inhibList},${intersectCount},${intersectList}")
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

}
