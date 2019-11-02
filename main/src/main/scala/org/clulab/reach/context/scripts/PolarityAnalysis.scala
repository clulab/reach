package org.clulab.reach.context.scripts

import java.io.{File, PrintWriter}

import com.typesafe.config.ConfigFactory
import org.clulab.reach.context.utils.polarity_analysis_utils.ContextLabelCountUtils

import scala.collection.immutable.ListMap

object PolarityAnalysis extends App {

  val config = ConfigFactory.load()
  val operatingDirPath = config.getString("polarityContext.outputForPolarityAnalysisDir")
  val (activationLabelsNonUnique, inhibitionLabelsNonUnique, contextsPerPaperMap) = ContextLabelCountUtils.classifyPolaritiesFromOutfiles(operatingDirPath)
  val allNonUniqueLabels = collection.mutable.ListBuffer[String]()

  allNonUniqueLabels ++= activationLabelsNonUnique // adding labels from activation polarity
  allNonUniqueLabels ++= inhibitionLabelsNonUnique // adding labels from inhibition polarity
  val frequencyOfAllNonUniqueLabels = ContextLabelCountUtils.countLabelFrequencyInList(allNonUniqueLabels.toArray)
  val parentPapersCountAllNonUniqueLabels = ContextLabelCountUtils.countPapersUsingLabelsInList(allNonUniqueLabels.toArray, contextsPerPaperMap)
  val composeAllLabelsResult = collection.mutable.HashMap[String,(Int, Int, Array[String])]()
  print(composeAllLabelsResult.keySet)
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


  //println(s"\n ************ There are ${exclusivelyActivation.size} unique activation labels (not including intersection), and they are:  ************ ")
  val outputFilePath = config.getString("polarityContext.outputForPolarityAnalysisFile")
  val outputFile = new File(outputFilePath)
  if(!outputFile.exists())
    outputFile.createNewFile()
  val printWriter = new PrintWriter(outputFile)
  println(s"Total number of papers:${contextsPerPaperMap.size}")
  //printWriter.append(s"Total number of papers: ${contextsPerPaperMap.size}\n")




  // CODE FOR COUNTING THE FREQUENCY OF EACH LABEL

  val activationParentPaperCountMap = sortedParentPaperMapPart1.filterKeys(exclusivelyActivation.contains(_))
  val inhibitionParentPaperCountMap = sortedParentPaperMapPart1.filterKeys(exclusivelyInhibition.contains(_))
  val intersectionParentPaperCountMap = sortedParentPaperMapPart1.filterKeys(commonLabels.contains(_))


  println(s"There are ${commonLabels.size} common labels, but ${intersectionParentPaperCountMap.size} common labels are detected in the map")
  println(s"There are ${exclusivelyActivation.size} unique activation labels, but ${activationParentPaperCountMap.size} labels are detected in the map")
  println(s"There are ${exclusivelyInhibition.size} unique inhibition labels, but ${inhibitionParentPaperCountMap.size} labels are detected in the map")
 // **********



  // ***********
  // CODE FOR COUNTING THE NUMBER OF LABELS IN EACH PAPER ******


  val labelDistributionPerPaper = ContextLabelCountUtils.countLabelsPerPaper(contextsPerPaperMap, exclusivelyActivation, exclusivelyInhibition, commonLabels)

  val sortedLabelDistributionPerPaper = ListMap(labelDistributionPerPaper.toSeq.sortWith(_._2._1 > _._2._1):_*)




  // ********** CODE FOR COUNTING HOW MANY TIMES EACH LABEL PAIR APPEARS ******


    val acrossPolarityPairs = collection.mutable.ListBuffer[(String,String,String,String)]()
    val pairListFromActInh = ContextLabelCountUtils.constructAllPairsTwoSets(exclusivelyActivation,"activation",exclusivelyInhibition, "inhibition")
    val pairListFromActInter = ContextLabelCountUtils.constructAllPairsTwoSets(exclusivelyActivation, "activation", commonLabels, "intersection")
    val pairListFromInhInter = ContextLabelCountUtils.constructAllPairsTwoSets(exclusivelyInhibition, "inhibition", commonLabels, "intersection")
  acrossPolarityPairs ++= pairListFromActInh
  acrossPolarityPairs ++= pairListFromActInter
  acrossPolarityPairs ++= pairListFromInhInter




    val pairListFromActAct = ContextLabelCountUtils.constructAllPairsTwoSets(exclusivelyActivation,"activation",exclusivelyActivation,"activation")
    val pairListFromInhInh = ContextLabelCountUtils.constructAllPairsTwoSets(exclusivelyInhibition, "inhibition", exclusivelyInhibition, "inhibition")
    val pairListFromInterInter = ContextLabelCountUtils.constructAllPairsTwoSets(commonLabels, "intersection", commonLabels, "intersection")


  // counting co-occurrence of labels of the same polarity
  val coOccurrenceActAct = ContextLabelCountUtils.countCoOccurrenceOfAllPairs(pairListFromActAct, contextsPerPaperMap)
  val sortedcoOccurrenceActAct = ListMap(coOccurrenceActAct.toSeq.sortWith(_._2._1 > _._2._1):_*)
  val coOccurrenceInhInh = ContextLabelCountUtils.countCoOccurrenceOfAllPairs(pairListFromInhInh, contextsPerPaperMap)
  val sortedcoOccurrenceInhInh = ListMap(coOccurrenceInhInh.toSeq.sortWith(_._2._1 > _._2._1):_*)
  val coOccurrenceInterInter = ContextLabelCountUtils.countCoOccurrenceOfAllPairs(pairListFromInterInter, contextsPerPaperMap)
  val sortedcoOccurrenceInterInter  = ListMap(coOccurrenceInterInter.toSeq.sortWith(_._2._1 > _._2._1):_*)

  val coOccurrenceAcrossPolarity = ContextLabelCountUtils.countCoOccurrenceOfAllPairs(acrossPolarityPairs.toArray, contextsPerPaperMap)
  val sortedcoOccurrenceAcrossPolarity  = ListMap(coOccurrenceAcrossPolarity.toSeq.sortWith(_._2._1 > _._2._1):_*)

  for((pair,paperFreq) <- sortedcoOccurrenceAcrossPolarity) {
    val listOfPapers = paperFreq._2.mkString("*")
    println(s"${pair._1},${pair._2},${pair._3},${pair._4},${paperFreq._1},${listOfPapers}")
  }



}
