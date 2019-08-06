package org.clulab.reach.context.context_exec

import java.io.{File, FileInputStream, ObjectInputStream}

import com.typesafe.config.ConfigFactory
import org.clulab.reach.mentions.BioEventMention

object PerformPolarityAnalysis extends App {

  val config = ConfigFactory.load()
  val operatingDir = config.getString("polarityContext.contextLabelsOutputDir")
  val activationLabelsFilePath = operatingDir.concat("activationContextLabels.txt")
  val inhibitionLabelsFilePath = operatingDir.concat("inhibitionContextLabels.txt")

  val activationLine = scala.io.Source.fromFile(activationLabelsFilePath).getLines().toSeq(0)

  val inhibitionLine = scala.io.Source.fromFile(inhibitionLabelsFilePath).getLines().toSeq(0)

  val activationLabels = activationLine.split(",")
  val inhibitionLabels = inhibitionLine.split(",")
  val eventIDsByPaper = collection.mutable.HashMap[String, Array[String]]()
  val contextMentionsByPaper = collection.mutable.HashMap[String, Array[String]]()
  println("********** Non-unique activation labels *************")
  println(s"There are a total ${activationLabels.size} non-unique activation context labels")
  println(activationLabels.mkString(","))

  println("\n ********** Non-unique inhibition labels *************")
  println(s"There are a total ${inhibitionLabels.size} non-unique inhibition context labels")
  println(inhibitionLabels.mkString(","))

  val fileInstance = new File(operatingDir)
  val allPaperDirs = fileInstance.listFiles().filter(_.isDirectory)
  for(paperDir <- allPaperDirs) {
    val paperID = paperDir.getName
//    val eventsFilePath = paperID.concat("/ArrayOfEvtsByPaper.txt")
//    val eventIDLines = scala.io.Source.fromFile(eventsFilePath).getLines().toSeq(0)
//
//
//    val eventsEntry = eventIDLines.split(",")
//
//    eventIDsByPaper ++= Map(paperID -> eventsEntry)

    val contextFilePath = paperID.concat("/contextLabelsPerPaper.txt")

    val ctxLines = scala.io.Source.fromFile(contextFilePath).getLines().toSeq(0)

    val contextsForThisPaper = ctxLines.split(",")
    val contextsEntry = Map(paperID -> contextsForThisPaper)
    contextMentionsByPaper ++= contextsEntry
  }

  val uniqueActivationLabelsIncudesIntersection = activationLabels.toSet
  val uniqueInhibitionLabelsIncludesIntersection = inhibitionLabels.toSet
  val commonLabels = uniqueActivationLabelsIncudesIntersection.intersect(uniqueInhibitionLabelsIncludesIntersection)
  println(s"There are ${commonLabels.size} common context labels that appear in both activation and inhibition set. They are printed below.")
  println(commonLabels.mkString(","))
  val uniqueActivationLabelsNoIntersection = uniqueActivationLabelsIncudesIntersection -- commonLabels
  val uniqueInhibitionLabelsNoIntersection = uniqueInhibitionLabelsIncludesIntersection -- commonLabels

  println("**************** PRINTING UNIQUE ACTIVATION LABELS NOT IN THE INTERSECTION ****************")
  println(s"There are ${uniqueActivationLabelsNoIntersection.size} unique activation labels that do not include the intersection. They are: ")
  println(uniqueActivationLabelsNoIntersection.mkString(","))


  println("**************** PRINTING UNIQUE INHIBITION LABELS NOT IN THE INTERSECTION ****************")
  println(s"There are ${uniqueInhibitionLabelsNoIntersection.size} unique inhibition labels that do not include the intersection. They are: ")
  println(uniqueInhibitionLabelsNoIntersection.mkString(","))

  val frequencyOfContextLabel = collection.mutable.HashMap[String, Int]()
  val noOfPapersThatUseContextLabel = collection.mutable.HashMap[String,(Int, Array[String])]()

  // CODE TO COUNT NO. OF OCCURRENCES OF A CONTEXT LABEL OVER ALL PAPERS
  println("EXECUTING CODE TO COUNT NO. OF OCCURRENCES OF A CONTEXT LABEL OVER ALL PAPERS")
  for(act <- uniqueActivationLabelsNoIntersection) {
    var freqPerLabel = 0
    for(nu <- activationLabels) {
      if(act == nu)
        freqPerLabel += 1
    }
    val mapEntry = Map(act -> freqPerLabel)
    frequencyOfContextLabel ++= mapEntry
  }


  for(inh <- uniqueInhibitionLabelsNoIntersection) {
    var freqPerLabel = 0
    for(nu <- inhibitionLabels) {
      if(inh == nu)
        freqPerLabel += 1
    }

    val mapEntry = Map(inh -> freqPerLabel)
    frequencyOfContextLabel ++= mapEntry
  }



  // CODE TO COUNT NO. OF PAPERS IN WHICH EACH CONTEXT LABEL APPEARS
  println("EXECUTING CODE TO COUNT NO. OF PAPERS IN WHICH EACH CONTEXT LABEL APPEARS")
  for(act <- uniqueActivationLabelsNoIntersection) {
    var paperCount = 0
    val paperList = collection.mutable.ListBuffer[String]()
    for((paperID, listOfContextLabels) <- contextMentionsByPaper) {
      if(listOfContextLabels.contains(act)) {
        paperCount += 1
        paperList += paperID
      }
    }

    val entry = (paperCount, paperList.toArray)
    val map = Map(act -> entry)
    noOfPapersThatUseContextLabel ++= map
  }


  for(inh <- uniqueInhibitionLabelsNoIntersection) {
    var paperCount = 0
    val paperList = collection.mutable.ListBuffer[String]()
    for((paperID, listOfContextLabels) <- contextMentionsByPaper) {
      if(listOfContextLabels.contains(inh)) {
        paperCount += 1
        paperList += paperID
      }
    }
    val entry = (paperCount, paperList.toArray)
    val map = Map(inh -> entry)
    noOfPapersThatUseContextLabel ++= map
  }


  for((contextLabel, totalFrequency) <- frequencyOfContextLabel) {
    println(s"The context label ${contextLabel} appears totally ${totalFrequency} times")
    val paperFrequency = noOfPapersThatUseContextLabel(contextLabel)
    println(s"Total number of papers: ${contextMentionsByPaper.size}")
    println(s"The total number of occurrences is distributed over ${paperFrequency._1} papers: ${paperFrequency._2.mkString(",")}")
  }




}
