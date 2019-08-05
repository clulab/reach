package org.clulab.reach.context.context_exec

import java.io.{File, FileInputStream, ObjectInputStream}

import com.typesafe.config.ConfigFactory
import org.clulab.reach.mentions.BioEventMention

object PerformPolarityAnalysis extends App {

  val config = ConfigFactory.load()
  val operatingDir = config.getString("polarityContext.contextLabelsOutputDir")
  val activationLabelsFilePath = operatingDir.concat("activationContextLabels.txt")
  val inhibitionLabelsFilePath = operatingDir.concat("inhibitionContextLabels.txt")
  val activationInputStream = new ObjectInputStream(new FileInputStream(activationLabelsFilePath))
  val inhibitionInputStream = new ObjectInputStream(new FileInputStream(inhibitionLabelsFilePath))
  val activationLabels = activationInputStream.readObject().asInstanceOf[Array[String]]
  val inhibitionLabels = inhibitionInputStream.readObject().asInstanceOf[Array[String]]
  val eventMentionsByPaper = collection.mutable.HashMap[String, Array[BioEventMention]]()
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
    val eventsFilePath = paperID.concat("/ArrayOfEvtsByPaper.txt")
    val eventsInputStreamForThisPaper = new ObjectInputStream(new FileInputStream(eventsFilePath))
    val eventsForThisPaper = eventsInputStreamForThisPaper.readObject().asInstanceOf[Array[BioEventMention]]
    val eventsEntry = Map(paperID -> eventsForThisPaper)
    eventMentionsByPaper ++= eventsEntry
    eventsInputStreamForThisPaper.close()


    val contextFilePath = paperID.concat("/contextLabelsPerPaper.txt")
    val contextInputStreamForThisPaper = new ObjectInputStream(new FileInputStream(contextFilePath))
    val contextsForThisPaper = contextInputStreamForThisPaper.readObject().asInstanceOf[Array[String]]
    val contextsEntry = Map(paperID -> contextsForThisPaper)
    contextMentionsByPaper ++= contextsEntry
    contextInputStreamForThisPaper.close()
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



}
