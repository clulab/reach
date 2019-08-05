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
  println(activationLabels.mkString(","))

  println("\n ********** Non-unique inhibition labels *************")
  println(inhibitionLabels.mkString(","))

  val fileInstance = new File(operatingDir)
  val allPaperDirs = fileInstance.listFiles().filter(_.isDirectory)
  for(paperDir <- allPaperDirs) {
    val paperID = paperDir.getName
    val eventsInputStreamForThisPaper = new ObjectInputStream(new FileInputStream(paperID))
    val eventsForThisPaper = eventsInputStreamForThisPaper.readObject().asInstanceOf[Array[BioEventMention]]
    val eventsEntry = Map(paperID -> eventsForThisPaper)
    eventMentionsByPaper ++= eventsEntry
    eventsInputStreamForThisPaper.close()
  }


}
