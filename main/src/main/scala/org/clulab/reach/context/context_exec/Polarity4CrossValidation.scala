package org.clulab.reach.context.context_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.reach.context.context_exec.CrossValBySentDist.config

object Polarity4CrossValidation extends App {
  val config = ConfigFactory.load()
  val labelFile = config.getString("svmContext.labelFile")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper).concat("/sentenceWindows")
  val allSentDirs = new File(dirForType).listFiles().filter(_.isDirectory)

  val allRowsBySentDist = collection.mutable.HashMap[Int, Seq[AggregatedContextInstance]]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val filterForFasterRun = List("0","1","2","3")
  val smallNumOfDirs = allSentDirs.filter(x => filterForFasterRun.contains(x.getName))
}
