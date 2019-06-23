package org.clulab.reach.context.context_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.context_utils.ContextFeatureUtils

object CrossValBySentDist extends App {

  val config = ConfigFactory.load()
  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val unTrainedSVMInstance = new LinearSVMContextClassifier(Some(SVMClassifier))
  val labelFile = config.getString("svmContext.labelFile")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper).concat("/sentenceWindows")
  val allSentDirs = new File(dirForType).listFiles().filter(_.isDirectory)
  val scorePerSentDist = collection.mutable.HashMap[Int,(Double,Double)]()
  val allRowsBySentDist = collection.mutable.HashMap[Int, Seq[AggregatedContextInstance]]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  for(d<- allSentDirs) {
    val rowFiles = d.listFiles().filter(_.getName.contains("Aggregated"))
    val rowsForCurrentSent = collection.mutable.ListBuffer[AggregatedContextInstance]()
    for(r<-rowFiles) {
      val pathToRow = dirForType.concat(s"/${d.getName}").concat(s"/${r.getName}")
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(r)
      val row = ContextFeatureUtils.readAggRowFromFile(pathToRow)
      keysForLabels ++= Map(row -> rowSpecs)
      rowsForCurrentSent += row
    }
    val intName = Integer.parseInt(d.getName)
    val entry = Map(intName -> rowsForCurrentSent)
    allRowsBySentDist ++= entry
  }


  val foldsBySentDist = collection.mutable.HashMap[Int, Seq[(Seq[AggregatedContextInstance], Seq[AggregatedContextInstance])]]()

  for((sentDist,rows) <- allRowsBySentDist) {
    println(s"Sentence distance ${sentDist} has a total of ${rows.size} aggregated rows")
    val foldsPerSentDist = collection.mutable.ArrayBuffer[(Seq[AggregatedContextInstance], Seq[AggregatedContextInstance])]()
    for(r<-rows) {
      val trainingRows = rows.filter(_.PMCID != r.PMCID)
      val testingRows = rows.filter(_.PMCID == r.PMCID)
      val tup = (testingRows, trainingRows)
      foldsPerSentDist += tup
    }
    foldsBySentDist ++= Map(sentDist -> foldsPerSentDist)
  }


}
