package org.clulab.reach.context.context_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.context_utils.ContextFeatureUtils

object CrossValBySentDist extends App {

  val config = ConfigFactory.load()
  val svmWrapper = new LinearSVMContextClassifier()
  val configPath = config.getString("contextEngine.params.trainedSvmPath")
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }

  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")
  val labelFile = config.getString("svmContext.labelFile")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper).concat("/sentenceWindows")
  val allSentDirs = new File(dirForType).listFiles().filter(_.isDirectory)
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val allRowsBySentDist = collection.mutable.HashMap[Int, Seq[AggregatedContextInstance]]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val filterForFasterRun = List("12","13","14","15")
  val smallNumOfDirs = allSentDirs.filter(x => filterForFasterRun.contains(x.getName))
  for(d<- smallNumOfDirs) {
    val rowFiles = d.listFiles().filter(_.getName.contains("Aggregated"))
    val rowsForCurrentSent = collection.mutable.ListBuffer[AggregatedContextInstance]()
    for(r<-rowFiles) {
      val pathToRow = dirForType.concat(s"/${d.getName}").concat(s"/${r.getName}")
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(r)
      val row = ContextFeatureUtils.readAggRowFromFile(pathToRow)
      val pred = trainedSVMInstance.predict(Seq(row))
      if(pred(0) != 0)
      {idMap ++= Map(rowSpecs -> row)
      keysForLabels ++= Map(row -> rowSpecs)
      rowsForCurrentSent += row}
    }
    val intName = Integer.parseInt(d.getName)
    val entry = Map(intName -> rowsForCurrentSent)
    allRowsBySentDist ++= entry
  }

  val giantScoreBoard = collection.mutable.HashMap[Int, Double]()
  for((sentist, rows) <- allRowsBySentDist) {
    val perSentPred = collection.mutable.ListBuffer[Int]()
    val perSentTruth = collection.mutable.ListBuffer[Int]()
    /*val nonZeroRows = rows.filter(x => {
      val predPerRow = trainedSVMInstance.predict(Seq(x))
      predPerRow(0) != 0
    })*/
    val labelIDsForInterSection = rows.map(keysForLabels(_))
    val intersectingAnnotations = labelIDsForInterSection.toSet.intersect(CodeUtils.generateLabelMap(labelFile).keySet)
    val commonRows = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val commonLabels = collection.mutable.ListBuffer[Int]()

    for(tup <- intersectingAnnotations) {
      val row = idMap(tup)
      val label = CodeUtils.generateLabelMap(labelFile)(tup)
      commonRows += row
      commonLabels += label
    }

    val preds = trainedSVMInstance.predict(commonRows)
    perSentTruth ++= commonLabels
    perSentPred ++= preds
    for(row <- commonRows) {
      val id = keysForLabels(row)
      val index = commonRows.indexOf(row)
      val partPred = preds(index)
      print(s"\n The ID tup ${id} has the prediction ${partPred}")
    }
    val counts =  CodeUtils.predictCounts(perSentTruth.toArray, perSentPred.toArray)
    val prec = CodeUtils.precision(counts)
    giantScoreBoard ++= Map(sentist -> prec)
  }

  for((sent, prec) <- giantScoreBoard) {
    println(s"\n The sentence distance ${sent} has micro-averaged precision of ${prec}")
  }



}
