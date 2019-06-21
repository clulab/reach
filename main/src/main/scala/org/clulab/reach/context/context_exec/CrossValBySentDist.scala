package org.clulab.reach.context.context_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.context_utils.ContextFeatureUtils

class CrossValBySentDist(testAggrRows: Seq[AggregatedContextInstance]) {

  val config = ConfigFactory.load()
  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val unTrainedSVMInstance = new LinearSVMContextClassifier(Some(SVMClassifier))
  val labelFile = config.getString("svmContext.labelFile")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/")
  val fileListUnfiltered = new File(outPaperDirPath)
  val directories = fileListUnfiltered.listFiles().filter(_.isDirectory)
  val rowsSup = collection.mutable.ArrayBuffer[AggregatedContextInstance]()
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  def performCrossVal(): Unit = {
    if(testAggrRows.size > 0)
      {
        for(d<-directories) {
          val rowFiles = d.listFiles().filter(_.getName.contains("Aggregated"))
          val rows = rowFiles.map(file => {
            val pmcid = file.getName.split("_")(1)
            val evtID = file.getName.split("_")(2)
            val ctxID = file.getName.split("_")(3)
            val ctxID2 = ctxID.slice(0,ctxID.length-4)
            val filePath = outPaperDirPath.concat(pmcid).concat(s"/${file.getName}")
            val row = ContextFeatureUtils.readAggRowFromFile(filePath)
            val tuple = (pmcid,evtID,ctxID2)
            val mapEntry = Map(tuple -> row)
            idMap ++= mapEntry
            row
          })
          rowsSup ++= rows
        }



        val groupedByPaperID = rowsSup.groupBy(row => s"PMC${row.PMCID.split("_")(0)}")


        val folds = collection.mutable.ArrayBuffer[(Seq[AggregatedContextInstance], Seq[AggregatedContextInstance])]()




        groupedByPaperID.keySet.map(_=> {
          val testPMCID = s"PMC${testAggrRows(0).PMCID.split("_")(0)}"
          val trainingKeys = groupedByPaperID.keySet.filter(_ != testPMCID)
          val trainingRows = trainingKeys.map(key => {
            groupedByPaperID(key)
          })
          val testingRows = groupedByPaperID(testPMCID)
          val perFold = (testingRows, trainingRows.flatten.toSeq)
          folds += perFold

        })



        val giantTruthLabel = collection.mutable.ListBuffer[Int]()
        val giantPredictedLabel = collection.mutable.ListBuffer[Int]()

        for(t<- testAggrRows)
          println(t)
      }
  }

}
