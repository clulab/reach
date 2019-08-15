package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.AggregatedContextInstance
import java.io.File

import org.clulab.reach.context.context_utils.ContextFeatureUtils
object PerformCrossValOldDataset extends App {
  val config = ConfigFactory.load()
  val svmWrapper = new LinearSVMContextClassifier()
  val configPath = config.getString("svmContext.untrainedSVMPath")
  val unTrainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = unTrainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }

  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")

  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val parentDirForRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val allPapersDirs = new File(parentDirForRows).listFiles().filter(_.isDirectory)
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val allRowsByPaperID = collection.mutable.HashMap[String, Seq[AggregatedContextInstance]]()
  for(paperDir <- allPapersDirs) {
    // In this code we won't face the double counting of two rows from a given paper, because each paper appears only once over all.
    // While analyzing the performance over sentence windows, we encountered the same paper over different values of sentence window. That's why we had the risk of adding the same row twice.
    // But we now see that each paper appears only once, and we read the rows from that paper. So we won't add the same row twice.
    // The only time we will see the same paper appear twice will be in the hold-one-out cross-validation phase, which is expected behavior.
    val rowFiles = paperDir.listFiles().filter(_.getName.contains("Aggregated"))
    val rowsForCurrentSent = collection.mutable.ListBuffer[AggregatedContextInstance]()
    for(r <- rowFiles) {
      // REMEMBER TO FILTER OUT THE NEGATIVE PREDICTIONS LATER ON
      val pathToRow = parentDirForRows.concat(s"${paperDir.getName}").concat(s"/${r.getName}")
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(r)
      val row = ContextFeatureUtils.readAggRowFromFile(pathToRow)
      idMap ++= Map(rowSpecs -> row)
      keysForLabels ++= Map(row -> rowSpecs)
      rowsForCurrentSent += row
    }
    val nameOfCurrentDirectory = paperDir.getName
    val entry = Map(nameOfCurrentDirectory -> rowsForCurrentSent)
    allRowsByPaperID ++= entry
  }

  println(s"We have a total of ${allRowsByPaperID.size} papers, and the micro-averaged precision will be calculated by treating each paper as a test case in the cross validation loop")
  val giantScoreBoard = collection.mutable.HashMap[String, Double]()
  for((paperID, rowsPerPaper) <- allRowsByPaperID) {

  }
}
