package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import java.io.File

import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.reach.context.context_utils.ContextFeatureUtils

object CrossValOldDatasetUsingEventsFile extends App {
  val config = ConfigFactory.load()
  val parentDirForRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val annotationsFileDir = config.getString("polarityContext.eventsFilesDir")
  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val labelMapFromOldDataset = CodeUtils.generateLabelMap(labelFile)
  val allPapersDirs = new File(parentDirForRows).listFiles().filter(x => x.isDirectory && x.getName != "newAnnotations")
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val allRowsByPaperID = collection.mutable.ListBuffer[(String, Seq[AggregatedContextInstance])]()
  for(paperDir <- allPapersDirs) {
    // In this code we won't face the double counting of two rows from a given paper, because each paper appears only once over all.
    // While analyzing the performance over sentence windows, we encountered the same paper over different values of sentence window. That's why we had the risk of adding the same row twice.
    // But we now see that each paper appears only once, and we read the rows from that paper. So we won't add the same row twice.
    // The only time we will see the same paper appear twice will be in the hold-one-out cross-validation phase, which is expected behavior.
    val rowFiles = paperDir.listFiles().filter(x => x.getName.contains("Aggregated"))
    val rowsForCurrentSent = collection.mutable.ListBuffer[AggregatedContextInstance]()
    var containsOnlyThisPapersRows = true
    for(r <- rowFiles) {
      containsOnlyThisPapersRows = containsOnlyThisPapersRows && (r.getName.contains(paperDir.getName))
      val pathToRow = parentDirForRows.concat(s"${paperDir.getName}").concat(s"/${r.getName}")
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(r)
      val row = ContextFeatureUtils.readAggRowFromFile(pathToRow)
      if(!rowsForCurrentSent.contains(row))
      {
        idMap ++= Map(rowSpecs -> row)
        keysForLabels ++= Map(row -> rowSpecs)
        rowsForCurrentSent += row
      }
    }
    val nameOfCurrentDirectory = paperDir.getName
    val entry = (nameOfCurrentDirectory,rowsForCurrentSent)
    //val entry = Map(nameOfCurrentDirectory -> rowsForCurrentSent)

    allRowsByPaperID += entry
  }


  println(allRowsByPaperID.size)


}
