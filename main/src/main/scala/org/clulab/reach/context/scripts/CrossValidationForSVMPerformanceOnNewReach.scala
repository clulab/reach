package org.clulab.reach.context.scripts


import com.typesafe.config.ConfigFactory
import org.clulab.reach.context.utils.io_utils.ReachSystemAnalysisIOUtils

object CrossValidationForSVMPerformanceOnNewReach extends App {
  val config = ConfigFactory.load()

  // Loading the AggrRows from file. AggrRows are the output from Reach 2019.
  val reach2019RootDir = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val mapOfaggrRowsByPaperID = ReachSystemAnalysisIOUtils.getReach2019RowsByPaperID(reach2019RootDir)
  print(s"Number of papers: ${mapOfaggrRowsByPaperID.size}")
//  val parentDirForManualAnnotations = config.getString("svmContext.transferredAnnotationsParentDir")
//  val manualPredictions = ReachSystemAnalysisIOUtils.getTransferredAnnotationsFromReach2016(parentDirForManualAnnotations)
//
//  print(s"We have a total of ${manualPredictions.size} annotations")





}
