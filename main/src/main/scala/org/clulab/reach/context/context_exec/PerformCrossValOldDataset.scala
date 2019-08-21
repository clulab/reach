package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import java.io.{File, PrintWriter}

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

  // The difference between the old papers parsed in the old reach vs the new Reach involves an extra token in the event. i.e. for a given paper and context label, the old Reach captured the event 0045, but the new Reach may capture it as 0046.
  // This is the same event, it only includes one extra token. But if we do an exact string match the way we did in the CrossValBySentDist code, we will get an empty set.
  // To combat this, this code will parse the events ID as an integer, and only consider those events as the same if they math.abs(a-b) <= 1. That means, if the integer values differ by 1, they are just one token away.

  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val labelMapFromOldDataset = CodeUtils.generateLabelMap(labelFile)
  val parentDirForRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val predictionFilePath = parentDirForRows.concat("/predictionsOldDataset_1.txt")
  val predsFile = new File(predictionFilePath)
  if(!predsFile.exists())
    predsFile.createNewFile()
  val printWriter = new PrintWriter(predsFile)
  val allPapersDirs = new File(parentDirForRows).listFiles().filter(x => x.isDirectory && x.getName != "newAnnotations")
  // creating a subset of small number of papers for debugging. Use dirsToUseForDebug on line 37 for debugging
  val smallSetOfPapers = List("PMC2156142", "PMC2195994", "PMC2743561", "PMC2064697", "PMC2193052", "PMC2196001")
  val dirsToUseForDebug = allPapersDirs.filter(x => smallSetOfPapers.contains(x.getName))
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val allRowsByPaperID = collection.mutable.ListBuffer[(String, Seq[AggregatedContextInstance])]()
  for(paperDir <- dirsToUseForDebug) {
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
    println(s"The current paper ${nameOfCurrentDirectory} has ${rowsForCurrentSent.size} rows")
    allRowsByPaperID += entry
  }

  val precisionScoreBoardPerPaper = collection.mutable.HashMap[String, Double]()
  val recallScoreBoardPerPaper = collection.mutable.HashMap[String, Double]()
  val giantPredictedLabels = collection.mutable.ListBuffer[Int]()
  val giantTruthLabels = collection.mutable.ListBuffer[Int]()
  val quickerFixer = 2
  // in the cross validation, each paper will be considered as test case once. So when a given paper is a test case, all other papers and their corresponding labels must be the training case.
  for((paperID, testRowsPerPaper) <- allRowsByPaperID) {
    val truthLabelsForThisPaper = collection.mutable.ListBuffer[Int]()
    val predictedLabelsForThisPaper = collection.mutable.ListBuffer[Int]()
    val trainingCaseRowsUnFiltered = allRowsByPaperID.filter(_._1 != paperID)
    val trainRowsNeedsProcessing = collection.mutable.ListBuffer[AggregatedContextInstance]()
    for((_,tRows) <- trainingCaseRowsUnFiltered) trainRowsNeedsProcessing ++= tRows
    println(s"When ${paperID} is the test case,")
    println(s"Without checking for matching annotations, we have a total of ${trainRowsNeedsProcessing.size} rows for training")
    val trainingRowsWithCorrectLabels = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val trainingLabels = collection.mutable.ListBuffer[Int]()
    for(t <- trainRowsNeedsProcessing) {
      val specForCurrentRow = keysForLabels(t)
      val evtIDInt = Integer.parseInt(specForCurrentRow._2)
      // getting the possible events that have the same paper ID and context ID
      val possibleMatchesInLabelFile = labelMapFromOldDataset.filter(x => {x._1._1 == specForCurrentRow._1 && x._1._3 == specForCurrentRow._3})
      for((id,lab) <- possibleMatchesInLabelFile) {
        val intId = Integer.parseInt(id._2)
        if(Math.abs(intId - evtIDInt) <= quickerFixer && !trainingRowsWithCorrectLabels.contains(t)) {
          trainingRowsWithCorrectLabels += t
          trainingLabels += lab

        }
      }
    }
    println(s"Current test case: ${paperID}")
    println(s"Size of training rows after filtering by appropriate event IDs: ${trainingRowsWithCorrectLabels.size}")
    println(s"Size of training labels after filtering by appropriate event IDs: ${trainingLabels.size}")

    val (trainingRVFDataset, _) = unTrainedSVMInstance.dataConverter(trainingRowsWithCorrectLabels,Some(trainingLabels.toArray))

    unTrainedSVMInstance.fit(trainingRVFDataset)


    for(testRow <- testRowsPerPaper) {
      val pred = unTrainedSVMInstance.predict(Seq(testRow))
      printWriter.write(s"${pred(0)}\n")

      if(pred(0)!=0) {
        val specForCurrTestRow = keysForLabels(testRow)
        val eventIDToInt = Integer.parseInt(specForCurrTestRow._2)
        val possibleLabels = labelMapFromOldDataset.filter(x => {x._1._1 == specForCurrTestRow._1 && x._1._3 == specForCurrTestRow._3})
        for((id,truthLab) <- possibleLabels) {
          val intId = Integer.parseInt(id._2)
          if(Math.abs(eventIDToInt - intId) <= quickerFixer) {
              println(s"Predicted value: ${pred(0)}")
              println(s"Actual value: ${truthLab}")
              truthLabelsForThisPaper += truthLab
              predictedLabelsForThisPaper += pred(0)
              giantPredictedLabels += pred(0)
              giantTruthLabels += truthLab
          }
        }
      }

    }


    val predictCountsMap = CodeUtils.predictCounts(truthLabelsForThisPaper.toArray, predictedLabelsForThisPaper.toArray)
    println(s"Current test case: ${paperID}")
    println(predictCountsMap)
    val precisionPerPaper = CodeUtils.precision(predictCountsMap)
    val recallPerPaper = CodeUtils.recall(predictCountsMap)
    recallScoreBoardPerPaper ++= Map(paperID -> recallPerPaper)
    precisionScoreBoardPerPaper ++= Map(paperID -> precisionPerPaper)
    //giantPredictedLabels ++= predictedLabelsForThisPaper
    //giantTruthLabels ++= truthLabelsForThisPaper


  }

  println(s"Size of predicted labels list: ${giantPredictedLabels.size}")
  println(s"Size of truth label list: ${giantTruthLabels.size}")

  val microAveragedCountsMap = CodeUtils.predictCounts(giantTruthLabels.toArray, giantPredictedLabels.toArray)
  val microAveragedPrecisionScore = CodeUtils.precision(microAveragedCountsMap)
  val microAveragedRecallScore = CodeUtils.recall(microAveragedCountsMap)
  val microAveragedF1Score = CodeUtils.f1(microAveragedCountsMap)
  var totalPrecision = 0.0
  for((paperID, perPaperPrecision) <- precisionScoreBoardPerPaper) {
    totalPrecision += perPaperPrecision
    println(s"The paper ${paperID} has the precision score ${perPaperPrecision}")
  }


  val arithmeticMeanPrecision = totalPrecision/(precisionScoreBoardPerPaper.size)


  println(s"The micro-averaged precision score is ${microAveragedPrecisionScore}")
  println(s"The micro-averaged recall score is ${microAveragedRecallScore}")
  //println(s"The micro-averaged f1 score is ${microAveragedF1Score}")
  println(s"Arithmetic mean precision is ${arithmeticMeanPrecision}")

}
