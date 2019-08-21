package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
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

  // The difference between the old papers parsed in the old reach vs the new Reach involves an extra token in the event. i.e. for a given paper and context label, the old Reach captured the event 0045, but the new Reach may capture it as 0046.
  // This is the same event, it only includes one extra token. But if we do an exact string match the way we did in the CrossValBySentDist code, we will get an empty set.
  // To combat this, this code will parse the events ID as an integer, and only consider those events as the same if they math.abs(a-b) <= 1. That means, if the integer values differ by 1, they are just one token away.

  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val labelMapFromOldDataset = CodeUtils.generateLabelMap(labelFile)
  val parentDirForRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val allPapersDirs = new File(parentDirForRows).listFiles().filter(_.isDirectory)
  val smallSetOfPapers = List("PMC2156142", "PMC2195994", "PMC2743561")
  val dirsToUseForDebug = allPapersDirs.filter(x => smallSetOfPapers.contains(x.getName))
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val allRowsByPaperID = collection.mutable.HashMap[String, Seq[AggregatedContextInstance]]()
  for(paperDir <- dirsToUseForDebug) {
    // In this code we won't face the double counting of two rows from a given paper, because each paper appears only once over all.
    // While analyzing the performance over sentence windows, we encountered the same paper over different values of sentence window. That's why we had the risk of adding the same row twice.
    // But we now see that each paper appears only once, and we read the rows from that paper. So we won't add the same row twice.
    // The only time we will see the same paper appear twice will be in the hold-one-out cross-validation phase, which is expected behavior.
    val rowFiles = paperDir.listFiles().filter(_.getName.contains("Aggregated"))
    println(s"The current paper ${paperDir.getName} has ${rowFiles.size} rows")
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
    val entry = Map(nameOfCurrentDirectory -> rowsForCurrentSent.toSet.toSeq)
    allRowsByPaperID ++= entry
  }

  println(s"We have a total of ${allRowsByPaperID.size} papers, and the micro-averaged precision will be calculated by treating each paper as a test case in the cross validation loop")
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
    val trainingRowsWithCorrectLabels = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val trainingLabels = collection.mutable.ListBuffer[Int]()
    for(t <- trainRowsNeedsProcessing) {
      val specForCurrentRow = keysForLabels(t)
      val evtIDInt = Integer.parseInt(specForCurrentRow._2)
      // getting the possible events that have the same paper ID and context ID
      val possibleMatchesInLabelFile = labelMapFromOldDataset.filter(x => {x._1._1 == specForCurrentRow._1 && x._1._3 == specForCurrentRow._3})
      var numOfValidEventsDetectedperRow = 0
      for((id,lab) <- possibleMatchesInLabelFile) {
        val intId = Integer.parseInt(id._2)
        if(Math.abs(intId - evtIDInt) <= quickerFixer && !trainingRowsWithCorrectLabels.contains(t)) {
          println(s"Current training row has the specs ${specForCurrentRow}")
          println(s"Current row spec of possible match from label file: ${id}")
          trainingRowsWithCorrectLabels += t
          trainingLabels += lab
          numOfValidEventsDetectedperRow += 1
        }
      }
      //println(numOfValidEventsDetectedperRow + " := number of events that matched for the current row")
    }
    println(trainingRowsWithCorrectLabels.size)
    println(trainingRowsWithCorrectLabels.toSet.size)
    println(trainingLabels.size)

    val (trainingRVFDataset, _) = unTrainedSVMInstance.dataConverter(trainingRowsWithCorrectLabels,Some(trainingLabels.toArray))

    unTrainedSVMInstance.fit(trainingRVFDataset)


    for(testRow <- testRowsPerPaper) {
      val pred = unTrainedSVMInstance.predict(Seq(testRow))


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
    println(predictCountsMap)
    val precisionPerPaper = CodeUtils.precision(predictCountsMap)
    val recallPerPaper = CodeUtils.recall(predictCountsMap)
    recallScoreBoardPerPaper ++= Map(paperID -> recallPerPaper)
    precisionScoreBoardPerPaper ++= Map(paperID -> precisionPerPaper)
    //giantPredictedLabels ++= predictedLabelsForThisPaper
    //giantTruthLabels ++= truthLabelsForThisPaper


  }

  println(giantPredictedLabels.size)
  println(giantTruthLabels.size)

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
