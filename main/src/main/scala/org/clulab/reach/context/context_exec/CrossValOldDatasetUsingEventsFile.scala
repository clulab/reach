package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory
import java.io.File

import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.context_utils.ContextFeatureUtils

import scala.io.Source

object CrossValOldDatasetUsingEventsFile extends App {
  val config = ConfigFactory.load()
  val parentDirForRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val annotationsFileDir = config.getString("polarityContext.eventsFilesDir")
  val labelsFromEventFiles = makeLabelMapFromEventFileDir(annotationsFileDir)
  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val labelMapFromOldDataset = CodeUtils.generateLabelMap(labelFile)
  val setOfEntriesWithAnnotations = labelsFromEventFiles.toSet
  //val setOfEntriesWithAnnotations = labelsFromEventFiles.toSet.union(labelMapFromOldDataset.toSet)

  //  val smallSetOfPapers = List("PMC2156142", "PMC2195994")
//  val dirsToUseForDebug = allPapersDirs.filter(x => smallSetOfPapers.contains(x.getName))
  val allPapersDirs = new File(parentDirForRows).listFiles().filter(x => x.isDirectory && x.getName != "newAnnotations")
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val allRowsByPaperID = collection.mutable.ListBuffer[(String, Seq[AggregatedContextInstance])]()
  println(allPapersDirs.size)
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

    allRowsByPaperID += entry
  }


  println(allRowsByPaperID.size)

  val precisionScoreBoardPerPaper = collection.mutable.HashMap[String, Double]()
  val recallScoreBoardPerPaper = collection.mutable.HashMap[String, Double]()
  val f1ScoreBoardPerPaper = collection.mutable.HashMap[String, Double]()
  val giantPredictedLabels = collection.mutable.ListBuffer[Int]()
  val giantTruthLabels = collection.mutable.ListBuffer[Int]()
  val quickerFixer = 1

  for((paperID, testRowsPerPaper) <- allRowsByPaperID) {
    val svmDeclaration = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
    val svmInstance = new LinearSVMContextClassifier(Some(svmDeclaration))
    val truthLabelsForThisPaper = collection.mutable.ListBuffer[Int]()
    val predictedLabelsForThisPaper = collection.mutable.ListBuffer[Int]()
    val trainingCaseRowsUnFiltered = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val notCurrentPaper = allRowsByPaperID.filter(_._1!=paperID)
    for((notThispaperId,rowsNotInThisPaper) <- notCurrentPaper) {
      if(notThispaperId != paperID) {
        for (r <- rowsNotInThisPaper) {
          if(!trainingCaseRowsUnFiltered.contains(r))
          {
            trainingCaseRowsUnFiltered += r
          }
        }
      }

    }


    println(s"When ${paperID} is the test case,")
    println(s"Without checking for matching annotations, we have a total of ${trainingCaseRowsUnFiltered.size} rows for training")
    val trainingRowsWithCorrectLabels = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val trainingLabels = collection.mutable.ListBuffer[Int]()
    for(t <- trainingCaseRowsUnFiltered) {
      val specForCurrentRow = keysForLabels(t)
      val evtID1 = specForCurrentRow._2
//      val possibleMatchesInLabelFile = setOfEntriesWithAnnotations.filter(x => {
//        val int1 = x._1._2
//        val sep = Math.abs(evtID - int1)
//        x._1._1 == specForCurrentRow._1 && x._1._3 == specForCurrentRow._3 && sep <= quickerFixer})
      val possibleMatches = setOfEntriesWithAnnotations.filter(x => {

          val evtID2 = x._2
          val eventsMatch = checksIfEventsAreSame(evtID1,evtID2)

          specForCurrentRow._1 == x._1 && eventsMatch && specForCurrentRow._3 == x._3
        })

      val possibleMatchesInLabelFile = possibleMatches.map({x =>
        val reformattedEvtID = collapseEvtId(x._2)._1
        val searchSpecForLabel = (x._1,reformattedEvtID,x._3)
        val labelFromDataframe = labelMapFromOldDataset(searchSpecForLabel)
        (x,labelFromDataframe)
      })

      for((_,lab) <- possibleMatchesInLabelFile) {
        if(!trainingRowsWithCorrectLabels.contains(t)) {
          trainingRowsWithCorrectLabels += t
          trainingLabels += lab

        }
      }
    }
    println(s"Current test case: ${paperID}")
    println(s"Size of training rows after filtering by appropriate event IDs: ${trainingRowsWithCorrectLabels.size}")
    println(s"Size of training labels after filtering by appropriate event IDs: ${trainingLabels.size}")
    val (trainingRVFDataset, _) = svmInstance.dataConverter(trainingRowsWithCorrectLabels,Some(trainingLabels.toArray))

    svmInstance.fit(trainingRVFDataset)


    for(testRow <- testRowsPerPaper) {
      val pred = svmInstance.predict(Seq(testRow))



      val specForCurrTestRow = keysForLabels(testRow)
      val eventID1 = specForCurrTestRow._2
//      val possibleLabels = labelMapFromOldDataset.filter(x => {
//        val int1 = Integer.parseInt(x._1._2)
//        val sep = Math.abs(int1 - eventID)
//        x._1._1 == specForCurrTestRow._1 && x._1._3 == specForCurrTestRow._3 && sep <= quickerFixer})
      val possibleMatches = setOfEntriesWithAnnotations.filter(x => {

        val evtID2 = x._2
        val eventsMatch = checksIfEventsAreSame(eventID1,evtID2)

        specForCurrTestRow._1 == x._1 && eventsMatch && specForCurrTestRow._3 == x._3
      })

      val possibleMatchesInLabelFileForTest = possibleMatches.map({x =>
        val reformattedEvtID = collapseEvtId(x._2)._1
        val searchSpecForLabel = (x._1,reformattedEvtID,x._3)
        val labelFromDataframe = labelMapFromOldDataset(searchSpecForLabel)
        (x,labelFromDataframe)
      })

      for((_,truthLab) <- possibleMatchesInLabelFileForTest) {

        truthLabelsForThisPaper += truthLab
        predictedLabelsForThisPaper += pred(0)
        //              giantPredictedLabels += pred(0)
        //              giantTruthLabels += truthLab

      }
    }


    val predictCountsMap = CodeUtils.predictCounts(truthLabelsForThisPaper.toArray, predictedLabelsForThisPaper.toArray)
    println(s"Current test case: ${paperID}")
    println(predictCountsMap)
    val precisionPerPaper = CodeUtils.precision(predictCountsMap)
    val recallPerPaper = CodeUtils.recall(predictCountsMap)
    val f1PerPaper = CodeUtils.f1(predictCountsMap)
    recallScoreBoardPerPaper ++= Map(paperID -> recallPerPaper)
    precisionScoreBoardPerPaper ++= Map(paperID -> precisionPerPaper)
    f1ScoreBoardPerPaper ++= Map(paperID -> f1PerPaper)
    giantPredictedLabels ++= predictedLabelsForThisPaper
    giantTruthLabels ++= truthLabelsForThisPaper

  }


  println(s"Size of predicted labels list: ${giantPredictedLabels.size}")
  println(s"Size of truth label list: ${giantTruthLabels.size}")

  val microAveragedCountsMap = CodeUtils.predictCounts(giantTruthLabels.toArray, giantPredictedLabels.toArray)
  val microAveragedPrecisionScore = CodeUtils.precision(microAveragedCountsMap)
  val microAveragedRecallScore = CodeUtils.recall(microAveragedCountsMap)
  val microAveragedF1Score = CodeUtils.f1(microAveragedCountsMap)


  val listOfAllPrecisions = precisionScoreBoardPerPaper.map{case (_,v) => v}
  val listOfAllRecalls = precisionScoreBoardPerPaper.map{case (_,v) => v}
  val listOfAllF1 = precisionScoreBoardPerPaper.map{case (_,v) => v}
  val arithmeticMeanPrecision = CodeUtils.arithmeticMeanScore(listOfAllPrecisions.toSeq)
  val arithmeticMeanRecall = CodeUtils.arithmeticMeanScore(listOfAllRecalls.toSeq)
  val arithmeticMeanF1 = CodeUtils.arithmeticMeanScore(listOfAllF1.toSeq)


  println(s"We have a total of ${allRowsByPaperID.size} papers")
  println(s"The micro-averaged precision score is ${microAveragedPrecisionScore}")
  println(s"The micro-averaged recall score is ${microAveragedRecallScore}")
  println(s"The micro-averaged f1 score is ${microAveragedF1Score}")
  println(s"Arithmetic mean precision is ${arithmeticMeanPrecision}")
  println(s"Arithmetic mean recall is ${arithmeticMeanRecall}")
  println(s"Arithmetic mean f1 is ${arithmeticMeanF1}")



  def makeLabelMapFromEventFileDir(pathToDir:String):Array[(String,String,String)] = {
    val labelMapFromEventFile = collection.mutable.ListBuffer[(String,String,String)]()
    val dirToRead = new File(pathToDir)
    val listOfFiles = dirToRead.listFiles()
    for(file <- listOfFiles) {
      val pmcid = file.getName.split("_")(0)
      val source = Source.fromFile(file)
      val lines = source.getLines()
      for(c <- lines) {
        val array = c.split("\t")
        if(array.size > 2) {
          val sentenceIndex = array(0)
          val tokenIntervalStart = array(1).split("-")(0)
          val tokenIntervalEnd = array(1).split("-")(1)
          val eventID = sentenceIndex+"*"+tokenIntervalStart+"="+tokenIntervalEnd
          //val eventID = sentenceIndex.concat(tokenIntervalStart.concat(tokenIntervalEnd))
          val contextIDs = array(2).split(",")
          for(cnt <- contextIDs) {
            val tupleEntry = (pmcid,eventID,cnt)
            labelMapFromEventFile += tupleEntry
          }
        }
      }
    }

    labelMapFromEventFile.toArray
  }

  def checksIfEventsAreSame(eventID1: String, eventID2:String):Boolean = {
//    val event1SentInd = Integer.parseInt(eventID1.split("*")(0))
//    val event1StartToken = Integer.parseInt(eventID1.split("*")(1).split("=")(0))
//    val event1EndToken = Integer.parseInt(eventID1.split("*")(1).split("=")(1))
//    val event2SentInd = Integer.parseInt(eventID2.split("*")(0))
//    val event2StartToken = Integer.parseInt(eventID2.split("*")(1).split("=")(0))
//    val event2EndToken = Integer.parseInt(eventID2.split("*")(1).split("=")(1))

    val (_,event1SentInd,event1StartToken,event1EndToken) = collapseEvtId(eventID1)
    val (_,event2SentInd,event2StartToken,event2EndToken) = collapseEvtId(eventID2)
    val areSentenceIndicesSame = event1SentInd == event2SentInd

    val eventContainsAnother = (event1StartToken < event2StartToken && event1EndToken < event2EndToken) || (event1StartToken > event2StartToken && event1EndToken > event2EndToken)
    val eventsMatchExactly = (event1StartToken == event2StartToken && event1EndToken == event2EndToken)
    val eventsOverlapApprox = (event1StartToken <= Math.abs(event2StartToken - event2EndToken)) || (event1EndToken <= Math.abs(event2StartToken - event2EndToken)) || (event2StartToken <= Math.abs(event1StartToken - event1EndToken)) || (event2EndToken <= Math.abs(event1StartToken - event1EndToken))
    val doEventsOverlapCorrectly = eventContainsAnother || eventsMatchExactly || eventsOverlapApprox
    areSentenceIndicesSame && doEventsOverlapCorrectly
  }


  def collapseEvtId(bigEvtID:String):(String,Int,Int,Int) = {
    val event1SentInd = Integer.parseInt((bigEvtID.split("*"))(0))
    val part1 = bigEvtID.split("*")(1)
    val part2 = part1.split("=")
    val event1StartToken = Integer.parseInt(part2(0))
    val event1EndToken = Integer.parseInt(part2(1))
    val strForm = event1SentInd+""+event1StartToken+""+event1EndToken
    (strForm,event1SentInd,event1StartToken,event1EndToken)
  }


}
