package org.clulab.reach.context.exec.research_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CrossValidationUtils}
import org.clulab.reach.context.feature_utils.ContextFeatureUtils
import org.clulab.reach.context.utils.annotation_alignment_utils.AnnotationAlignmentUtils
import org.clulab.reach.context.utils.io_utils.ReachSystemAnalysisIOUtils
import org.clulab.reach.context.utils.score_utils.ScoreMetricsOfClassifier

object SVMPerformanceOnNewReach extends App {
  val svmWrapper = new LinearSVMContextClassifier()
  val config = ConfigFactory.load()


  val configPath = config.getString("contextEngine.params.trainedSvmPath")
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }


  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")
  println(s"In svm performance class, running code")
  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val labelMap = ReachSystemAnalysisIOUtils.generateLabelMap(labelFile).toSeq
  val specsByRow = collection.mutable.HashMap[AggregatedContextInstance, (String,String,String)]()
  val pathToParentdirToLoadNewRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val parentDirfileInstanceToLoadNewRows = new File(pathToParentdirToLoadNewRows)
  val paperDirs = parentDirfileInstanceToLoadNewRows.listFiles().filter(x => x.isDirectory && x.getName.startsWith("PMC"))
  val paperIDByNewRows = collection.mutable.ListBuffer[(String, Seq[AggregatedContextInstance])]()
  val paperIDByNewRowsSpecs = collection.mutable.ListBuffer[(String, Seq[(String,String,String)])]()
  for (paperDir <- paperDirs) {

    val listOfRowsInPaper = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val listOfSpecsInPaper = collection.mutable.ListBuffer[(String,String,String)]()
    val paperID = paperDir.getName
    val rowFilesInThisPaper = paperDir.listFiles().filter(_.getName.startsWith("Aggreg"))
    for(rowFile <- rowFilesInThisPaper) {
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(rowFile)
      val row = ContextFeatureUtils.readAggRowFromFile(rowFile)
      if(!listOfRowsInPaper.contains(row)) {
        listOfRowsInPaper += row
        listOfSpecsInPaper += rowSpecs
        specsByRow ++= Map(row -> rowSpecs)
      }
    }

    val tuple2 = (paperID, listOfRowsInPaper)
    val tuple2Specs = (paperID,listOfSpecsInPaper)
    paperIDByNewRows += tuple2
    paperIDByNewRowsSpecs += tuple2Specs
  }


  val allAnnotationsFromOldReach = labelMap.map(_._1)

  val giantTruthLabelList = collection.mutable.ListBuffer[Int]()
  val giantPredictedLabelList = collection.mutable.ListBuffer[Int]()
  val matchingLabelsInNewReachByPaper = collection.mutable.HashMap[String,Seq[(String,String,String)]]()
  val matchingLabelsInOldReachByPaper = collection.mutable.HashMap[String,Seq[(String,String,String)]]()

  // testing if event-context pairs detected by new Reach align neatly with those from the old Reach.
  // If they do, we can use the annotation from the old one as "gold standard", and the new row can be predicted and tested for precision
  val listOfMatchesForOldFromNew = collection.mutable.ListBuffer[(String,String)]()
  for((paperID, testRows) <- paperIDByNewRows) {
    val testRowsWithMatchingLabels = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val matchingLabelsPerPaperNewReach = collection.mutable.ListBuffer[(String, String, String)]()
    val matchingLabelsPerPaperOldReach = collection.mutable.ListBuffer[(String, String, String)]()
    val predictedLabelsInThisPaper = collection.mutable.ListBuffer[Int]()
    val trueLabelsInThisPaper = collection.mutable.ListBuffer[Int]()
    val possibleLabelIDsInThisPaper = labelMap.filter(_._1._1 == paperID)
    for(tester <- testRows) {

      for((labelID,label) <- possibleLabelIDsInThisPaper) {
        val specForTester = specsByRow(tester)
        if(AnnotationAlignmentUtils.eventsAlign(specForTester._2,labelID._2) && AnnotationAlignmentUtils.contextsAlign(specForTester._3,labelID._3)) {
          println(s"The matching label from new Reach is: ${specForTester}")
          println(s"The matching label from old Reach is: ${labelID}")


          //if(!alreadyVisitedOldAnnotations.contains(labelID)) {
            testRowsWithMatchingLabels += tester
            trueLabelsInThisPaper += label
            val tupToAddForFreqCount = (labelID._2,paperID)
            listOfMatchesForOldFromNew += tupToAddForFreqCount



            matchingLabelsPerPaperNewReach += specForTester
            matchingLabelsPerPaperOldReach += labelID
          //}
        }
      }


      val matchingLabelsInThisPaperNewReach = Map(paperID -> matchingLabelsPerPaperNewReach)
      matchingLabelsInNewReachByPaper ++= matchingLabelsInThisPaperNewReach




      val matchingLabelsInThisPaperOldReach = Map(paperID -> matchingLabelsPerPaperOldReach)
      matchingLabelsInOldReachByPaper  ++= matchingLabelsInThisPaperOldReach


    }



    for(validTestRow <- testRowsWithMatchingLabels) {
      val predictionByTestRow = trainedSVMInstance.predict(Seq(validTestRow))(0)
      predictedLabelsInThisPaper += predictionByTestRow
    }

    giantTruthLabelList ++= trueLabelsInThisPaper
    giantPredictedLabelList ++= predictedLabelsInThisPaper
  }






  // Tasks 1 and 2: finding the events in new and old Reach that don't match up with each other
  println(s"The paper PMC2063868 is degenerate as per new Reach and will not appear in this analysis.")
  println(s"All events and contexts that appeared in the new Reach are 15 sentences away. We will restrict ourselves to this window even n the old dataset.")
  var totalEventsMissingFromNewDataset = 0
  var totalEventsMissingFromOldDataset = 0
  val eventsOnlyInNewReach = collection.mutable.HashMap[String, Seq[String]]()
  val eventsOnlyInOldReach = collection.mutable.HashMap[String, Seq[String]]()
  for((paperID,matchingLabelsNew) <-  matchingLabelsInNewReachByPaper) {
    // getting all the rows that were extracted by new Reach and extracting their event IDs
    val allRowSpecsInThisPaper = paperIDByNewRowsSpecs.filter(_._1 == paperID).map(_._2).flatten
    val allUniqueEventSpans = allRowSpecsInThisPaper.map(_._2).toSet
    val matchingUniqueEventSpans = matchingLabelsNew.map(_._2).toSet
    val nonMatches = allUniqueEventSpans -- matchingUniqueEventSpans
    val mapEntry = Map(paperID -> nonMatches.toSeq)
    eventsOnlyInNewReach ++= mapEntry
    totalEventsMissingFromOldDataset += nonMatches.size
  }



  println("*********")
  var totalContextCountForAllMissingEvents = 0
  var totalUniqueEventSpansOldData = 0
  val eventSpansOnlyInOldReachPaperAgnostic = collection.mutable.ListBuffer[String]()

  for((paperID, matchingLabelsOld) <- matchingLabelsInOldReachByPaper) {

    val allLabelsInPaper = allAnnotationsFromOldReach.filter(_._1 == paperID)

    val allEventsInPaper = allLabelsInPaper.map(_._2)

    totalUniqueEventSpansOldData += allEventsInPaper.toSet.size
    val matchingEventSpans = matchingLabelsOld.map(_._2)

    val nonMatches = collection.mutable.ListBuffer[String]()
    for(a <- allEventsInPaper) {
      if(!matchingEventSpans.contains(a) && !nonMatches.contains(a))
        nonMatches += a
    }

    eventSpansOnlyInOldReachPaperAgnostic ++= nonMatches

    val mapEntry = Map(paperID -> nonMatches)
    eventsOnlyInOldReach ++= mapEntry
    totalEventsMissingFromNewDataset += nonMatches.size
  }

  println(s"Total no. of unique event spans appearing only in old Reach (Reach 2015): ${totalEventsMissingFromNewDataset} ")
  println(s"Total no. of unique event spans appearing only in new Reach (Reach 2019): ${totalEventsMissingFromOldDataset} ")

//
//  uncomment the below block for task 3
//  // Task 3: In the old annotation dataset, we need to find the neighboring events that have different contexts associated with them
//    // To do task 3, the first step is to use only those manual annotations that have a true prediction. If an event is not truly associated with a context, we need not bother counting it in.
//  val trueLabelsFromOldDataset = labelMap.filter(_._2 == 1)
//
//  // in these true predictions, we should first group the annotations by paperID, to get all the true predictions in a given paper
//  val trueLabelsGroupedByPaperID = trueLabelsFromOldDataset.groupBy(_._1._1)
//
//
//  // Storing the information of neighboring events per paper per sentence:
//  // We will have a Map(paperID -> Map(sentenceIndex -> (neighborCount, List((leftNeighbor, rightNeighbor))  ) ) )
//  val neighborsPerSentencePerPaper = collection.mutable.HashMap[String, Map[Int,(Int,List[(String,String)])]]()
//
//
//  // then, for each paper, we need to cluster the events by the sentence in which they occur.
//  for((paperID,annotationsInThisPaper) <- trueLabelsGroupedByPaperID) {
//    // getting the eventIDs in this paper
//    val eventIDs = annotationsInThisPaper.map(_._1._2)
//    val eventIDInTupForm = eventIDs.map(EventAlignmentUtils.parseEventIDFromStringToTup(_))
//
//
//    // group the events by the sentence in which they occur
//    // this is because we need to find the events that are neighbors, and their sentence position is a necessary condition for this.
//    val eventsGroupedBySentIndex = eventIDInTupForm.groupBy(_._1)
//
//    // for all the events in a given sentence, ensure they are sorted in increasing order of start token
//    // This step is necessary because we need to find the events that are neighboring to each other.
//    // This condition of "neighborhood" is contingent upon the end token of the left event and the start token of the right event
//    // basically, two events are "neighbors" if they appear in the same sentence and there is no event that appears between them
//
//    val neighborsPerSent = collection.mutable.HashMap[Int,(Int,List[(String,String)])]()
//    for((sentenceIndex, eventsInThisSentence) <- eventsGroupedBySentIndex) {
//      val eventsSortedByStartToken = eventsInThisSentence.sortWith(_._2 <= _._2)
//      val neighboringEventsInThisSentence = collection.mutable.ListBuffer[(String,String)]()
//      // we start checking for neighbors with the left most event
//      for(leftEvent <- eventsSortedByStartToken) {
//
//          // we then take the next immediate event to be the right event.
//
//          val allExceptLeft = eventsSortedByStartToken.filter(_._2 > leftEvent._2)
//          if(allExceptLeft.size > 0) {
//            val rightEvent = allExceptLeft(0)
//            // we need all other events that are not the left event or the right event, to see if anything else appears between them.
//            if(EventAlignmentUtils.areEventsAdjacent(leftEvent,rightEvent)) {
//              val myNameIsLeftEvent = EventAlignmentUtils.parseEventIDFromTupToString(leftEvent)
//              val myNameIsRightEvent = EventAlignmentUtils.parseEventIDFromTupToString(rightEvent)
//              val neighbors = (myNameIsLeftEvent,myNameIsRightEvent)
//              neighboringEventsInThisSentence += neighbors
//            }
//          }
//
//
//
//      }
//
//      val neighborsEntry = (neighboringEventsInThisSentence.size,neighboringEventsInThisSentence.toList)
//      val sentenceIndexEntry = Map(sentenceIndex -> neighborsEntry)
//      neighborsPerSent ++= sentenceIndexEntry
//    }
//
//    val paperEntry = Map(paperID -> neighborsPerSent.toMap)
//
//    neighborsPerSentencePerPaper ++= paperEntry
//
//  }
  // Uncomment the above block for task 3


// If there are neighboring events, we can print them by uncommenting the block below
//  // printing some neighbors and no-neighbors for debugging and manual verification
//  for((paperID, neighborsBySent) <- neighborsPerSentencePerPaper) {
//    println(s"The current paper is: ${paperID}")
//    val fiveSentencesThatHaveSomeNeighbors = neighborsBySent.filter(_._2._1 > 0).take(5)
//    val fiveSentencesThatHaveNoNeighbors = neighborsBySent.filter(_._2._1 == 0).take(5)
//    println("Printing some sentences that do have neighbors in them")
//    println(fiveSentencesThatHaveSomeNeighbors)
//
//    println("Printing some sentences that have no neighbors in them")
//    println(fiveSentencesThatHaveNoNeighbors)
//
//    println(" **************************** ")
//
//  }


  println(s"After prediction, ${giantTruthLabelList.size} truth labels were found")
  println(s"After prediction, ${giantPredictedLabelList.size} predicted labels were found")

  // Task 4: Finding the accuracy of predictions vs ground truth, i.e. how much of the matching labels do we agree upon with the old dataset
  val accuracy = ScoreMetricsOfClassifier.accuracy(giantTruthLabelList.toArray, giantPredictedLabelList.toArray)
  println(s"The accuracy was found to be: ${accuracy}")


  println("**********************")


  // Task 5: Writing binary strings for sentences in each paper

  // step 1: load the sentences from each paper into a map of [paperID -> Seq[Sentences]]
  val dirPathForSentencesFileByPaper = config.getString("svmContext.outputDirForAnnotations")
  val mapOfSentencesByPaper = ReachSystemAnalysisIOUtils.loadSentencesPerPaper(dirPathForSentencesFileByPaper)


  // step 2: get the events that lie exclusively in new Reach or exclusively in old Reach, and sort them by sentenceIndex
  // and start token of the events.

  // if something goes wrong with the binary string, first check for the order in which sentences are appearing, maybe the missing event spans didn't get sorted correctly.
  val sortedEventsInNewReachByPaper = AnnotationAlignmentUtils.getSortedEventSpansPerPaper(eventsOnlyInNewReach.toMap)
  val sortedEventsInOldReachByPaper = AnnotationAlignmentUtils.getSortedEventSpansPerPaper(eventsOnlyInOldReach.toMap)

  val mapOfBinaryStringsByPaperOldReach = collection.mutable.HashMap[String, Seq[String]]()
  val mapOfBinaryStringsByPaperNewReach = collection.mutable.HashMap[String, Seq[String]]()
  for((paperID,sentences) <- mapOfSentencesByPaper) {
      val sentencesWithIndices = sentences.zipWithIndex

      val sentencesToWriteToNewReach = collection.mutable.ListBuffer[String]()
      val sentencesToWriteToOldReach = collection.mutable.ListBuffer[String]()
      val sortedEventSpansInPaperNewReach = sortedEventsInNewReachByPaper.filter(_._1 == paperID)(0)
      val sortedEventSpansInPaperOldReach = sortedEventsInOldReachByPaper.filter(_._1 == paperID)(0)
      for((sentence,sentenceIndex) <- sentencesWithIndices) {
        val sentenceToAddToNewReach = AnnotationAlignmentUtils.makeBinarySentenceFromWords(sentence, sentenceIndex, sortedEventSpansInPaperNewReach._2, sortedEventSpansInPaperNewReach._1, "new")
        val sentenceToAddToOldReach = AnnotationAlignmentUtils.makeBinarySentenceFromWords(sentence, sentenceIndex, sortedEventSpansInPaperOldReach._2, sortedEventSpansInPaperOldReach._1, "old")
        sentencesToWriteToNewReach += sentenceToAddToNewReach
        sentencesToWriteToOldReach += sentenceToAddToOldReach
      }
    val mapEntryOldReach = Map(paperID -> sentencesToWriteToOldReach)
    mapOfBinaryStringsByPaperOldReach ++= mapEntryOldReach

    val mapEntryNewReach = Map(paperID -> sentencesToWriteToNewReach)
    mapOfBinaryStringsByPaperNewReach ++= mapEntryNewReach
  }


  ReachSystemAnalysisIOUtils.writeBinaryStringsOfEventSpansByPaper(dirPathForSentencesFileByPaper, mapOfBinaryStringsByPaperOldReach.toMap, "old")
  ReachSystemAnalysisIOUtils.writeBinaryStringsOfEventSpansByPaper(dirPathForSentencesFileByPaper, mapOfBinaryStringsByPaperNewReach.toMap, "new")



  val eventSpansInMatchingLabelsInOldData = collection.mutable.HashMap[String,(Int,Seq[(Int,Int,Int)])]()
  var totalUniqueEventSpansInOldMatchings = 0
  for((paperID, matchingLabels) <- matchingLabelsInOldReachByPaper) {
    val eventSpans = matchingLabels.map(x=>x._2)
    val eventSpansInTupForm = eventSpans.map(AnnotationAlignmentUtils.parseEventIDFromStringToTup(_)).toSet.toSeq
    val tupEntry = (eventSpansInTupForm.size, eventSpansInTupForm)
    totalUniqueEventSpansInOldMatchings += eventSpansInTupForm.size
    val mapEntry = Map(paperID -> tupEntry)
    eventSpansInMatchingLabelsInOldData ++= mapEntry
  }


  println(s"A total of ${totalUniqueEventSpansInOldMatchings} unique event spans were found in the 7k set of matching context-event labels in old Reach")
  println(s"A total of ${totalUniqueEventSpansOldData} unique event spans were found in the whole annotation set, matchings and non-matchings included in old reach")


  //paperIDByOldRowsSpecs
//  val onlyMatchingsFromOldReach = matchingLabelsInOldReachByPaper.map(x => x._2).flatten
//  val missingAnnotations = allAnnotationsFromOldReach -- onlyMatchingsFromOldReach
//  println(s"Size of missing annotations: ${missingAnnotations.size}")

  val onlyMatchingsFromOldReach = matchingLabelsInOldReachByPaper.map(x => x._2).flatten.toSeq
  val missingAnnotations = collection.mutable.ListBuffer[(String,String,String)]()
  for(a <- allAnnotationsFromOldReach)
    if(!onlyMatchingsFromOldReach.contains(a))
      missingAnnotations += a
  println(s"Number of missing annotations: ${missingAnnotations.size}")
  var totalNoOfAnnotations = 0
  for((_,_) <- labelMap) totalNoOfAnnotations += 1




  val freqMapOfEventSpanInPaper = AnnotationAlignmentUtils.countFrequency(listOfMatchesForOldFromNew)
  val maxFreq = freqMapOfEventSpanInPaper.map(_._2).max
  val whichAnnotationMax = freqMapOfEventSpanInPaper.filter(_._2 == maxFreq)
  println(s"The max frequency was found to be: ${maxFreq}")
  println(s"In the old Reach, The event span with the maximum number of matches is: ${whichAnnotationMax}")


  println(s"The total number of annotations we have is: ${totalNoOfAnnotations}")



}
