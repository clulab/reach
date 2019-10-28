package org.clulab.reach.context.exec.research_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CrossValidationUtils}
import org.clulab.reach.context.feature_utils.ContextFeatureUtils
import org.clulab.reach.context.utils.annotation_alignment_utils.AnnotationAlignmentUtils
import org.clulab.reach.context.utils.io_utils.{ClassifierLoader, ReachPredictionDataTypeIOUtils, ReachSystemAnalysisIOUtils}
import org.clulab.reach.context.utils.score_utils.ScoreMetricsOfClassifier

object SVMPerformanceOnNewReach extends App {
  val svmWrapper = new LinearSVMContextClassifier()
  val config = ConfigFactory.load()


  val configPath = config.getString("contextEngine.params.trainedSvmPath")

  val trainedSVMInstance = ClassifierLoader.getClassifierInstanceFromPath(configPath)


  val labelFile = config.getString("svmContext.labelFileOldDataset")
  val labelMap= ReachSystemAnalysisIOUtils.generateLabelMap(labelFile)
  val pathToParentdirToLoadNewRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val (paperIDByNewDataPoints,paperIDByNewDataPointSpecs, idForNewReachAggrRow) = ReachPredictionDataTypeIOUtils.readAggrRowsWithSpecsFromFile(pathToParentdirToLoadNewRows)




  val allAnnotationsFromOldReach = labelMap.map(_._1).toSeq
  val oldAnnotationsGroupedByPaperID = allAnnotationsFromOldReach.groupBy(_._1)

  val annotationsGroupedByPaperID = labelMap.groupBy(x => x._1._1)
  val giantTruthLabelList = collection.mutable.ListBuffer[Int]()
  val giantPredictedLabelList = collection.mutable.ListBuffer[Int]()
  val matchingLabelsInNewReachByPaper = collection.mutable.HashMap[String,Seq[(String,String,String)]]()
  val matchingLabelsInOldReachByPaper = collection.mutable.HashMap[String,Seq[(String,String,String)]]()

  // testing if event-context pairs detected by new Reach align neatly with those from the old Reach.
  // If they do, we can use the annotation from the old one as "gold standard", and the new row can be predicted and tested for precision

  for((paperID, testRows) <- paperIDByNewDataPoints) {
    val testRowsWithMatchingLabels = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val annotationsAlreadyVisited = collection.mutable.ListBuffer[(String,String,String)]()
    val matchingLabelsPerPaperNewReach = collection.mutable.ListBuffer[(String, String, String)]()
    val matchingLabelsPerPaperOldReach = collection.mutable.ListBuffer[(String, String, String)]()
    val predictedLabelsInThisPaper = collection.mutable.ListBuffer[Int]()
    val trueLabelsInThisPaper = collection.mutable.ListBuffer[Int]()
    val possibleLabelIDsInThisPaper = annotationsGroupedByPaperID(paperID)
    for(tester <- testRows) {
      val specForTester = idForNewReachAggrRow(tester)
      for((labelID,label) <- possibleLabelIDsInThisPaper) {

        if(AnnotationAlignmentUtils.eventsAlign(specForTester._2,labelID._2) && AnnotationAlignmentUtils.contextsAlign(specForTester._3,labelID._3)) {

          if(!annotationsAlreadyVisited.contains(labelID)) {
            testRowsWithMatchingLabels += tester
            trueLabelsInThisPaper += label
            matchingLabelsPerPaperNewReach += specForTester
            matchingLabelsPerPaperOldReach += labelID
            annotationsAlreadyVisited += labelID
          }
        }
      }

    }



    for(validTestRow <- testRowsWithMatchingLabels) {
      val predictionByTestRow = trainedSVMInstance.predict(Seq(validTestRow))(0)
      predictedLabelsInThisPaper += predictionByTestRow
    }


    val matchingLabelsInThisPaperNewReach = Map(paperID -> matchingLabelsPerPaperNewReach)
    matchingLabelsInNewReachByPaper ++= matchingLabelsInThisPaperNewReach




    val matchingLabelsInThisPaperOldReach = Map(paperID -> matchingLabelsPerPaperOldReach)
    matchingLabelsInOldReachByPaper  ++= matchingLabelsInThisPaperOldReach

    giantTruthLabelList ++= trueLabelsInThisPaper
    giantPredictedLabelList ++= predictedLabelsInThisPaper
  }






  // finding the events in new and old Reach that don't match up with each other
  println(s"All events and contexts that appeared in the new Reach are 15 sentences away. We will restrict ourselves to this window even n the old dataset.")

  val (eventsOnlyInNewReach,totalEventsMissingFromOldDataset) = AnnotationAlignmentUtils.getUniqueEventSpansInReach(matchingLabelsInNewReachByPaper.toMap, paperIDByNewDataPointSpecs)



  println("*********")
  var totalUniqueEventSpansOldData = 0

  val (eventsOnlyInOldReach, totalEventsMissingFromNewDataset) = AnnotationAlignmentUtils.getUniqueEventSpansInReach(matchingLabelsInOldReachByPaper.toMap, oldAnnotationsGroupedByPaperID)

  println(s"Total no. of unique event spans appearing only in old Reach (Reach 2015): ${totalEventsMissingFromNewDataset} ")
  println(s"Total no. of unique event spans appearing only in new Reach (Reach 2019): ${totalEventsMissingFromOldDataset} ")

//

// In the old annotation dataset, we need to find the neighboring events that have different contexts associated with them
// the first step is to use only those manual annotations that have a true prediction. If an event is not truly associated with a context, we need not bother counting it in.
  val trueLabelsFromOldDataset = labelMap.filter(_._2 == 1)

  // in these true predictions, we should first group the annotations by paperID, to get all the true predictions in a given paper
  val trueLabelsGroupedByPaperID = trueLabelsFromOldDataset.groupBy(_._1._1)


  // Storing the information of neighboring events per paper per sentence:
  // We will have a Map(paperID -> Map(sentenceIndex -> (neighborCount, List((leftNeighbor, rightNeighbor))  ) ) )
  val neighborsPerSentencePerPaper = collection.mutable.HashMap[String, Map[Int,(Int,List[(String,String)])]]()


  // then, for each paper, we need to cluster the events by the sentence in which they occur.
  for((paperID,annotationsInThisPaper) <- trueLabelsGroupedByPaperID) {
    // getting the eventIDs in this paper
    val eventIDs = annotationsInThisPaper.map(_._1._2)
    val eventIDInTupForm = eventIDs.map(AnnotationAlignmentUtils.parseEventIDFromStringToTup(_)).toSeq


    // group the events by the sentence in which they occur
    // this is because we need to find the events that are neighbors, and their sentence position is a necessary condition for this.
    val eventsGroupedBySentIndex = eventIDInTupForm.groupBy(_._1)

    // for all the events in a given sentence, ensure they are sorted in increasing order of start token
    // This step is necessary because we need to find the events that are neighboring to each other.
    // This condition of "neighborhood" is contingent upon the end token of the left event and the start token of the right event
    // basically, two events are "neighbors" if they appear in the same sentence and there is no event that appears between them

    val neighborsPerSent = collection.mutable.HashMap[Int,(Int,List[(String,String)])]()
    for((sentenceIndex, eventsInThisSentence) <- eventsGroupedBySentIndex) {
      val eventsSortedByStartToken = eventsInThisSentence.sortWith(_._2 <= _._2)
      val neighboringEventsInThisSentence = collection.mutable.ListBuffer[(String,String)]()
      // we start checking for neighbors with the left most event
      for(leftEvent <- eventsSortedByStartToken) {

          // we then take the next immediate event to be the right event.

          val allExceptLeft = eventsSortedByStartToken.filter(_._2 > leftEvent._2)
          if(allExceptLeft.size > 0) {
            val rightEvent = allExceptLeft(0)
            // we need all other events that are not the left event or the right event, to see if anything else appears between them.
            if(AnnotationAlignmentUtils.areEventsAdjacent(leftEvent,rightEvent)) {
              val myNameIsLeftEvent = AnnotationAlignmentUtils.parseEventIDFromTupToString(leftEvent)
              val myNameIsRightEvent = AnnotationAlignmentUtils.parseEventIDFromTupToString(rightEvent)
              val neighbors = (myNameIsLeftEvent,myNameIsRightEvent)
              neighboringEventsInThisSentence += neighbors
            }
          }
      }

      val neighborsEntry = (neighboringEventsInThisSentence.size,neighboringEventsInThisSentence.toList)
      val sentenceIndexEntry = Map(sentenceIndex -> neighborsEntry)
      neighborsPerSent ++= sentenceIndexEntry
    }

    val paperEntry = Map(paperID -> neighborsPerSent.toMap)

    neighborsPerSentencePerPaper ++= paperEntry

  }





  println(s"After prediction, ${giantTruthLabelList.size} truth labels were found")
  println(s"After prediction, ${giantPredictedLabelList.size} predicted labels were found")

  // Finding the accuracy of predictions vs ground truth, i.e. how much of the matching labels do we agree upon with the old dataset
  val accuracy = ScoreMetricsOfClassifier.accuracy(giantTruthLabelList.toArray, giantPredictedLabelList.toArray)
  println(s"The accuracy was found to be: ${accuracy}")


  println("**********************")


  // Writing binary strings for sentences in each paper

  // step 1: load the sentences from each paper into a map of [paperID -> Seq[Sentences]]
  // This config path has the path to the sentences.txt file as generated by the GenerateOutputfile script.
  val dirPathForSentencesFileByPaper = config.getString("svmContext.outputDirForAnnotations")
  val mapOfSentencesByPaper = ReachSystemAnalysisIOUtils.loadSentencesPerPaper(dirPathForSentencesFileByPaper)


  // step 2: get the events that lie exclusively in new Reach or exclusively in old Reach, and sort them by sentenceIndex
  // and start token of the events.

  // if something goes wrong with the binary string, first check for the order in which sentences are appearing, maybe the missing event spans didn't get sorted correctly.
  val sortedEventsInNewReachByPaper = AnnotationAlignmentUtils.getSortedEventSpansPerPaper(eventsOnlyInNewReach)
  val sortedEventsInOldReachByPaper = AnnotationAlignmentUtils.getSortedEventSpansPerPaper(eventsOnlyInOldReach)

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




}
