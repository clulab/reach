package org.clulab.reach.context.context_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.learning.LinearSVMClassifier
import org.clulab.reach.context.context_utils.ContextFeatureUtils

object CrossValBySentDist extends App {

  val config = ConfigFactory.load()
  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val unTrainedSVMInstance = new LinearSVMContextClassifier(Some(SVMClassifier))
  val labelFile = config.getString("svmContext.labelFile")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper).concat("/sentenceWindows")
  val allSentDirs = new File(dirForType).listFiles().filter(_.isDirectory)

  val allRowsBySentDist = collection.mutable.HashMap[Int, Seq[AggregatedContextInstance]]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val filterForFasterRun = List("8","9","10","11")
  val smallNumOfDirs = allSentDirs.filter(x => filterForFasterRun.contains(x.getName))
  for(d<- smallNumOfDirs) {
    val rowFiles = d.listFiles().filter(_.getName.contains("Aggregated"))
    val rowsForCurrentSent = collection.mutable.ListBuffer[AggregatedContextInstance]()
    for(r<-rowFiles) {
      val pathToRow = dirForType.concat(s"/${d.getName}").concat(s"/${r.getName}")
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(r)
      val row = ContextFeatureUtils.readAggRowFromFile(pathToRow)
      keysForLabels ++= Map(row -> rowSpecs)
      rowsForCurrentSent += row
    }
    val intName = Integer.parseInt(d.getName)
    val entry = Map(intName -> rowsForCurrentSent)
    allRowsBySentDist ++= entry
  }


  val foldsBySentDist = collection.mutable.HashMap[Int, Seq[(Seq[AggregatedContextInstance], Seq[AggregatedContextInstance])]]()

  for((sentDist,rows) <- allRowsBySentDist) {
    println(s"Sentence distance ${sentDist} has a total of ${rows.size} aggregated rows")
    val foldsPerSentDist = collection.mutable.ArrayBuffer[(Seq[AggregatedContextInstance], Seq[AggregatedContextInstance])]()
    for(r<-rows) {
      val trainingRows = rows.filter(_.PMCID != r.PMCID)
      val testingRows = rows.filter(_.PMCID == r.PMCID)
      val tup = (testingRows, trainingRows)
      foldsPerSentDist += tup
    }
    foldsBySentDist ++= Map(sentDist -> foldsPerSentDist)
  }


  val scorePerSentDist = collection.mutable.HashMap[Int,(Double, Double)]()
  for((sentDist, folds) <- foldsBySentDist) {
    val giantTruthArrayPerSentDist = collection.mutable.ListBuffer[Int]()
    val giantPredictedArrayPerSentDist = collection.mutable.ListBuffer[Int]()
    val precisionPerFold = collection.mutable.ListBuffer[Double]()
    for((test, train) <- folds) {
      val trainingIdsToIntersect = train.map(t => {
        keysForLabels(t)
      })

      val intersectTrainLabels = trainingIdsToIntersect.toSet.intersect(CodeUtils.generateLabelMap(labelFile).keySet)
      val trainingRows = collection.mutable.ListBuffer[AggregatedContextInstance]()
      val trainingLabels = collection.mutable.ListBuffer[Int]()
      for(tup<-intersectTrainLabels) {
        for(key <- keysForLabels.keySet) {
          if(keysForLabels(key) == tup)
            {
              trainingRows += key
              val label = CodeUtils.generateLabelMap(labelFile)(tup)
              trainingLabels += label
            }
        }
      }

      val (trainingRVFDataset, _) = unTrainedSVMInstance.dataConverter(trainingRows,Some(trainingLabels.toArray))

      unTrainedSVMInstance.fit(trainingRVFDataset)


      val testingLabelsIDs = test.map(t => {
        keysForLabels(t)
      })


      val intersectingTestingLabels = testingLabelsIDs.toSet.intersect(CodeUtils.generateLabelMap(labelFile).keySet)
      val testingRows = collection.mutable.ListBuffer[AggregatedContextInstance]()
      val testingLabels = collection.mutable.ListBuffer[Int]()
      for(tup<-intersectingTestingLabels) {
        for(key <- keysForLabels.keySet) {
          if(keysForLabels(key) == tup)
          {
            testingRows += key
            val label = CodeUtils.generateLabelMap(labelFile)(tup)
            testingLabels += label
          }
        }
      }
      val predictedLabels = unTrainedSVMInstance.predict(testingRows)

      val metricPerFold = findMetrics(testingLabels.toArray, predictedLabels.toArray)
      precisionPerFold += metricPerFold._1._1


      giantTruthArrayPerSentDist ++= testingLabels
      giantPredictedArrayPerSentDist ++= predictedLabels
    }

    val (metrics, _) = findMetrics(giantTruthArrayPerSentDist.toArray, giantPredictedArrayPerSentDist.toArray)
    val microPrecisionPerSentDist = metrics._1.toString.take(8).toDouble
    val meanPrecisionPerSentDist = CodeUtils.findAggrMetrics(precisionPerFold)
    val tup = (microPrecisionPerSentDist, meanPrecisionPerSentDist._3)
    scorePerSentDist ++= Map(sentDist -> tup)
  }


  def findMetrics(truth:Array[Int], test:Array[Int]):((Double,Double,Double),Map[String,Int]) = {
    val countsTest = CodeUtils.predictCounts(truth, test)
    val precision = CodeUtils.precision(countsTest)
    val recall = CodeUtils.recall(countsTest)
    val accuracy = CodeUtils.accuracy(countsTest)
    ((precision,recall,accuracy), countsTest)
  }


  for((sentDist, score) <- scorePerSentDist) {
    println(s"The sent dist ${sentDist} has the score ${score}")
  }

}
