package org.clulab.reach.context.context_exec

import java.io.File

import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import com.typesafe.config.ConfigFactory
import org.clulab.reach.context.context_utils.ContextFeatureUtils

object Polarity4CrossValidation extends App {
  val config = ConfigFactory.load()
  val labelFile = config.getString("svmContext.labelFile")
  val dirForType = config.getString("policy4Params.mentionsOutputFile").concat("sentenceWindows")

  //val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper).concat("/policy4/sentenceWindows")
  val allSentDirs = new File(dirForType).listFiles().filter(_.isDirectory)
  val sentenceWindow = config.getString("contextEngine.params.bound").toInt
  val allRowsBySentDist = collection.mutable.HashMap[Int, Seq[AggregatedContextInstance]]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val filterForFasterRun = List("0", "1", "2", "3")
  val smallNumOfDirs = allSentDirs.filter(x => filterForFasterRun.contains(x.getName))
  for(d<- smallNumOfDirs) {
    val rowFiles = d.listFiles().filter(_.getName.contains("Aggregated"))
    val rowsForCurrentSent = collection.mutable.ListBuffer[AggregatedContextInstance]()
    for(r<-rowFiles) {
      val pathToRow = dirForType.concat(s"/${d.getName}").concat(s"/${r.getName}")
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(r)
      val row = ContextFeatureUtils.readAggRowFromFilePath(pathToRow)
      if(!rowsForCurrentSent.contains(row))
        {keysForLabels ++= Map(row -> rowSpecs)
          rowsForCurrentSent += row}

    }
    val intName = Integer.parseInt(d.getName)
    val entry = Map(intName -> rowsForCurrentSent)
    allRowsBySentDist ++= entry
  }


  val foldsBySentDist = collection.mutable.HashMap[Int, Seq[Seq[AggregatedContextInstance]]]()

  for((sentDist,rows) <- allRowsBySentDist) {
    println(s"Sentence distance ${sentDist} has a total of ${rows.size} aggregated rows")
    val foldsPerSentDist = collection.mutable.ArrayBuffer[Seq[AggregatedContextInstance]]()
    for(r<-rows) {
      val testingRows = rows.filter(_.PMCID == r.PMCID)

      foldsPerSentDist += testingRows
    }
    foldsBySentDist ++= Map(sentDist -> foldsPerSentDist)
  }

  val predictedArraysPerSentDist = collection.mutable.HashMap[Int, (Array[Int], Array[Int])]()

  for((sent, testRowsByPaper) <- foldsBySentDist) {

    val giantTruthArrayPerSentDist = collection.mutable.ListBuffer[Int]()
    val giantPredictedArrayPerSentDist = collection.mutable.ListBuffer[Int]()
    for(test <- testRowsByPaper) {

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

      val predictedLabels = predict(testingRows.toArray,sentenceWindow)
      giantTruthArrayPerSentDist ++= testingLabels
      giantPredictedArrayPerSentDist ++= predictedLabels
    }
    predictedArraysPerSentDist ++= Map(sent -> (giantTruthArrayPerSentDist.toArray, giantPredictedArrayPerSentDist.toArray))
  }

  private def predict(test: Array[AggregatedContextInstance], swindow: Int): Array[Int] = {
    val toReturn = test.map(t => {
      val indexOfSentMin = t.featureGroupNames.indexOf("sentenceDistance_min")
      val value = if(t.featureGroups(indexOfSentMin) <= swindow.toDouble) 1 else 0
      value
    })
    toReturn
  }



  val scorePerSentDist = collection.mutable.HashMap[Int,Double]()

  for((sent, (truth, predicted)) <- predictedArraysPerSentDist) {
    val precision = findPrecision(truth, predicted)
    scorePerSentDist ++= Map(sent -> precision)
  }

  def findPrecision(truth:Array[Int], test:Array[Int]):Double = {
    val countsTest = CodeUtils.predictCounts(truth, test)
    val precision = CodeUtils.precision(countsTest)
    precision
  }

  for((sent, precision) <- scorePerSentDist) {
    println(s"The sentence distance ${sent} has micro-averaged precision ${precision}")
  }
}
