package org.clulab.reach.context.context_exec

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.reach.context.context_exec.CrossValBySentDist.config
import org.clulab.reach.context.context_utils.ContextFeatureUtils

object Polarity4CrossValidation extends App {
  val config = ConfigFactory.load()
  val labelFile = config.getString("svmContext.labelFile")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper).concat("/sentenceWindows")
  val allSentDirs = new File(dirForType).listFiles().filter(_.isDirectory)
  val sentenceWindow = config.getString("contextEngine.params.bound").toInt
  val allRowsBySentDist = collection.mutable.HashMap[Int, Seq[AggregatedContextInstance]]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val filterForFasterRun = List("0","1","2","3")
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


  // we only need the testing rows for the Policy4 cross validation, because we only need to check for the sentence distance min value
  // there is no need to train our model
  val foldsBySentDist = collection.mutable.HashMap[Int, Seq[AggregatedContextInstance]]()

  for((sentDist,rows) <- allRowsBySentDist) {
    println(s"Sentence distance ${sentDist} has a total of ${rows.size} aggregated rows")
    val foldsPerSentDist = collection.mutable.ArrayBuffer[AggregatedContextInstance]()
    for(r<-rows) {
      val testingRows = rows.filter(_.PMCID == r.PMCID)
      foldsPerSentDist ++= testingRows
    }
    foldsBySentDist ++= Map(sentDist -> foldsPerSentDist)
  }

  val perSentDistMicroArray = collection.mutable.HashMap[Int, (Array[Int], Array[Int])]()

  for((sentDist, test) <- foldsBySentDist) {
    val giantTruthArrayPerSentDist = collection.mutable.ListBuffer[Int]()
    val giantPredictedArrayPerSentDist = collection.mutable.ListBuffer[Int]()
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

    val predictedLabels = predict(testingRows.toArray, sentenceWindow)

    giantTruthArrayPerSentDist ++= testingLabels
    giantPredictedArrayPerSentDist ++= predictedLabels
    perSentDistMicroArray ++= Map(sentDist -> (giantTruthArrayPerSentDist.toArray, giantPredictedArrayPerSentDist.toArray))
  }


  //sentenceDistance_min

  private def predict(test: Array[AggregatedContextInstance], swindow: Int): Array[Int] = {
    val toReturn = test.map(t => {
      val indexOfSentMin = t.featureGroupNames.indexOf("sentenceDistance_min")
      val value = if(t.featureGroups(indexOfSentMin) <= swindow.toDouble) 1 else 0
      value
    })
    toReturn
  }





}
