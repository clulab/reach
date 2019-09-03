package org.clulab.reach.context.context_exec

import java.io.File

import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.reach.context.context_utils.ContextFeatureUtils

object CrossValBySentDist extends App{
  val config = ConfigFactory.load()
  val labelFile = config.getString("svmContext.labelFile")
  val labelMap = CodeUtils.generateLabelMap(labelFile)
  val svmWrapper = new LinearSVMContextClassifier()
  val configPath = config.getString("contextEngine.params.trainedSvmPath")
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }

  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")
  val dirForType = config.getString("policy4Params.mentionsOutputFile").concat("sentenceWindows")
  val allSentDirs = new File(dirForType).listFiles().filter(_.isDirectory)
  val allRowsBySentDist = collection.mutable.HashMap[Int, Seq[AggregatedContextInstance]]()
  val keysForLabels = collection.mutable.HashMap[AggregatedContextInstance, (String, String, String)]()
  val filterForFasterRun = List("15")
  val smallNumOfDirs = allSentDirs.filter(x => filterForFasterRun.contains(x.getName))
  for(d<- smallNumOfDirs) {
    println(s"Starting sentence distance ${d.getName}")
    val rowFiles = d.listFiles().filter(_.getName.contains("Aggregated"))
    val rowsForCurrentSent = collection.mutable.ListBuffer[AggregatedContextInstance]()
    for(r<-rowFiles) {
      val pathToRow = dirForType.concat(s"/${d.getName}").concat(s"/${r.getName}")
      val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(r)
      val row = ContextFeatureUtils.readAggRowFromFile(pathToRow)
      if(!rowsForCurrentSent.contains(row))
      {keysForLabels ++= Map(row -> rowSpecs)
        rowsForCurrentSent += row}

    }
    val intName = Integer.parseInt(d.getName)
    val entry = Map(intName -> rowsForCurrentSent)
    allRowsBySentDist ++= entry
  }

  val giantTruthListPerSentDist = collection.mutable.ListBuffer[Int]()
  val giantPredListPerSentDist = collection.mutable.ListBuffer[Int]()
  val perSentDistScoreBoard = collection.mutable.HashMap[Int, (Double, Double, Double)]()

  for((sentDist, rowsInThisSentDist) <- allRowsBySentDist) {
    val predListPerSentDist = collection.mutable.ListBuffer[Int]()
    val truthListPerSentDist = collection.mutable.ListBuffer[Int]()
    for(r <- rowsInThisSentDist) {
      val rowSpecForCurrentRow = keysForLabels(r)
      if(labelMap.contains(rowSpecForCurrentRow)) {
        val truth = labelMap(rowSpecForCurrentRow)
        val pred = svmWrapper.predict(Seq(r))(0)
        predListPerSentDist += pred
        truthListPerSentDist += truth
      }
    }

    println(predListPerSentDist.size)
    println(truthListPerSentDist)

  }



}
