package org.clulab.reach.context.research_exec

import java.io.File

import org.clulab.context.utils.{AggregatedContextInstance, CrossValidationUtils}
import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.reach.context.feature_utils.ContextFeatureUtils
import org.clulab.reach.context.utils.io_utils.ReachSystemAnalysisIOUtils
import org.clulab.reach.context.utils.score_utils.ScoreMetricsOfClassifier

object CrossValBySentDist extends App{
  val svmWrapper = new LinearSVMContextClassifier()
  val config = ConfigFactory.load()


  val configPath = config.getString("contextEngine.params.trainedSvmPath")
  println(configPath)
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }

  println(classifierToUse == null)

  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")
  val labelFile = config.getString("svmContext.labelFile")
  val labelMap = ReachSystemAnalysisIOUtils.generateLabelMap(labelFile)
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
      val row = ContextFeatureUtils.readAggRowFromFilePath(pathToRow)
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
  val valsForMeanPrec = collection.mutable.ListBuffer[Double]()
  val valsForMeanRec = collection.mutable.ListBuffer[Double]()
  val valsForMeanF1 = collection.mutable.ListBuffer[Double]()
  for((sentDist, rowsInThisSentDist) <- allRowsBySentDist) {
    val predListPerSentDist = collection.mutable.ListBuffer[Int]()
    val truthListPerSentDist = collection.mutable.ListBuffer[Int]()
    for(r <- rowsInThisSentDist) {
      val rowSpecForCurrentRow = keysForLabels(r)
      if(labelMap.contains(rowSpecForCurrentRow)) {
        val truth = labelMap(rowSpecForCurrentRow)
        val pred = trainedSVMInstance.predict(Seq(r))(0)
        predListPerSentDist += pred
        truthListPerSentDist += truth
      }
    }


    giantTruthListPerSentDist ++= truthListPerSentDist
    giantPredListPerSentDist ++= predListPerSentDist

    println(predListPerSentDist.size)
    println(truthListPerSentDist.size)


    val perPaperCountsMap = ScoreMetricsOfClassifier.predictCounts(truthListPerSentDist.toArray, predListPerSentDist.toArray)
    val perPaperPrecision = ScoreMetricsOfClassifier.precision(truthListPerSentDist.toArray, predListPerSentDist.toArray)
    val perPaperRecall = ScoreMetricsOfClassifier.recall(truthListPerSentDist.toArray, predListPerSentDist.toArray)
    val perPaperF1 = ScoreMetricsOfClassifier.f1(truthListPerSentDist.toArray, predListPerSentDist.toArray)
    valsForMeanPrec += perPaperPrecision
    valsForMeanRec += perPaperRecall
    valsForMeanF1 += perPaperF1

    perSentDistScoreBoard ++= Map(sentDist -> (perPaperPrecision, perPaperRecall, perPaperF1))
  }

  val microAveragedMap = ScoreMetricsOfClassifier.predictCounts(giantTruthListPerSentDist.toArray, giantPredListPerSentDist.toArray)
  val microAveragedPrecision = ScoreMetricsOfClassifier.precision(giantTruthListPerSentDist.toArray, giantPredListPerSentDist.toArray)
  val microAveragedRecall = ScoreMetricsOfClassifier.recall(giantTruthListPerSentDist.toArray, giantPredListPerSentDist.toArray)
  val microAveragedF1 = ScoreMetricsOfClassifier.f1(giantTruthListPerSentDist.toArray, giantPredListPerSentDist.toArray)
  for((sentDist,(prec,rec,f1)) <- perSentDistScoreBoard) {
    println(s"The sentence distance ${sentDist} has precision of ${prec}, recall of ${rec} and f1 score of ${f1}")
  }


  val arithmeticMeanPrec = ScoreMetricsOfClassifier.arithmeticMeanScore(valsForMeanPrec)
  val arithmeticMeanRecl = ScoreMetricsOfClassifier.arithmeticMeanScore(valsForMeanRec)
  val arithmeticMeanF1 = ScoreMetricsOfClassifier.arithmeticMeanScore(valsForMeanF1)


  println(s"Arithmetic mean precision: ${arithmeticMeanPrec}")
  println(s"Arithmetic mean recall: ${arithmeticMeanRecl}")
  println(s"Arithmetic mean F1: ${arithmeticMeanF1}")


}
