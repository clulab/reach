package org.clulab.reach.context.context_exec

import java.io.{File, FileInputStream, ObjectInputStream, PrintWriter}

import com.typesafe.config.ConfigFactory
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils}
import org.clulab.learning.LinearSVMClassifier

import scala.io.Source

object SVMCrossValidation extends App {

  val config = ConfigFactory.load()
  val configPath = config.getString("svmContext.untrainedSVMPath")
  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
  val svmWrapper = new LinearSVMContextClassifier(Some(SVMClassifier))
  svmWrapper.saveModel(configPath)
  val unTrainedSVMInstance = svmWrapper.loadFrom(configPath)
  val labelFile = config.getString("svmContext.labelFile")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/")
  val fileListUnfiltered = new File(outPaperDirPath)
  val directories = fileListUnfiltered.listFiles().filter(_.isDirectory)
  val rowsSup = collection.mutable.ArrayBuffer[AggregatedContextInstance]()
  val idMap = collection.mutable.HashMap[(String,String,String),AggregatedContextInstance]()
  val dupMap = collection.mutable.HashMap[AggregatedContextInstance, (String,String,String)]()
  val metricsMapPerPaper = collection.mutable.HashMap[String,(Double, Double, Double)]()
  val collectCountsMap = collection.mutable.ListBuffer[Map[String,Int]]()
  for(d<-directories) {
    val rowFiles = d.listFiles().filter(_.getName.contains("Aggregated"))
    val rows = rowFiles.map(file => {
      val pmcid = file.getName.split("_")(1)
      val evtID = file.getName.split("_")(2)
      val ctxID = file.getName.split("_")(3)
      val ctxID2 = ctxID.slice(0,ctxID.length-4)
      val filePath = outPaperDirPath.concat(pmcid).concat(s"/${file.getName}")
      val row = readAggRowFromFile(filePath)
      val tuple = (pmcid,evtID,ctxID2)
      val mapEntry = Map(tuple -> row)
      idMap ++= mapEntry

      val dupMapEntry = Map(row -> tuple)
      dupMap ++= dupMapEntry
      row
    })
    rowsSup ++= rows
  }



  val groupedByPaperID = rowsSup.groupBy(row => s"PMC${row.PMCID.split("_")(0)}")


  val folds = collection.mutable.ArrayBuffer[(Seq[AggregatedContextInstance], Seq[AggregatedContextInstance])]()




  groupedByPaperID.keySet.map(s=> {
    val trainingKeys = groupedByPaperID.keySet.filter(_ != s)
    val trainingRows = trainingKeys.map(key => {
      groupedByPaperID(key)
    })
    val testingRows = groupedByPaperID(s)
    val perFold = (testingRows, trainingRows.flatten.toSeq)
    folds += perFold

  })



  val giantTruthLabel = collection.mutable.ListBuffer[Int]()
  val giantPredictedLabel = collection.mutable.ListBuffer[Int]()

  for((test,train) <- folds) {
    val trainingLabelsIds = collection.mutable.ListBuffer[(String,String,String)]()
    train.map(row => {
      val pmcid = row.PMCID.split("_")(0)
      val pmcidReformat = s"PMC${pmcid}"
      val evtCtxPerPaper = idMap.keySet.filter(_._1 == pmcidReformat)
      trainingLabelsIds ++= evtCtxPerPaper
    })


    val intersectingLabels = trainingLabelsIds.toSet.intersect(generateLabelMap(labelFile).keySet)
    val trainingRows = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val trainingLabels = collection.mutable.ListBuffer[Int]()
    for(idTup <- intersectingLabels) {
      val row = idMap(idTup)
      val label = generateLabelMap(labelFile)(idTup)
      trainingRows += row
      trainingLabels += label
    }
    //val balancedTrainingData = Balancer.balanceByPaperAgg(trainingRows, 1)
    //val (trainingRVFDataset, _) = unTrainedSVMInstance.dataConverter(balancedTrainingData,Some(trainingLabels.toArray))
    val (trainingRVFDataset, _) = unTrainedSVMInstance.dataConverter(trainingRows,Some(trainingLabels.toArray))

    unTrainedSVMInstance.fit(trainingRVFDataset)

    // calculating precision per paper as test case.
    val testingLabelsIDs = collection.mutable.ListBuffer[(String,String,String)]()
    test.map(row => {
      val pmcid = row.PMCID.split("_")(0)
      val pmcidReformat = s"PMC${pmcid}"
      val evtCtxPerPaper = idMap.keySet.filter(_._1 == pmcidReformat)
      testingLabelsIDs ++= evtCtxPerPaper
    })
    val intersectingTestingLabels = testingLabelsIDs.toSet.intersect(generateLabelMap(labelFile).keySet)
    val testingRows = collection.mutable.ListBuffer[AggregatedContextInstance]()
    val testingLabels = collection.mutable.ListBuffer[Int]()
    for(idTup <- intersectingTestingLabels) {
      val row = idMap(idTup)
      val label = generateLabelMap(labelFile)(idTup)
      testingRows += row
      testingLabels += label
    }



    // filtering out pairs for which the prediction was 0
    // check with Prof. Morrison if this kind of filtering is correct
    // leave it commented for now but uncomment the filtering code if he recommends you to.
    val predictedLabels = unTrainedSVMInstance.predict(testingRows)

    giantTruthLabel ++= testingLabels
    giantPredictedLabel ++= predictedLabels


    val testPaperPMCID = test(0).PMCID
    val testIDReformat = s"PMC${testPaperPMCID.split("_")(0)}"
    val (metricsPerTestCase, countsPerTestCase) = findMetrics(testingLabels.toArray, predictedLabels)
    collectCountsMap += countsPerTestCase
    val metricsScorePerPaperID = Map(testIDReformat -> metricsPerTestCase)
    metricsMapPerPaper ++= metricsScorePerPaperID

  }

  val (metrics, _) = findMetrics(giantTruthLabel.toArray, giantPredictedLabel.toArray)

  //val countsTest = CodeUtils.predictCounts(giantTruthLabel.toArray, giantPredictedLabel.toArray)

  println(s"Micro-averaged Precision: ${metrics._1.toString.take(7)}")
  println(s"Micro-averaged Recall: ${metrics._2}")
  println(s"Micro-averaged Accuracy: ${metrics._3.toString.take(7)}")


  println("Total sample count: " + metricsMapPerPaper.size)
  for((paperID, metrics) <- metricsMapPerPaper) {
    println("Current Paper ID: " + paperID + " \t Precision: " + metrics._1.toString.take(7) + " \t Recall: " + metrics._2 + "\t Accuracy: " + metrics._3.toString.take(7))
  }
  writeMetricsToCSV(metricsMapPerPaper.toMap)

  val precisionOverAllPapers = collection.mutable.ListBuffer[Double]()
    metricsMapPerPaper foreach (x => precisionOverAllPapers += x._2._1)
  val precAggrMetrics = findAggrMetrics(precisionOverAllPapers)
  println(s"Avg precision (arithmetic mean) over 14 papers: ${precAggrMetrics._3.toString.take(7)}")

  val recallOverAllPapers = collection.mutable.ListBuffer[Double]()
    metricsMapPerPaper foreach (x => recallOverAllPapers += x._2._2)
  val recAggrMetrics = findAggrMetrics(recallOverAllPapers)
  println(s"Avg recall (arithmetic mean) over 14 papers: ${recAggrMetrics._3.toString.take(7)}")

  val accuracyOverAllPapers = collection.mutable.ListBuffer[Double]()
    metricsMapPerPaper foreach (x => accuracyOverAllPapers += x._2._3)
  val accuracyAggrMetrics = findAggrMetrics(accuracyOverAllPapers)
  println(s"Avg accuracy (arithmetic mean) over 14 papers: ${accuracyAggrMetrics._3.toString.take(7)}")


  def findMetrics(truth:Array[Int], test:Array[Int]):((Double,Double,Double),Map[String,Int]) = {
    val countsTest = CodeUtils.predictCounts(truth, test)
    val precision = CodeUtils.precision(countsTest)
    val recall = CodeUtils.recall(countsTest)
    val accuracy = CodeUtils.accuracy(countsTest)
    ((precision,recall,accuracy), countsTest)
  }

  def readAggRowFromFile(file: String):AggregatedContextInstance = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val c = is.readObject().asInstanceOf[AggregatedContextInstance]
    is.close()
    c
  }


  def generateLabelMap(fileName: String): Map[(String,String,String), Int] = {
    val map = collection.mutable.HashMap[(String,String,String), Int]()
    val source = Source.fromFile(fileName)
    val lines = source.getLines()
    val content = lines.drop(1)
    for(c <- content) {
      val array = c.split(",")
      val pmcid = array(0)
      val evtID = array(1)
      val ctxID = array(2)
      val label = Integer.parseInt(array(3))
      val tup = (pmcid,evtID,ctxID)
      map ++= Map(tup -> label)
    }

    map.toMap
  }


  def findAggrMetrics(seq:Seq[Double]): (Double,Double,Double) = {
    val min = seq.foldLeft(Double.MaxValue)(Math.min(_,_))
    val max = seq.foldLeft(Double.MinValue)(Math.max(_,_))
    val sum = seq.foldLeft(0.0)(_+_)
    val avg = sum.toDouble/seq.size.toDouble
    (min,max,avg)
  }

  def writeMetricsToCSV(metricMap:Map[String,(Double,Double,Double)]): Unit = {
    val cvmetricsFilePath = config.getString("svmContext.cvmetricsFilePath")
    val pw = new PrintWriter(cvmetricsFilePath)
    for((key,valueTup) <- metricMap) {
      val string = s"${key},${valueTup._1.toString.take(5)},${valueTup._2.toString.take(5)},${valueTup._3.toString.take(5)}"
      pw.write(string)
      pw.write("\n")
    }
    pw.close()
  }
}
