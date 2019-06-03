package org.clulab.context.utils

import org.clulab.context.classifiers.{BaselineContextClassifier, LinearSVMContextClassifier}

import scala.collection.mutable
import scala.io.BufferedSource

case class FoldMaker(groupedFeatures: Map[(String, String, String), AggregatedContextInstance]) extends Iterable[(Array[Int], Array[Int], Array[Int])]{
  def toFolds:Iterable[(Array[Int], Array[Int], Array[Int])] = new mutable.HashSet[(Array[Int], Array[Int], Array[Int])]()
  override def iterator:Iterator[(Array[Int], Array[Int], Array[Int])] = this.toFolds.iterator
}

object FoldMaker {

  def getFoldsPerPaper(bufSource:BufferedSource):Array[(Array[Int], Array[Int], Array[Int])] = {
    val perPaperLines = bufSource.getLines()
    val toReturn = collection.mutable.ListBuffer[(Array[Int], Array[Int], Array[Int])]()
    perPaperLines.foreach(p => {
      val sets = p.split("\\]\",\"\\[")
      val cleanStrSets = sets.map(s => s.replace("\"[",""))
      val clean2 = cleanStrSets.map(c => c.replace("]\"",""))
      val trainIndex = splitAndExtract(clean2(0))
      val valIndex = splitAndExtract(clean2(1))
      val testIndex = splitAndExtract(clean2(2))
      val tup = (trainIndex, valIndex, testIndex)
      toReturn += tup
    })

    def splitAndExtract(arr: String):Array[Int] = {
      val strArr = arr.split(", ")
      val intVals = strArr.map(s => Integer.parseInt(s))
      intVals
    }
    toReturn.toArray
  }

  def baselineController(foldsFromCSV: Array[(Array[Int], Array[Int], Array[Int])], rows2: Seq[AggregatedContextInstance]): (Array[Int], Array[Int]) = {
    val giantTruthTestLabel = new mutable.ArrayBuffer[Int]()
    val giantPredTestLabel = new mutable.ArrayBuffer[Int]()
    for((train,_,test) <- foldsFromCSV) {
      val trainingData = train.collect{case x:Int => rows2(x)}
      val balancedTrainingData = Balancer.balanceByPaperAgg(trainingData, 1)
      val kToF1Map = new mutable.HashMap[Int, Double]
      for(k_val <- 0 until 51) {
        val trainInstance = new BaselineContextClassifier(k_val)
        val pred = trainInstance.predict(balancedTrainingData)
        val labelsToInt = trainInstance.createLabels(balancedTrainingData)
        val counts = CodeUtils.predictCounts(labelsToInt, pred)
        val f1score = CodeUtils.f1(counts)
        kToF1Map += (k_val -> f1score)
      }
      val bestK = CodeUtils.argMax(kToF1Map.toMap)
      val testInstance = new BaselineContextClassifier(bestK)
      val testingData = test.collect{case x:Int => rows2(x)}
      val currentTruthTestInt = testInstance.createLabels(testingData)
      giantTruthTestLabel ++= currentTruthTestInt

      val predTestLabel = testInstance.predict(testingData)
      giantPredTestLabel ++= predTestLabel

    }
    (giantTruthTestLabel.toArray, giantPredTestLabel.toArray)
  }

  def svmControllerLinearSVM(svmInstance: LinearSVMContextClassifier, foldsFromCSV: Array[(Array[Int], Array[Int])], rows2: Seq[AggregatedContextInstance]): (Array[Int], Array[Int]) = {
    val giantTruthTestLabel = new mutable.ArrayBuffer[Int]()
    val giantPredTestLabel = new mutable.ArrayBuffer[Int]()
    for((train,test) <- foldsFromCSV) {
      val trainingData = train.collect{case x:Int => rows2(x)}
      val balancedTrainingData = Balancer.balanceByPaperAgg(trainingData, 1)
      val (trainDataSet, _) = svmInstance.dataConverter(balancedTrainingData)
      svmInstance.fit(trainDataSet)

      val testingData = test.collect{case t: Int => rows2(t)}
      val testLabelsTruth = svmInstance.createLabels(testingData)
      giantTruthTestLabel ++= testLabelsTruth
      val testLabelsPred = svmInstance.predict(testingData)
      giantPredTestLabel ++= testLabelsPred
    }

    (giantTruthTestLabel.toArray, giantPredTestLabel.toArray)

  }
}
