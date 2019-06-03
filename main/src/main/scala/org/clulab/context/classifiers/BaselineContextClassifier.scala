package org.clulab.context.classifiers

import org.clulab.context.utils.AggregatedContextInstance

case class Baseline(k:Int) extends ContextClassifier {
  override def fit(xTrain: Seq[AggregatedContextInstance]): Unit = {}

  override def predict(xTest: Seq[AggregatedContextInstance]): Array[Int] = {
    val convert = dataConverter(xTest)
    val toPass = convert.map(s => s(0))
    deterministicSentenceDist(toPass, k)
  }

  private def deterministicSentenceDist(sentDistVals:Array[Double], k:Int):Array[Int] = {
    val res = sentDistVals.map(s => if(s <= k) 1 else 0)
    res
  }

  override def saveModel(fileName: String): Unit = ()

  override def loadFrom(fileName: String): LinearSVMContextClassifier = null

  def dataConverter(data:Seq[AggregatedContextInstance]):Array[Array[Double]] = {
    // sentence distance is at index 18 in the row. We will extract the value at this index, create an array from it,
    // and then add it to the resulting value.
    val sentDistIndex = 18
    val result = collection.mutable.ListBuffer[Array[Double]]()
    data.map(d => {
      result += Array(d.featureGroups(sentDistIndex))
    })
    result.toArray
  }

  def createLabels(data:Seq[AggregatedContextInstance]):Array[Int] = {
    val currentTruthTest = DummyClassifier.convertOptionalToBool(data)
    val currentTruthTestInt = DummyClassifier.convertBooleansToInt(currentTruthTest)
    currentTruthTestInt
  }
}
