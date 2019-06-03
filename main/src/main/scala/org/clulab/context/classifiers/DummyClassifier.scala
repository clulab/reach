package org.clulab.context.data.classifiers

import org.clulab.context.data.utils.AggregatedRow
import org.clulab.context.data.utils.AggregatedRow

object DummyClassifier extends ClassifierMask {
  override def fit(xTrain: Seq[AggregatedRow]):Unit = ()

  override def predict(xTest: Seq[AggregatedRow]): Array[Int] = List.fill(xTest.size)(1).toArray

  def convertBooleansToInt(labels: Seq[Boolean]):Array[Int] = {

    val toReturn = labels.map(l => l match {
      case true => 1
      case false => 0
    })
    toReturn.toArray
  }

  def convertOptionalToBool(rows: Seq[AggregatedRow]): Seq[Boolean] = {
    rows.map(x => x.label match {
      case Some(x) => x
      case _ => false
    })
  }

  override def saveModel(fileName: String): Unit = ()

  override def loadFrom(fileName: String): LinearSVMWrapper = null
}
