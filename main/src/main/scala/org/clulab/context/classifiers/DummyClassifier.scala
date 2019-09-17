package org.clulab.context.classifiers

import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.context.utils.AggregatedContextInstance

object DummyClassifier extends ContextClassifier {
  override def fit(xTrain: Seq[AggregatedContextInstance]):Unit = ()

  override def predict(xTest: Seq[AggregatedContextInstance]): Array[Int] = List.fill(xTest.size)(1).toArray

  def convertBooleansToInt(labels: Seq[Boolean]):Array[Int] = {

    val toReturn = labels.map(l => l match {
      case true => 1
      case false => 0
    })
    toReturn.toArray
  }

  def convertOptionalToBool(rows: Seq[AggregatedContextInstance]): Seq[Boolean] = {
    rows.map(x => x.label match {
      case Some(x) => x
      case _ => false
    })
  }

  override def saveModel(fileName: String): Unit = ()

  override def loadFrom(fileName: String): LinearSVMContextClassifier = null
}
