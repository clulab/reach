package org.clulab.context.classifiers

import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.context.utils.AggregatedContextInstance

object DummyClassifier extends ContextClassifier {
  override def fit(xTrain: Seq[AggregatedContextInstance]):Unit = ()

  override def predict(xTest: Seq[AggregatedContextInstance]): Array[Int] = List.fill(xTest.size)(1).toArray

  override def saveModel(fileName: String): Unit = ()

  override def loadFrom(fileName: String): LinearSVMContextClassifier = null
}
