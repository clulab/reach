package org.clulab.context.classifiers

import org.clulab.context.utils.AggregatedContextInstance

trait ContextClassifier{
  def fit(xTrain: Seq[AggregatedContextInstance]): Unit

  def predict(xTest: Seq[AggregatedContextInstance]):Array[Int]
  def saveModel(fileName: String): Unit
  def loadFrom(fileName: String):LinearSVMContextClassifier
}
