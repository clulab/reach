package org.clulab.reach.context.utils.svm_training_utils

import org.clulab.context.utils.AggregatedContextInstance

object DatatypeConversionUtils {
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
}
