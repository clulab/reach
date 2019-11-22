package org.clulab.context.utils

object ICDMCrossValidationUtils {


  def combineTrainVal(folds: Array[(Array[Int], Array[Int], Array[Int])]):Array[(Array[Int], Array[Int])] = {
    val trainValCombined = collection.mutable.ListBuffer[(Array[Int], Array[Int])]()
    for((train,validate,test) <- folds) {
      val trainVal = train ++ validate
      val toAdd = (trainVal, test)
      trainValCombined += toAdd
    }
    trainValCombined.toArray
  }


}
