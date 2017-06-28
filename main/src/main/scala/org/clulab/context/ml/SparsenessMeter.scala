package org.clulab.context.ml

import org.clulab.learning.RVFDatum

import scala.collection.mutable

/**
  * Measures the sparseness of the design matrix
  * Created by enrique on 28/06/17.
  */
class SparsenessMeter {

  // Keep track of the data to compute the sparseness
  private val sparsnessArray = new mutable.ArrayBuffer[Int]
  private val presentLabels = new mutable.HashSet[String]
  private var n:Int = 0


  /***
    * Counts the number of non-zero count features in the datum to compute the
    * design matrix's sparseness
    *
    * @param v Datum to account for
    */
  def accountVector(v:RVFDatum[String, String]): Unit ={
    sparsnessArray += v.features map v.getFeatureCount count (_ > 0)
    presentLabels ++= v.features
    n += 1
  }

  /***
    * Computes the sparseness after accounting for the data
    * @return
    */
  def sparseness = sparsnessArray.sum / (presentLabels.size.toDouble * n)

  /***
    * Number of features present in the dataset
    * @return
    */
  def totalFeatures = presentLabels.size

}
