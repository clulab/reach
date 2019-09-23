package org.clulab.reach.context.utils.svm_training_utils

import java.io.{BufferedInputStream, FileInputStream, ObjectInputStream}
import java.util.zip.GZIPInputStream

import org.clulab.context.utils.AggregatedContextInstance

import org.clulab.reach.context.utils.feature_utils.FeatureNameProcessor

import scala.io.Source

object IOUtilsForFeatureName {
  def loadAggregatedRowsFromDataFrame(groupedFeaturesFileName: String, pathToSpecificNonDepFeatures: String):(Seq[String], Seq[AggregatedContextInstance]) = {
    val listOfSpecificFeatures = readSpecificNonDependencyFeatureNames(pathToSpecificNonDepFeatures)
    def allOtherFeatures(headers:Seq[String]): Set[String] = headers.toSet -- (listOfSpecificFeatures ++ Seq(""))
    def indices(headers:Seq[String]): Map[String, Int] = headers.zipWithIndex.toMap
    val fileInputStream = new FileInputStream(groupedFeaturesFileName)
    val bufferedStream = new BufferedInputStream(new GZIPInputStream(fileInputStream))
    val source = Source.fromInputStream(bufferedStream)
    val lines = source.getLines()
    val headers = lines.next() split ","
    val rectifiedHeaders = FeatureNameProcessor.fixFeatureNameInInputStream(headers)
    val features = allOtherFeatures(rectifiedHeaders)
    val ixs = indices(rectifiedHeaders)
    val ret = lines.map(l => AggregatedContextInstance(l, rectifiedHeaders, features, ixs, listOfSpecificFeatures)).toList
    source.close()
    (rectifiedHeaders, ret)
  }



  def getTrainingFeatures(file:String):Map[String, Seq[String]] = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val headers = is.readObject().asInstanceOf[Array[String]]
    val rectifiedHeaders = FeatureNameProcessor.fixFeatureNameInInputStream(headers)
    is.close()
    FeatureNameProcessor.createBestFeatureSetForTraining(rectifiedHeaders)
  }

  def readSpecificNonDependencyFeatureNames(fileName: String):Array[String] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val headers = is.readObject().asInstanceOf[Array[String]]
    headers
  }
}
