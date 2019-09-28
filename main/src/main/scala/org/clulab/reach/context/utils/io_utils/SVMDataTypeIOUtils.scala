package org.clulab.reach.context.utils.io_utils

import java.io.{BufferedInputStream, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.zip.GZIPInputStream

import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.learning.RVFDataset

import scala.collection.mutable
import scala.io.Source

object SVMDataTypeIOUtils {
  def writeHardcodedFeaturesToFile(fileName: String):Unit = {
    val listOfSpecificForAggregated = Seq("PMCID", "label", "EvtID", "CtxID", "closesCtxOfClass_min", "closesCtxOfClass_max", "closesCtxOfClass_avg", "context_frequency_min","context_frequency_max", "context_frequency_avg",
      "evtNegationInTail_min","evtNegationInTail_max","evtNegationInTail_avg", "ctxSentenceFirstPerson_min","ctxSentenceFirstPerson_max","ctxSentenceFirstPerson_avg","ctxNegationIntTail_min","ctxNegationIntTail_max","ctxNegationIntTail_avg","evtSentenceFirstPerson_min","evtSentenceFirstPerson_max","evtSentenceFirstPerson_avg", "evtSentencePastTense_min","evtSentencePastTense_max","evtSentencePastTense_avg", "evtSentencePresentTense_min","evtSentencePresentTense_max","evtSentencePresentTense_avg", "ctxSentencePresentTense_min","ctxSentencePresentTense_max","ctxSentencePresentTense_avg", "ctxSentencePastTense_max","ctxSentencePastTense_min", "ctxSentencePastTense_avg","ctxSentenceFirstPerson_min","ctxSentenceFirstPerson_min","ctxSentenceFirstPerson_min","sentenceDistance_min","sentenceDistance_max","sentenceDistance_avg", "dependencyDistance_min", "dependencyDistance_max", "dependencyDistance_avg")
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os.writeObject(listOfSpecificForAggregated.toArray)
    os.close
  }

  def readHardcodedFeaturesFromFile(fileName: String):Array[String] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val headers = is.readObject().asInstanceOf[Array[String]]
    headers
  }

  def readRVFDatasetFromFile(fileName: String): RVFDataset[Int, String] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val dataset = is.readObject.asInstanceOf[RVFDataset[Int, String]]
    dataset
  }

  def writeAllFeaturesToFile(allFeatures:Seq[String], fileName:String):Seq[String] = {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    val str = new mutable.StringBuilder()
    for(i<- 0 until allFeatures.size - 1) {
      val current = allFeatures(i)
      str.append(current+",")
    }
    str.append(allFeatures.last)
    val stringEquiv = str.toString()
    val arr = stringEquiv.split(",")
    os.writeObject(arr.asInstanceOf[Array[String]])
    os.close()
    allFeatures
  }

  def loadAggregatedRowsFromFile(groupedFeaturesFileName: String, hardCodedFilePath: String):(Seq[String], Seq[AggregatedContextInstance]) = {
    val listOfSpecificFeatures = SVMDataTypeIOUtils.readHardcodedFeaturesFromFile(hardCodedFilePath)
    def allOtherFeatures(headers:Seq[String]): Set[String] = headers.toSet -- (listOfSpecificFeatures ++ Seq(""))
    def indices(headers:Seq[String]): Map[String, Int] = headers.zipWithIndex.toMap
    val fileInputStream = new FileInputStream(groupedFeaturesFileName)
    val bufferedStream = new BufferedInputStream(new GZIPInputStream(fileInputStream))
    val source = Source.fromInputStream(bufferedStream)
    val lines = source.getLines()
    val headers = lines.next() split ","
    val rectifiedHeaders = rectifyWrongFeatures(headers)
    val features = allOtherFeatures(rectifiedHeaders)
    val ixs = indices(rectifiedHeaders)
    val ret = lines.map(l => AggregatedContextInstance(l, rectifiedHeaders, features, ixs, listOfSpecificFeatures)).toList
    source.close()
    (rectifiedHeaders, ret)
  }

  def bestFeatureSetForTrainingConstructor(file:String):Map[String, Seq[String]] = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val headers = is.readObject().asInstanceOf[Array[String]]
    val rectifiedHeaders = rectifyWrongFeatures(headers)
    is.close()
    createBestFeatureSet(rectifiedHeaders)
  }

  def rectifyWrongFeatures(headers:Seq[String]): Seq[String] = {
    val result = collection.mutable.ListBuffer[String]()
    headers.map(h => if(headers.indexOf(h) == 1) result += "PMCID" else result += h)
    result
  }

  def createBestFeatureSet(allFeatures:Seq[String]):Map[String, Seq[String]] = {
    val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
    val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
    val featureDict = createFeatureDictionary(numericFeatures.toSeq)
    featureDict
  }

  def createFeatureDictionary(numericFeatures: Seq[String]):Map[String, Seq[String]] = {
    val contextDepFeatures = numericFeatures.filter(_.startsWith("ctxDepTail"))
    val eventDepFeatures = numericFeatures.filter(_.startsWith("evtDepTail"))
    val nonDepFeatures = numericFeatures.toSet -- (contextDepFeatures.toSet ++ eventDepFeatures.toSet)
    val map = collection.mutable.Map[String, Seq[String]]()
    map += ("All_features" -> numericFeatures)
    map += ("Non_Dependency_Features" -> nonDepFeatures.toSeq)
    map += ("NonDep_Context" -> (nonDepFeatures ++ contextDepFeatures.toSet).toSeq)
    map += ("NonDep_Event" -> (nonDepFeatures ++ eventDepFeatures.toSet).toSeq)
    map += ("Context_Event" -> (contextDepFeatures.toSet ++ eventDepFeatures.toSet).toSeq)
    map.toMap
  }
}
