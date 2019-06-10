package org.clulab.reach.context.context_utils

import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils, ContextPairInstance}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ContextFeatureAggregator(instances:Seq[ContextPairInstance], featValLookUp:Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])]) {
  val featureSetNames = collection.mutable.ListBuffer[String]()
  val featureSetValues = collection.mutable.ListBuffer[Double]()
  def aggregateContextFeatures():AggregatedContextInstance = {

    val label = None

    val inputRows = instances
    val featNameToVals = collection.mutable.Map[String,mutable.ListBuffer[Double]]()
    val specfeatureNamesToUse = instances(0).specificFeatureNames
    val ctxFeatureNamesToUse = instances(0).ctx_dependencyTails
    val evtFeatureNamesToUse = instances(0).evt_dependencyTails

    // we read through the input row values and add them to a name -> list of features map.
    // So for a given feature name as key, we will have a list of double as values, where the doubles are the values to the feature in a given input row.

    for(in <- inputRows) {
      val (specificVals, evtVals, ctxVals) = featValLookUp(in)
      for((spec,value)<-specificVals) {
        if(featNameToVals.contains(spec)) {
          val currentList = featNameToVals(spec)
          currentList += value
        }
        else {
          val toAddVal = collection.mutable.ListBuffer[Double]()
          toAddVal += value
          featNameToVals ++= Map(spec -> toAddVal)
        }
      }

      for((spec,value)<-evtVals) {
        if(featNameToVals.contains(spec)) {
          val currentList = featNameToVals(spec)
          currentList += value
        }
        else {
          val toAddVal = collection.mutable.ListBuffer[Double]()
          toAddVal += value
          featNameToVals ++= Map(spec -> toAddVal)
        }
      }

      for((spec,value)<-ctxVals) {
        if(featNameToVals.contains(spec)) {
          val currentList = featNameToVals(spec)
          currentList += value
        }
        else {
          val toAddVal = collection.mutable.ListBuffer[Double]()
          toAddVal += value
          featNameToVals ++= Map(spec -> toAddVal)
        }
      }
    }
    val aggregatedSpecVals = aggregateInputRowFeatValues(specfeatureNamesToUse, featNameToVals.toMap)
    val aggregatedctxDepVals = aggregateInputRowFeatValues(ctxFeatureNamesToUse.toSeq, featNameToVals.toMap)
    val aggregatedevtDepVals = aggregateInputRowFeatValues(evtFeatureNamesToUse.toSeq, featNameToVals.toMap)

    val specFeatVal = featureValuePairing(aggregatedSpecVals)
    val ctxFeatVal = featureValuePairing(aggregatedctxDepVals)
    val evtFeatVal = featureValuePairing(aggregatedevtDepVals)

    addAggregatedOnce(specFeatVal)
    addAggregatedOnce(ctxFeatVal)
    addAggregatedOnce(evtFeatVal)
    val newAggRow = AggregatedContextInstance(0, instances(0).PMCID, "", "", label, featureSetValues.toArray,featureSetNames.toArray)
    newAggRow
  }

  private def aggregateInputRowFeatValues(features:Seq[String], lookUpTable: Map[String,mutable.ListBuffer[Double]]):Map[String,(Double,Double, Double, Int)] = {
    val resultingMap = collection.mutable.Map[String,(Double,Double, Double, Int)]()
    for(r <- features) {
      if(lookUpTable.contains(r)) {
        val valueList = lookUpTable(r)
        val min = valueList.foldLeft(Double.MaxValue)(Math.min(_,_))
        val max = valueList.foldLeft(Double.MinValue)(Math.max(_,_))
        val sum = valueList.foldLeft(0.0)(_ + _)
        val tup = (min,max,sum,valueList.size)
        resultingMap ++= Map(r -> tup)
      }

      else {
        val tup = (0.0, 0.0, 0.0, 1)
        resultingMap ++= Map(r -> tup)
      }
    }
    resultingMap.toMap
  }

  private def featureValuePairing(aggr:Map[String,(Double,Double, Double, Int)]): Seq[(String,Double)] = {
    val pairings = collection.mutable.ListBuffer[(String,Double)]()
    for((key,value) <- aggr) {
      val extendedName = CodeUtils.extendFeatureName(key)
      val minTup = (extendedName._1, value._1)
      val maxTup = (extendedName._2, value._2)
      val avgTup = (extendedName._3, value._3/value._4)

      val list = ListBuffer(minTup, maxTup, avgTup)
      pairings ++= list
    }
    pairings
  }


  private def addAggregatedOnce(input: Seq[(String, Double)]):Unit = {
    for((name,value) <- input) {
      featureSetNames += name
      featureSetValues += value
    }
  }


}
