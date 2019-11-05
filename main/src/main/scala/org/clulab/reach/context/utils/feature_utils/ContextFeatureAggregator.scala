package org.clulab.reach.context.feature_utils

import org.clulab.context.utils.{AggregatedContextInstance, CrossValidationUtils, ContextPairInstance}
import org.clulab.reach.context.utils.feature_utils.FeatureNameCruncher

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ContextFeatureAggregator(instances:Seq[ContextPairInstance], featValLookUp:Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])]) {
  val featureSetNames = collection.mutable.ListBuffer[String]()
  val featureSetValues = collection.mutable.ListBuffer[Double]()
  def aggregateContextFeatures():AggregatedContextInstance = {

    val label = None

    //val inputRows = instances
    val featNameToVals = collection.mutable.Map[String,mutable.ListBuffer[Double]]()
    // we are using the same set of features over all the ContextPairInstance instances, hence using the first ContextPairInstance in the sequence to get the names of the features is a safe step.
    val specfeatureNamesToUse = instances(0).specificFeatureNames
    val ctxFeatureNamesToUse = instances(0).ctx_dependencyTails
    val evtFeatureNamesToUse = instances(0).evt_dependencyTails

    // we read through the ContextPairInstance values and add them to a name -> list of features map.
    // So for a given feature name as key, we will have a list of double as values, where each double is the value to the feature in a given ContextPairInstance.

    for(in <- instances) {
      val (specificVals, evtVals, ctxVals) = featValLookUp(in)
      for((spec,value)<-specificVals) {
        if(featNameToVals.contains(spec)) {
          val currentList = featNameToVals(spec)
          currentList += value
          featNameToVals(spec) = currentList
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
          featNameToVals(spec) = currentList
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
          featNameToVals(spec) = currentList
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
    println(s"In feature aggregator class, the size of feature value pairings of event dependency features is ${evtFeatVal.size}")
    addAggregatedOnce(specFeatVal)
    addAggregatedOnce(ctxFeatVal)
    addAggregatedOnce(evtFeatVal)

    // In the ContextPairInstance, we had filtered the feature names into three separate feature sets: non-dependency, ctx-dependency and evt-dependency.
    // But in the AggregatedContextInstance, we will add them all into one list, called featureSetNames.
    // featureSetNames and featureSetValues are of the same sizes. Both lists are created such that feature name at index i in featureSetNames has its corresponding value at index i in featureSetValues
    val newAggRow = AggregatedContextInstance(0, instances(0).PMCID, "", "", label, featureSetValues.toArray,featureSetNames.toArray)
    newAggRow
  }

  private def aggregateInputRowFeatValues(features:Seq[String], valuesPerGivenFeature: Map[String,mutable.ListBuffer[Double]]):Map[String,(Double,Double, Double, Int)] = {
    val resultingMap = collection.mutable.Map[String,(Double,Double, Double, Int)]()
    for(r <- features) {
      if(valuesPerGivenFeature.contains(r)) {
        val valueList = valuesPerGivenFeature(r)
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
      val extendedName = FeatureNameCruncher.extendFeatureNameToMinMaxAvg(key)
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
