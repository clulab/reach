package org.clulab.context.utils

import scala.collection.mutable
case class AggregatedContextInstance(
                             sentenceIndex:Int,
                             PMCID:String,
                             EvtID: String,
                             CtxID: String,
                             label: Option[Boolean],
                             featureGroups: Array[Double],
                             featureGroupNames:Array[String])


object AggregatedContextInstance {
  def apply(str: String, headers: Seq[String], allOtherFeatures: Set[String], indices: Map[String, Int], listOfSpecificFeatures: Array[String]):AggregatedContextInstance = {
    val rowData = str.split(",")
    val sentencePos = rowData(0).toInt
    var evt_dependencyTails = new mutable.ListBuffer[Double]
    var ctx_dependencyTails = new mutable.ListBuffer[Double]
    var evt_dependencyFeatures = new mutable.ListBuffer[String]
    var ctx_dependencyFeatures = new mutable.ListBuffer[String]
    val featureGroups = new mutable.ListBuffer[Double]
    val featureNames = new mutable.ListBuffer[String]
    allOtherFeatures foreach {
      case evt:String if evt.startsWith("evtDepTail") =>
        if(rowData(indices(evt)) != "0.0")
          {evt_dependencyTails += (rowData(indices(evt))).toDouble
            evt_dependencyFeatures += evt
          }
      case ctx:String if ctx.startsWith("ctxDepTail") =>
        if(rowData(indices(ctx)) != "0.0")
        {
          ctx_dependencyTails += (rowData(indices(ctx))).toDouble
          ctx_dependencyFeatures += ctx
        }
      case _ => 0.0
    }



    val pmcid = rowData(indices("PMCID"))

    val evt = rowData(indices("EvtID"))
    val ctx = rowData(indices("CtxID"))
    val label = rowData(indices("label"))

    val listOfNumericFeatures = listOfSpecificFeatures.drop(4)
    featureNames ++= listOfNumericFeatures
    listOfNumericFeatures.map(l => {
      val tempVal = rowData(indices(l))
      featureGroups += tempVal.toDouble
    })

    featureGroups ++= evt_dependencyTails
    featureGroups ++= ctx_dependencyTails
    featureNames ++= evt_dependencyFeatures
    featureNames ++= ctx_dependencyFeatures
    AggregatedContextInstance(sentencePos, pmcid, evt, ctx, Some(label.toBoolean), featureGroups.toArray, featureNames.toArray)
  }
}
