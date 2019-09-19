package org.clulab.context.utils

import java.io.InputStream

import com.typesafe.config.ConfigFactory

import scala.collection.mutable
import scala.io.Source
case class ContextPairInstance(
                     sentenceIndex:Int,
                     PMCID:String,

                     label: Option[Boolean],
                     EvtID: String,
                     CtxID: String,
                     specificFeatureNames:Array[String],
                     ctx_dependencyTails:Set[String],
                     evt_dependencyTails:Set[String]
                   )

object ContextPairInstance{

  val resourcesPath = "/org/clulab/context/svmFeatures"


  val pathToSpecificNonDepFeatures = s"${resourcesPath}/specific_nondependency_featurenames.txt"
  val urlToSpecificNonDependFeaturesFile = getClass.getResource(pathToSpecificNonDepFeatures)
  // this function call to getResource returns to us a URL that is the path to the file svm_model.dat
  // the variable urlToSpecificNonDependFeaturesFile holds the value file:/home/....
  // so we need to take the shorter version of it that starts from /home/...
  val truncatedPathToSpecificNonDep = urlToSpecificNonDependFeaturesFile.toString.replace("file:","")
  val listOfSpecificNonDependFeatures = Scores_IO_Utils.readHardcodedFeaturesFromFile(truncatedPathToSpecificNonDep)
  private def allOtherFeatures(headers:Seq[String]): Set[String] = headers.toSet -- (listOfSpecificNonDependFeatures ++ Seq(""))

  private def indices(headers:Seq[String]): Map[String, Int] = headers.zipWithIndex.toMap

  def apply(str:String, headers: Seq[String], allOtherFeatures:Set[String], indices:Map[String, Int]):ContextPairInstance = {
    // Parse the commas into tokens
    val rowData = str.split(",")
    val sentencePos = rowData(0).toInt


    var evt_dependencyTails = new mutable.HashSet[String]
    var ctx_dependencyTails = new mutable.HashSet[String]

    allOtherFeatures foreach {
      case evt:String if evt.startsWith("evtDepTail") =>
        if(rowData(indices(evt)) != "0.0")
          evt_dependencyTails += evt.substring(11)
      case ctx:String if ctx.startsWith("ctxDepTail") =>
        if(rowData(indices(ctx)) != "0.0")
          ctx_dependencyTails += ctx.substring(11)
      case _ => ()
    }

    val pmcid = rowData(indices("PMCID"))
    val label = rowData(indices("label"))
    val evt = rowData(indices("EvtID"))
    val ctx = rowData(indices("CtxID"))

    val specificFeatureNames = collection.mutable.ListBuffer[String]()
    val listOfNumericFeatures = listOfSpecificNonDependFeatures.drop(4)
    listOfNumericFeatures.map(l => {
      specificFeatureNames += l
    })
    ContextPairInstance(sentencePos,
      pmcid,
      Some(label.toBoolean),
      evt,
      ctx,
      specificFeatureNames.toArray,
      ctx_dependencyTails.toSet,
      evt_dependencyTails.toSet)
  }


  // This fromStreamFunction accepts dataframe to be passed as an inputstream,
  // and returns an instance of Seq(ContextPairInstance), wherein each of the data point is considered a *ContextPairInstance*
  def fromStream(stream:InputStream):Seq[ContextPairInstance] = {
    val source = Source.fromInputStream(stream)
    val lines = source.getLines()
    val headers = lines.next() split ","
    val features = allOtherFeatures(headers)
    val ixs = indices(headers)
    val ret = (lines map (l => ContextPairInstance(l, headers.toSeq, features, ixs))).toList
    source.close()
    ret
  }

}
