package org.clulab.reach.context.context_utils

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, PrintWriter}
import org.apache.commons.io.{FilenameUtils}
import com.typesafe.config.ConfigFactory
import org.clulab.context.utils.{AggregatedContextInstance, ContextPairInstance}
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}

object ContextFeatureUtils {
  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  val config = ConfigFactory.load()

  // :input  :- seq(filteredPair), seq(contextMentions)
  // using the inputs, this function calls the feature extractor, and receives a seq(map). To simplify this data structure, we will flatten the output to a simple map.
  // :output :- map of ContextPairInstance -> (feature_name -> feature_value)
  def getFeatValMapPerInput(filteredPairs: Set[Pair], ctxMentions: Seq[BioTextBoundMention]):Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])] = {

    val tempo = filteredPairs.map{p =>
      val featureExtractor = new ContextFeatureExtractor(p, ctxMentions)
      featureExtractor.extractFeaturesToCalcByBestFeatSet()
    }
    val flattenedMap = tempo.flatMap(t=>t).toMap
    flattenedMap
  }

  // getCtxPairInstances takes a map of ContextPairInstance and their corresponding feature values, and returns the keyset, i.e. set[ContextPairInstance]
  def getCtxPairInstances(ctxPairFeatValMap: Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])]): Seq[ContextPairInstance] = {
    ctxPairFeatValMap.keySet.toSeq
  }



  // the following few functions with the name writeAggRowToFile offer different signatures to the function,
  // such that the aggregated rows may be written to file for further analyses


  // This signature of writeAggRowToFile writes the AggregatedRow object to file whose path is specified by parentDir.
  // The function also specifies the event and context IDs in the name of the file.
  def writeAggRowToFile(row:AggregatedContextInstance, evtID: String, ctxString:String, parentDir:String):Unit = {
    val pmcid = s"PMC${row.PMCID.split("_")(0)}"
    val whichDirToWriteRow = parentDir.concat(s"${pmcid}")
    val paperDir = new File(whichDirToWriteRow)
    if(!paperDir.exists())
      paperDir.mkdirs()
    val whereToWriteRow = whichDirToWriteRow.concat(s"/AggregatedRow_${pmcid}_${evtID}_${ctxString}.txt")
    val file2 = new File(whereToWriteRow)
    val os = new ObjectOutputStream(new FileOutputStream(whereToWriteRow))
    if (!file2.exists()) {
      file2.createNewFile()
    }
    os.writeObject(row)
    os.close()
  }




  // This following signature of writeAggRowToFile takes the aggregated row, event ID, context ID, sentence window and directory into which the file is to be written is to be written.
  // In this directory, a sub-directory called "sentencewindows/$value_of_sent_window" will be created, and rows will be written to the directory.
  def writeAggRowToFile(row: AggregatedContextInstance, evtID: String, ctxID: String, sentenceWindow: Int, parentDir:String):Unit = {

    val outDir = parentDir.concat(s"/sentenceWindows/${sentenceWindow}")
    val outdirFile = new File(outDir)
    if(!outdirFile.exists())
      outdirFile.mkdirs()
    val currentPMCID = s"PMC${row.PMCID.split("_")(0)}"
    val aggrRowFilePath = outDir.concat(s"/AggregatedRow_${currentPMCID}_${evtID}_${ctxID}.txt")
    val aggrRowFile = new File(aggrRowFilePath)
    if(!aggrRowFile.exists())
      aggrRowFile.createNewFile()
    val os = new ObjectOutputStream(new FileOutputStream(aggrRowFilePath))
    os.writeObject(row)
    os.close()

  }

  def readAggRowFromFile(file: String):AggregatedContextInstance = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val c = is.readObject().asInstanceOf[AggregatedContextInstance]
    is.close()
    c
  }

  def readAggRowsFromFile(file: String): Array[((String, String),AggregatedContextInstance)] = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val c = is.readObject().asInstanceOf[Array[((String, String),AggregatedContextInstance)]]
    is.close()
    c
  }


  def createAggRowSpecsFromFile(file: File):(String, String, String) = {
    val strOnly = FilenameUtils.removeExtension(file.getName)
    val pmcid = strOnly.split("_")(1)
    val evtID = strOnly.split("_")(2)
    val ctxID = strOnly.split("_")(3)
    var rem = ""
    if(strOnly.split("_").size > 4) {
      rem = "_".concat(strOnly.split("_")(4))
    }
    val ctxID2 = ctxID.concat(rem)
    val toReturn = (pmcid, evtID, ctxID2)
    toReturn
  }



  // The function extractEventID takes an event mention as the sole parameter,
  // and returns a string containing information about the sentence index, token start and token end of the event interval.
  // The eventId will be returned in the format "in${sentenceIndex}from${event_start_token}to${event_end_token}
  // For example, if a given BioEventMention has the sentence index 3, and token start and end values of 7 and 8,
  // the function will return a string that reads "in3from7to8"
  // This format will be used across the SVM engine.

  def extractEvtId(evt:BioEventMention):EventID = {
    val sentIndex = evt.sentence
    val tokenIntervalStart = (evt.tokenInterval.start).toString()
    val tokenIntervalEnd = (evt.tokenInterval.end).toString()
    "in"+sentIndex+"from"+tokenIntervalStart+"to"+tokenIntervalEnd
  }



}
