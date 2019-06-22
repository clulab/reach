package org.clulab.reach.context.context_utils

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import com.typesafe.config.ConfigFactory
import org.clulab.context.utils.{AggregatedContextInstance, ContextPairInstance}
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}

object ContextFeatureUtils {
  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  val config = ConfigFactory.load()

  // :input  :- seq(filteredPair), seq(contextMentions)
  // using the inputs, this function calls the feature extractor, and receives a seq(map). To simplify this data structure, we will flatten the output to a simple map.
  // :output :- map of ContextPairInstance -> (feature_name -> feature_value)
  def getFeatValMapPerInput(filteredPairs: Seq[Pair], ctxMentions: Seq[BioTextBoundMention]):Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])] = {
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

  def writeRowToFile(row:AggregatedContextInstance, evtID: String, ctxID: String):Unit = {
    val typeOfPaper = config.getString("svmContext.paperType")
    val dirForType = if(typeOfPaper.length != 0) config.getString("papersDir").concat(s"/${typeOfPaper}") else config.getString("papersDir")
    val fileListUnfiltered = new File(dirForType)
    val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
    val currentPMCID = s"PMC${row.PMCID.split("_")(0)}"
    for(file <- fileList) {
      val fileNamePMCID = file.getName.slice(0,file.getName.length-5)
      val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${fileNamePMCID}")
      // creating output directory if it doesn't already exist
      val outputPaperDir = new File(outPaperDirPath)
      if(!outputPaperDir.exists()) {
        outputPaperDir.mkdirs()
      }

      if(currentPMCID == fileNamePMCID) {
        val pathForRow = outPaperDirPath.concat(s"/AggregatedRow_${currentPMCID}_${evtID}_${ctxID}.txt")
        val sentenceFile = new File(pathForRow)
        if (!sentenceFile.exists()) {
          sentenceFile.createNewFile()
        }
        val os = new ObjectOutputStream(new FileOutputStream(pathForRow))

        os.writeObject(row)
        os.close()
      }
    }
  }


  def writeRowToFile(row:AggregatedContextInstance, evtID: String, ctxID: String, sentenceWindow:Int):Unit = {
    val typeOfPaper = config.getString("polarityContext.typeOfPaper")
    val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
    val fileListUnfiltered = new File(dirForType)
    val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
    val currentPMCID = s"PMC${row.PMCID.split("_")(0)}"
    for(file <- fileList) {
      val fileNamePMCID = file.getName.slice(0,file.getName.length-5)
      val outPaperDirPath = dirForType.concat(s"/sentenceWindows/${sentenceWindow}")
      // creating output directory if it doesn't already exist
      val outputPaperDir = new File(outPaperDirPath)
      if(!outputPaperDir.exists()) {
        outputPaperDir.mkdirs()
      }

      if(currentPMCID == fileNamePMCID) {
        val pathForRow = outPaperDirPath.concat(s"/AggregatedRow_${currentPMCID}_${evtID}_${ctxID}.txt")
        val sentenceFile = new File(pathForRow)
        if (!sentenceFile.exists()) {
          sentenceFile.createNewFile()
        }
        val os = new ObjectOutputStream(new FileOutputStream(pathForRow))

        os.writeObject(row)
        os.close()
      }
    }
  }

  def readAggRowFromFile(file: String):AggregatedContextInstance = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val c = is.readObject().asInstanceOf[AggregatedContextInstance]
    is.close()
    c
  }



}
