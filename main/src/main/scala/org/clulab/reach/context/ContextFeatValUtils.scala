package org.clulab.reach.context

import java.io.{File, FileOutputStream, ObjectOutputStream}

import com.typesafe.config.ConfigFactory
import org.clulab.context.utils.{AggregatedContextInstance, ContextPairInstance}
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}

object ContextFeatValUtils {
  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  val config = ConfigFactory.load()
  def getFeatValMapPerInput(filteredPairs: Seq[Pair], ctxMentions: Seq[BioTextBoundMention]):Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])] = {
    val tempo = filteredPairs.map{p =>
      val featureExtractor = new ContextFeatureExtractor(p, ctxMentions)
      featureExtractor.extractFeaturesToCalcByBestFeatSet()
    }
    val flattenedMap = tempo.flatMap(t=>t).toMap
    flattenedMap

    //filteredMaps.flatMap(t=>t).toMap


  }
  //Seq[Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])]]



  def getCtxPairInstances(ctxPairFeatValMap: Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])]): Seq[ContextPairInstance] = {
   /* val tempo = filteredPairs.map{p =>
      val featureExtractor = new ContextFeatureExtractor(p, ctxMentions)
      featureExtractor.extractFeaturesToCalcByBestFeatSet()
    }
    tempo.flatMap(t => t.keySet)*/

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

}
