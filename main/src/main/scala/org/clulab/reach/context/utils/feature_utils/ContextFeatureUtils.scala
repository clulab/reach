package org.clulab.reach.context.feature_utils

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
    val labelFilePath = config.getString("svmContext.outputDirForAnnotations")
    val allContextPairsFilePath = labelFilePath.concat("/AllContextPairs.txt")
    val allPairsFile = new File(allContextPairsFilePath)
    if(!allPairsFile.exists())
      allPairsFile.createNewFile()
    val pw = new PrintWriter(allPairsFile)
    val tempo = filteredPairs.map{p =>
      val currentPaperID = p._1.document.id match {
        case Some(x) => s"PMC${x.split("_")(0)}"
        case None => "unknown_paper_id"
      }
      pw.append(s"Paper ID: ${currentPaperID}, Event ID := ${extractEvtId(p._1)}, Context ID := ${p._2.nsId()} \n")

      val featureExtractor = new ContextFeatureExtractor(p, ctxMentions)
      featureExtractor.extractFeaturesToCalcByBestFeatSet()
    }
    pw.close()
    val flattenedMap = tempo.flatMap(t=>t).toMap
    println(s"The number of pairs we are operating on is: ${filteredPairs.size} \n")
    println(s"The number of input rows we have is ${flattenedMap.size}")
    flattenedMap
  }

  // getCtxPairInstances takes a map of ContextPairInstance and their corresponding feature values, and returns the keyset, i.e. set[ContextPairInstance]
  def getCtxPairInstances(ctxPairFeatValMap: Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])]): Seq[ContextPairInstance] = {
    ctxPairFeatValMap.keySet.toSeq
  }

  def writeAggRowToFile(row:AggregatedContextInstance, evtID: String, ctxID: String):Unit = {
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

  def writeAggRowToFile(row:AggregatedContextInstance, evtID: String, ctxString:String, whereToWriteFeatVal:String, whereToWriteRow:String):Unit = {
    val file = new File(whereToWriteFeatVal)
    if (!file.exists()) {
      file.createNewFile()
    }
    val pw = new PrintWriter(file)
    val zipped = row.featureGroupNames zip row.featureGroups
    for((name,value) <- zipped) {
      pw.write(s"The aggregated feature ${name} has value ${value} \n")
    }

    val file2 = new File(whereToWriteRow)
    val os = new ObjectOutputStream(new FileOutputStream(whereToWriteRow))
    if (!file2.exists()) {
      file2.createNewFile()
    }
    os.writeObject(row)
    os.close()
  }

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


  def writeAggrRowsToFile(rows:Array[((String, String),AggregatedContextInstance)], pathToFeatVal:String, pathToArrOfRows:String):Unit = {

    val arrOfRowsFile = new File(pathToArrOfRows)
    if(!(arrOfRowsFile.exists()))
      arrOfRowsFile.createNewFile()
    val os = new ObjectOutputStream(new FileOutputStream(pathToArrOfRows))
    os.writeObject(rows)
    val featValFile = new File(pathToFeatVal)
    if(!(featValFile.exists()))
      featValFile.createNewFile()
    val printWriter = new PrintWriter(pathToFeatVal)
    for(((eventID, ctxID), row) <- rows) {
      val zip = row.featureGroupNames zip row.featureGroups
      for((featName, featVal) <- zip) {
        printWriter.write(s"The eventID ${eventID} and contextID ${ctxID} have feature ${featName} value of ${featVal}\n")
      }
    }
    os.close()
  }


  //takes the aggregated row, event ID, context ID, sentence window and directory in which output is to be written. In this directory, a sub-directory called "sentencewindows/value_of_sent_window" will be created, and rows will be written to the directory.
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

  def readAggRowFromFilePath(pathToFile: String):AggregatedContextInstance = {
    val is = new ObjectInputStream(new FileInputStream(pathToFile))
    val c = is.readObject().asInstanceOf[AggregatedContextInstance]
    is.close()
    c
  }


  def readAggRowFromFile(file: File):AggregatedContextInstance = {
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



  def extractEvtId(evt:BioEventMention):EventID = {
    val sentIndex = evt.sentence
    val tokenIntervalStart = (evt.tokenInterval.start).toString()
    val tokenIntervalEnd = (evt.tokenInterval.end).toString()
    "in"+sentIndex+"from"+tokenIntervalStart+"to"+tokenIntervalEnd
  }



}
