package org.clulab.reach.context.context_utils

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, PrintWriter}

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
    println(s"The current paper uses ${filteredPairs.size} event-context pairs")

    val labelFileDir = config.getString("polarityContext.attemptDir")
    val outputPaperDir = new File(labelFileDir)
    if(!outputPaperDir.exists()) {
      outputPaperDir.mkdirs()
    }
    val labelFilePath = labelFileDir.concat("/contextLabelsWrittenToFile.txt")
    val labelFile = new File(labelFilePath)
    if (!labelFile.exists()) {
      labelFile.createNewFile()
    }
    val pw = new PrintWriter(labelFile)
    val tempo = filteredPairs.map{p =>
      pw.write(s"Paper ID: ${p._1.document.id}, Event ID := ${extractEvtId(p._1)}, Context ID := ${p._2.nsId()} \n")
      val featureExtractor = new ContextFeatureExtractor(p, ctxMentions)
      featureExtractor.extractFeaturesToCalcByBestFeatSet()
    }
    val flattenedMap = tempo.flatMap(t=>t).toMap
    val featValPath = labelFileDir.concat("/InputRowFeatureValueToFile.txt")
    val featValFile = new File(featValPath)
    if (!featValFile.exists()) {
      featValFile.createNewFile()
    }
    val printWrite = new PrintWriter(featValFile)
    for((_, (specs, ctxDep, evtDep)) <- flattenedMap) {
      for((name, value) <- specs) {
        printWrite.write(s"The feature ${name} has value ${value} \n")
      }
      for((name, value) <- ctxDep) {
        printWrite.write(s"The feature ${name} has value ${value} \n")
      }
      for((name, value) <- evtDep) {
        printWrite.write(s"The feature ${name} has value ${value} \n")
      }
    }
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

  def writeAggRowToFile(row:AggregatedContextInstance, evtID: String, ctxString:String, whereToWrite:String):Unit = {
    val file = new File(whereToWrite)
    if (!file.exists()) {
      file.createNewFile()
    }
    val pw = new PrintWriter(file)
    val zipped = row.featureGroupNames zip row.featureGroups
    for((name,value) <- zipped) {
      pw.write(s"The aggregated feature ${name} has value ${value} \n")
    }
  }

  def writeAggRowToFile(row: AggregatedContextInstance, evtID: String, ctxID: String, sentenceWindow: Int, whereToWrite: String):Unit = {
    val typeOfPaper = config.getString("polarityContext.typeOfPaper")
    val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
    val fileListUnfiltered = new File(dirForType)
    val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
    val currentPMCID = s"PMC${row.PMCID.split("_")(0)}"
    for(file <- fileList) {
      val fileNamePMCID = file.getName.slice(0,file.getName.length-5)
      //val outPaperDirPath = dirForType.concat(s"/sentenceWindows/${sentenceWindow}")
      val outPaperDirPath = whereToWrite.concat(s"/sentenceWindows/${sentenceWindow}")
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


  def createAggRowSpecsFromFile(file: File):(String, String, String) = {
    val pmcid = file.getName.split("_")(1)
    val evtID = file.getName.split("_")(2)
    val ctxID = file.getName.split("_")(3)
    val ctxID2 = ctxID.slice(0,ctxID.length-4)
    (pmcid, evtID, ctxID2)
  }

  def writeAggrRowsToFile(mentions: Seq[BioMention]):Unit = {
    val sentenceWindow = config.getString("contextEngine.params.bound")
    val aggrRows = constructAggregatedInstance(mentions, Some(sentenceWindow.toInt))
    val whereToWrite = config.getString("policy4Params.mentionsOutputFile")

    for(((evtID, ctxID), row) <- aggrRows) {
      writeAggRowToFile(row,evtID,ctxID,sentenceWindow.toInt,whereToWrite)
    }
  }

  private def constructAggregatedInstance(mentions: Seq[BioMention], sentenceWindow:Option[Int]):Seq[((String, String),AggregatedContextInstance)] = {
    val ctxMentions = mentions filter ContextEngine.isContextMention map (_.asInstanceOf[BioTextBoundMention])
    val pairGenerator = new EventContextPairGenerator(mentions, ctxMentions)
    val pairs = pairGenerator.yieldContextEventPairs()
    val filteredPairs = sentenceWindow match {
      case Some(bound) =>
        pairs.filter {
          case (evt, ctx) =>
            Math.abs(evt.sentence - ctx.sentence) <= bound
        }
      case None =>
        pairs
    }
    val lookUpTable = ContextFeatureUtils.getFeatValMapPerInput(filteredPairs.toSet, ctxMentions)
    val contextPairInput:Seq[ContextPairInstance] = ContextFeatureUtils.getCtxPairInstances(lookUpTable)


    val aggrRowsToReturn = collection.mutable.ListBuffer[((String, String),AggregatedContextInstance)]()
    val aggregatedFeatures:Map[EventID, Seq[(ContextID, AggregatedContextInstance)]] =
      (pairs zip contextPairInput).groupBy{
        case (pair, _) => extractEvtId(pair._1) // Group by their EventMention
      }.mapValues{
        v =>
          v.groupBy(r => ContextEngine.getContextKey(r._1._2)).mapValues(s =>  {
            val seqOfInputRowsToPass = s map (_._2)
            val featureAggregatorInstance = new ContextFeatureAggregator(seqOfInputRowsToPass, lookUpTable)
            val aggRow = featureAggregatorInstance.aggregateContextFeatures()
            aggRow}).toSeq
      }

    for((k,a) <- aggregatedFeatures) {
      for((ctxID, aggregatedFeature) <- a) {
        val entry = ((k.toString, ctxID._2), aggregatedFeature)
        aggrRowsToReturn += entry
      }
    }


    aggrRowsToReturn



  }


  def extractEvtId(evt:BioEventMention):EventID = {
    val sentIndex = evt.sentence
    val tokenIntervalStart = (evt.tokenInterval.start).toString()
    val tokenIntervalEnd = (evt.tokenInterval.end).toString()
    sentIndex+tokenIntervalStart+tokenIntervalEnd
  }



}
