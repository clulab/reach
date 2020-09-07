package org.clulab.reach.assembly


import org.json4s.jackson.JsonMethods._
import org.json4s._

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.{ReachSystem, context}

import ai.lum.common.FileUtils._
import java.nio.charset.StandardCharsets.UTF_8
import org.clulab.reach.mentions.serialization.json._

import org.clulab.reach.mentions.BioMention


object RegenerateMention extends App {

  /*
  All of the aim of this function is to replace the old event pairs text with the new event pairs text. To do this, we need to
  (1) load the old event pairs, see what documents we need to reprocess.
  (2) reprocess the documents using reach and save the mentions in json.
  (3) replace the text in the old event pairs with the reprocessed text.
   */
  implicit val formats = DefaultFormats

  val config = ConfigFactory.load()
  val corpusDirOldTrain = config.getString("assembly.corpus.corpusDirOldTrain")
  val corpusDirOldEval = config.getString("assembly.corpus.corpusDirOldEval")
  //val jsonDirOldTrain = config.getString("assembly.corpus.jsonDirOldTrain")
  //val jsonDirOldEval = config.getString("assembly.corpus.jsonDirOldEval")

  val corpusDirNewTrain = config.getString("assembly.corpus.corpusDirNewTrain")
  val corpusDirNewEval = config.getString("assembly.corpus.corpusDirNewEval")
  //val jsonDirNewTrain = config.getString("assembly.corpus.jsonDirNewTrain")
  //val jsonDirNewEval = config.getString("assembly.corpus.jsonDirNewEval")

  val procAnnotator = new BioNLPProcessor()
  lazy val reachSystem = new ReachSystem(
    processorAnnotator = Some(procAnnotator)
  )


  private case class AssemblyAnnotation(
                                 id: Int,
                                 text: String,
                                 relation: String,
                                 `annotator-id`: String,
                                 coref: Boolean,
                                 `e1-id`: String,
                                 `e2-id`: String,
                                 confidence: Double,
                                 `paper-id`: String,
                                 notes: Option[String]
                               )

  private case class PMIDAnnotation(`paper-id`: String) //used to get the paper ID from the event pairs json.

  // This function reads the unique paper IDs of the event pairs.
  def readPMIDsFromOldEventPairs(corpusDirOld:String):Seq[String] = {
    val epsJAST = parse(new File(corpusDirOld, s"event-pairs.json"))
    val allPMIDs = for {aa <- epsJAST.extract[Seq[PMIDAnnotation]]} yield aa.`paper-id`

    allPMIDs.distinct
  }

  def readDocumentTextByPMID(corpusDirOld:String):Map[String, String] = {
    val docDataDir = corpusDirOld+"/mention-data/"
    val pmids = readPMIDsFromOldEventPairs(corpusDirOld)

    var numHit = 0
    var numMiss = 0
    val docTextMap = scala.collection.mutable.Map[String, String]()
    for (pmid <- pmids) {
      val docFileName = docDataDir+pmid.toString+"-mention-data.json"
      if (new File(docFileName).exists()){
        numHit+=1
        // TODO: not sure there is a better choice to parse the document json
        val documentObjRaw = parse(new File(docFileName)).extract[Map[String, Any]]
        val documentObj = documentObjRaw("documents").asInstanceOf[Map[String, Any]]
        val paperIDInFile = documentObj.keys.head.toString

        val documentText = documentObj(paperIDInFile).asInstanceOf[Map[String, String]]("text")

        docTextMap(pmid.toString) = documentText.toString
      }
      else {numMiss+=1}
    }
    println(s"Num paper matched: ${numHit}, Num paper missed: ${numMiss}")
    docTextMap.toMap
  }

  // This method is from Gus's assmebly project, but it has problems writing to the file. So I comment this method for now.
//  def writeJSON(dmLUT: Map[String, Seq[BioMention]], corpusDir: String, pretty:Boolean): Unit = {
//
//    val mentionDataDir = new File(new File(corpusDir), "mention-data")
//    // create data dir
//    if (! mentionDataDir.exists) forceMkdir(mentionDataDir)
//    // for each doc, write doc + mentions to a json file
//    for ((paperID, cms) <- dmLUT) {
//      val of = new File(mentionDataDir, s"$paperID-mention-data.json")
//      of.writeString(cms.json(pretty), java.nio.charset.StandardCharsets.UTF_8)
//    }
//
//  }

  def writeMentionsToJsons(corpusDir:String, pmid:String, mentions:Seq[BioMention]):Unit = {
    val mentionDataDir = new File(new File(corpusDir), "mention-data")

    val json = mentions.json(true)
    // assumes you've specified/defined an outDir: File
    val f = new File(mentionDataDir, s"${mentions.head.document.id.get}-mention-data.json")
    f.writeString(string = json, charset = UTF_8, append = false, gzipSupport = false)
  }

  def annotateDocumentsAndSaveOutput(corpusDirOld:String, corpusDirNew:String):Unit = {
    val docTextMap = readDocumentTextByPMID(corpusDirOld)

//    val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
//    val contextConfig = config.getConfig("contextEngine.params").root
//    val contextEngineParams: Map[String, String] =
//      context.createContextEngineParams(contextConfig)

    // initialize ReachSystem

    val allDocIDs = docTextMap.keys.toSeq

    println(allDocIDs)

    val paperMentionsMap = scala.collection.mutable.Map[String, Seq[BioMention]]()
    var nDone = 0
    var nSkipped = 0
    for (pmid <- allDocIDs){
      println("processing paper ",pmid)
      try {
        val docText = docTextMap(pmid.toString)
        val mentions = reachSystem.extractFrom(docText.toString, pmid.toString, "")
        writeMentionsToJsons(corpusDirNew, pmid.toString, mentions)
        nDone+=1
      }
      catch{
        case _ => {
          nSkipped+=1
          println("\tpaper skipped")
        }

      }
    }
    println(s"processing done! N done: ${nDone}, N skipped: ${nSkipped}")

    //writeJSON(paperMentionsMap.toMap, corpusDirNew, false) //what does this pretty do?
  }

  annotateDocumentsAndSaveOutput(corpusDirOldTrain, corpusDirNewTrain)
  annotateDocumentsAndSaveOutput(corpusDirOldEval, corpusDirNewEval)

}
