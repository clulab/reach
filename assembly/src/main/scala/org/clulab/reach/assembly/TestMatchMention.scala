package org.clulab.reach.assembly

import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}
import org.clulab.reach.{ReachSystem, mentions}
import org.json4s.JsonAST.JValue

import scala.collection.mutable.ArrayBuffer

import org.clulab.reach.mentions.serialization.json.{JSONSerializer, MentionJSONOps, REACHMentionSeq}

import scala.util.hashing.MurmurHash3._
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils.forceMkdir

import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.assembly.relations.corpus._

import scala.collection.parallel.ForkJoinTaskSupport

import java.io.File


import org.clulab.serialization.json.stringify
import org.clulab.serialization.json.JSONSerialization
import org.json4s.JsonDSL._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import org.json4s._
import ai.lum.common.FileUtils._
import java.nio.charset.StandardCharsets.UTF_8

/**
  * This is to annotate the mentions of the papers using the latest reach. This is needed because the old reach uses
  * Different grammars as the new reach, therefore the mentions are in different formats.
  *
  * It first reads the papers and mention from the old event pairs, then annotate the document using the new reach.
  * Finally, it saves the mentions (but not the event pairs) to the mcc_intermediate folder.
  *
  * Contribution by Zhengzhong.
  */

object SerializePapersToJSONByMentionData extends App with LazyLogging{
  implicit val formats = DefaultFormats

  val config = ConfigFactory.load()
  val corpusDirOldTrain = config.getString("assembly.corpus.corpusDirOldTrain")
  val corpusDirOldEval = config.getString("assembly.corpus.corpusDirOldEval")
  //val jsonDirOldTrain = config.getString("assembly.corpus.jsonDirOldTrain")
  //val jsonDirOldEval = config.getString("assembly.corpus.jsonDirOldEval")

  val corpusDirInterTrain = config.getString("assembly.corpus.corpusDirInterTrain")
  val corpusDirInterEval = config.getString("assembly.corpus.corpusDirInterEval")
  //val jsonDirNewTrain = config.getString("assembly.corpus.jsonDirNewTrain")
  //val jsonDirNewEval = config.getString("assembly.corpus.jsonDirNewEval")

  val threadLimit = config.getInt("threadLimit")

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

  def annotateDocumentsAndSaveOutput(corpusDirOld:String, corpusDirNew:String):Unit = {
    val docTextMap = readDocumentTextByPMID(corpusDirOld)

    //    val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
    //    val contextConfig = config.getConfig("contextEngine.params").root
    //    val contextEngineParams: Map[String, String] =
    //      context.createContextEngineParams(contextConfig)
    // initialize ReachSystem

    val allPapers = docTextMap.toSeq.par // allDocs is a Seq of tuple ((id, text), (id, text), (id, text), ...)
    allPapers.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))

    var nDone = 0
    var nSkipped = 0
    for ((paperID, paperText) <- allPapers){
      logger.info(s"processing paper $paperID")
      try {
        val outFile = new File(s"$corpusDirNew/mention-data/$paperID-mention-data.json")

        val mentions = reachSystem.extractFrom(paperText, paperID, "")
        val cms = mentions.map(_.toCorefMention)
        cms.saveJSON(outFile, pretty = false)
        logger.info(s"\tsaved json to $outFile")
        nDone+=1
      }
      catch{
        case _ => {
          nSkipped+=1
          logger.info("\tpaper skipped")
        }

      }
    }
    logger.info(s"processing done! N done: ${nDone}, N skipped: ${nSkipped}")

    //writeJSON(paperMentionsMap.toMap, corpusDirNew, false) //what does this pretty do?
  }

  annotateDocumentsAndSaveOutput(corpusDirOldTrain, corpusDirInterTrain)
  annotateDocumentsAndSaveOutput(corpusDirOldEval, corpusDirInterEval)
}

/**
  * This script is to test the functionality of the alignment function. The alignment function is used to match the
  * mention annotated by the old reach with the mention annotated by the new reach.
  *
  * The [matchEventWithinASentence] and [findEventSentenceIndex] methods are used in the Corpus.softAlign method in the
  * formal alignment process.
  *
  * Contribution by Zhengzhong
  */

object TestMatchMention extends App {
  val config = ConfigFactory.load()
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirOldEval")).instances


  val newMentions = Corpus.loadMentions(config.getString("assembly.corpus.corpusDirInterEval"))

  var n_pairs_mention_match = 0
  var n_pairs_mention_not_match = 0

  var n_mention_exact_match = 0
  var n_mention_soft_match = 0

  //val X = Corpus.softAlign(eps, newMentions)
  softAlginPrototype(eps, newMentions)

  def softAlginPrototype(eps:Seq[EventPair], newMentions: Map[String, Seq[mentions.CorefMention]]):Unit = {
    var n_pairs_due_to_missing_paper = 0
    var n_pairs_due_to_missing_sentence = 0

    for (ep <- eps){
      val docID = ep.e1.document.id.get.split("_")(0)
      if (newMentions.contains(docID)){
        //println("="*20)

        val e1MatchedSentenceIndex = findEventSentenceIndex(ep.e1, newMentions(docID))
        val e2MatchedSentenceIndex = findEventSentenceIndex(ep.e2, newMentions(docID))

        if (e2MatchedSentenceIndex-e1MatchedSentenceIndex != ep.e2.sentence-ep.e1.sentence){
          println("sentence matching problem!")
        }

        val e1MatchedMention = matchEventWithinASentence(ep.e1, newMentions(docID).filter(x => x.sentence==e1MatchedSentenceIndex))
        val e2MatchedMention = matchEventWithinASentence(ep.e2, newMentions(docID).filter(x => x.sentence==e2MatchedSentenceIndex))

        //Corpus.debugPrintMentionAttributes(ep, e1MatchedMention, e2MatchedMention)

        if (e1MatchedMention.isDefined && e2MatchedMention.isDefined){
          n_pairs_mention_match+=1
        }
        else{
          n_pairs_mention_not_match+=1
        }
      }
      else {
        n_pairs_due_to_missing_paper+=1
      }

    }
    println(s"n pairs due to missing paper: ${n_pairs_due_to_missing_paper}")
    println(s"n pairs mention match: ${n_pairs_mention_match}")
    println(s"n pairs mention not match: ${n_pairs_mention_not_match}")
    println(s"n mention exact match ${n_mention_exact_match}")
    println(s"n mention soft match ${n_mention_soft_match}")
    //println(s"n pairs due to unmatched sentence: ${n_pairs_due_to_missing_sentence}")
  }

  def matchEventWithinASentence(originalMention:mentions.CorefMention, candidateMentionsFromOneSentence:Seq[mentions.CorefMention]):Option[mentions.CorefMention] = {
    // Think about this: how to use different criterions: mention text, mention boundary, mention label, mention arguments.
    // Should we use these criterion in a sequential manner or in a parallel manner?

    // 1, if the mention text and the boundary are exactly the same, return it.
    val exactMatchResult = candidateMentionsFromOneSentence.find(x => x.text==originalMention.text && x.start==originalMention.start && x.end==originalMention.end)
    if (exactMatchResult.isDefined){
      n_mention_exact_match+=1

      //println("-"*20)
      //println("exact match")
      //debugPrintBestMatchedCandidate(originalMention, exactMatchResult.get)

      exactMatchResult
    }
    else{
      //println("-"*20)

      // 2, if the mention text or boundary are not exactly the same, compute these scores for each candidate mention:
      // 2.1 mention text edit distance; 2.2, boundary difference; 2.3, label jacard distance; 2.4, controller and controlled
      val allMentionScores = ArrayBuffer[Float]()
      for (m <- candidateMentionsFromOneSentence){
        val mentionTextDistance = math.min(editDistance(originalMention.text, m.text).toFloat/m.text.length.toFloat, 1.0f)
        val mentionBoundDistance = math.min(((originalMention.start-m.start).abs.toFloat+(originalMention.end-m.end).abs.toFloat)/(originalMention.end-originalMention.start).toFloat, 1.0f)
        val labelDistance = {
          val originalLabelsSet = originalMention.labels.toSet
          val candidateLabelsSet = m.labels.toSet
          1.0f-originalLabelsSet.intersect(candidateLabelsSet).size.toFloat/originalLabelsSet.size.toFloat
        }

//        println(s"text ${originalMention.text} ||| ${m.text}")
//        println(s"bound: (${originalMention.start}, ${originalMention.end}), (${m.start}, ${m.end})")
//        println("labels:", originalMention.labels, m.labels)
//        println(s"scores: ${mentionTextDistance}, ${mentionBoundDistance}, ${labelDistance}")
//        scala.io.StdIn.readLine()

        allMentionScores.append(mentionTextDistance+mentionBoundDistance+labelDistance)
      }

      val bestMatchedMention = candidateMentionsFromOneSentence(allMentionScores.indexOf(allMentionScores.min))

      if (allMentionScores.min<=0.6) { // I think this hyper parameter is reasonable.
        n_mention_soft_match+=1

        //println("soft matched distance")
        //debugPrintBestMatchedCandidate(originalMention, bestMatchedMention)

        Some(bestMatchedMention)
      }

      else {
        None
        //if (bestMatchedMention.text.contains(originalMention.text)) {

//        if (bestMatchedMention.text.contains(originalMention.text) || originalMention.text.contains(bestMatchedMention.text)) {
//          n_mention_soft_match+=1
//
//          println("soft matched containment")
//          debugPrintBestMatchedCandidate(originalMention, bestMatchedMention)
//
//          Some(bestMatchedMention)
//        }
//        else{
//
//          println("not matched")
//          debugPrintBestMatchedCandidate(originalMention, bestMatchedMention)
//
//          None
//        }

      }

    }
  }

  def findEventSentenceIndex(originalMention:mentions.CorefMention, candidateMentions:Seq[mentions.CorefMention]):Int = {
    val matchedMentionOpt = candidateMentions.find(x => x.sentenceObj.words.mkString("")==originalMention.sentenceObj.words.mkString(""))
    val matchedSentenceIndex = {
      if (matchedMentionOpt.isDefined){
        matchedMentionOpt.get.sentence
      }
      else{
        val sentenceEditDistance = candidateMentions.map{x => editDistance(originalMention.sentenceObj.words.mkString(" "), x.sentenceObj.words.mkString(" "))}
        candidateMentions(sentenceEditDistance.indexOf(sentenceEditDistance.min)).sentence
      }
    }

    matchedSentenceIndex
  }

  def debugFindSentence(ep:EventPair, candidateMentions:Seq[mentions.CorefMention]):Boolean = {
    val matchingResultE1 = candidateMentions.map{x => if (ep.e1.sentenceObj.words.mkString("") == x.sentenceObj.words.mkString("")) {x.sentence} else -1}.toSet
    val matchingResultE2 = candidateMentions.map{x => if (ep.e2.sentenceObj.words.mkString("") == x.sentenceObj.words.mkString("")) {x.sentence} else -1}.toSet
    println(s"matched results: ${matchingResultE1}, ${matchingResultE2}")
    if (matchingResultE1.size==2 && matchingResultE2.size==2){
      true
    }
    else{
      // print the sentences without exact match:
      println("="*20)
      if (matchingResultE1.size!=2){
        println("e1 characteristics:")
        println(s"original sentence:${ep.e1.sentenceObj.words.mkString(" ")}")
        val sentenceEditDistance = candidateMentions.map{x => editDistance(ep.e1.sentenceObj.words.mkString(" "), x.sentenceObj.words.mkString(" "))}
        val bestMatchedSentence = candidateMentions(sentenceEditDistance.indexOf(sentenceEditDistance.min)).sentenceObj.words.mkString(" ")
        println(s"best match sentence:${bestMatchedSentence}")
      }
      println("-"*20)
      if (matchingResultE2.size!=2){
        println("e2 characteristics:")
        println(s"original sentence:${ep.e2.sentenceObj.words.mkString(" ")}")
        val sentenceEditDistance = candidateMentions.map{x => editDistance(ep.e2.sentenceObj.words.mkString(" "), x.sentenceObj.words.mkString(" "))}
        val bestMatchedSentence = candidateMentions(sentenceEditDistance.indexOf(sentenceEditDistance.min)).sentenceObj.words.mkString(" ")
        println(s"best match sentence:${bestMatchedSentence}")
      }


      false
    }

  }

  private def editDistance(textSeq1: Seq[Char], textSeq2: Seq[Char]):Int = {
    //This is found from here: https://www.reddit.com/r/scala/comments/7sqtyf/scala_edit_distance_implementation/
    // Use simple text cases to verify it:
    // println(editDistance(Seq("I", "have","a","dream"), Seq("I", "have","a", "good", "dream")))
    // println(editDistance(Seq("I", "have","a","dream"), Seq("I", "have","a", "very", "good", "dream")))
    val startRow = (0 to textSeq2.size).toList
    textSeq1.foldLeft(startRow) { (prevRow, aElem) =>
      (prevRow.zip(prevRow.tail).zip(textSeq2)).scanLeft(prevRow.head + 1) {
        case (left, ((diag, up), bElem)) => {
          val aGapScore = up + 1
          val bGapScore = left + 1
          val matchScore = diag + (if (aElem == bElem) 0 else 1)
          List(aGapScore, bGapScore, matchScore).min
        }
      }
    }.last
  }


  private def debugPrintBestMatchedCandidate(originalMention:mentions.CorefMention, bestMatchedMention:mentions.CorefMention):Unit = {
    println("="*20)
    println("e1 characteristics")
    println(s"\toriginal mention text: ${originalMention.text}, mention bound: (${originalMention.start},${originalMention.end})")
    println(s"\te1 sent idx: ${originalMention.sentence}, sent words: ${originalMention.sentenceObj.words.toSeq}")
    println(s"\te1 labels:${originalMention.labels}") // Trigger is not printed because the function to print trigger is a little problematic.
    println(s"\te1 arguments")
    originalMention.arguments.toSeq.foreach{x=>println(s"\t\t(${x._1},${x._2.head.text})")}
    println(s"\te1 modifications")
    originalMention.modifications.foreach{x=> println(s"\t\t${x.label}")}
    println("\n")

    println(s"\tmatched text: ${bestMatchedMention.text}, mention bound: (${bestMatchedMention.start},${bestMatchedMention.end})")
    println(s"\tmatched sent idx: ${bestMatchedMention.sentence}, sent words: ${bestMatchedMention.sentenceObj.words.toSeq}")
    println(s"\tmatched labels:${bestMatchedMention.labels}")
    println(s"\tmatched arguments")
    bestMatchedMention.arguments.toSeq.foreach{x=>println(s"\t\t(${x._1},${x._2.head.text})")}
    println(s"\tmatched modifications")
    bestMatchedMention.modifications.foreach{x=> println(s"\t\t${x.label}")}

    //scala.io.StdIn.readLine()

  }
}


/**
  * This function is used to write the event pairs for the python neural models.
  * It first read the old event-pairs.json file, then get the new corresponding mentions for each event pair.
  * Finally it writes the new event pairs.
  *
  * It does not save the new corresponding mentions.
  *
  * Contribution by Zhengzhong
  */

object WriteUpdatedPairForPython extends App {


  val config = ConfigFactory.load()

  val oldDirTrain  = config.getString("assembly.corpus.corpusDirOldTrain")
  val interDirTrain  = config.getString("assembly.corpus.corpusDirInterTrain")
  val newDirTrain = config.getString("assembly.corpus.corpusDirNewTrain")

  val oldDirEval  = config.getString("assembly.corpus.corpusDirOldEval")
  val interDirEval  = config.getString("assembly.corpus.corpusDirInterEval")
  val newDirEval = config.getString("assembly.corpus.corpusDirNewEval")

  writeUpdatedPair(oldDirTrain, interDirTrain, newDirTrain)
  writeUpdatedPair(oldDirEval, interDirEval, newDirEval)


  /**
    * @param oldDir: where the old mentions and event pairs are saved.
    * @param interDir: where the newly extracted mentions are saved.
    * @param newDir: where the new event-pairs.json and the matched mention-data are saved.
    */

  def writeUpdatedPair(oldDir:String, interDir:String, newDir:String):Unit = {

    val epsOld: Seq[EventPair] = CorpusReader.readCorpus(oldDir).instances
    val newMentions = Corpus.loadMentions(interDir)
    val eps = Corpus.softAlign(epsOld, newMentions)

    val epsJAST = eps.map{x =>

      val e1SentIdx = x.e1.sentence
      val e2SentIdx = x.e2.sentence

      val e1SentEntities = x.e1.sentenceObj.entities.get.toList
      val e2SentEntities = x.e2.sentenceObj.entities.get.toList

      if ((e1SentIdx-e2SentIdx).abs<=1){
        jsonAST(x, List.empty, e1SentEntities, e2SentEntities, List.empty)
      }
      else{
        val interSentTokenSeq = {
          if(e1SentIdx<e2SentIdx)
            {(e1SentIdx+1 until e2SentIdx).map(idx => x.e1.document.sentences(idx).words.toList)}
          else
            {(e2SentIdx+1 until e1SentIdx).map(idx => x.e1.document.sentences(idx).words.toList)}
        }

        // Get intersentence entities.
        val interSentEntities = {
          if(e1SentIdx<e2SentIdx)
          {(e1SentIdx until e2SentIdx).map(idx => x.e1.document.sentences(idx).entities.get.toList)}.toList
          else
          {(e2SentIdx until e1SentIdx).map(idx => x.e1.document.sentences(idx).entities.get.toList)}.toList
        }

        jsonAST(x, interSentTokenSeq.toList, e1SentEntities, e2SentEntities, interSentEntities)
      }
    }

    val epsJsonOutF = new File(newDir, s"event-pairs-python.json")
    epsJsonOutF.writeString(stringify(epsJAST, pretty = true), java.nio.charset.StandardCharsets.UTF_8)  }

  private def jsonAST(eventPair:EventPair, interSentences:List[List[String]],
                      e1SentEntities:List[String], e2SentEntities:List[String],
                      interSentEntities:List[List[String]]):JValue = {
    ("id" -> eventPair.equivalenceHash) ~
      ("text" -> eventPair.text) ~
      ("coref" -> eventPair.coref) ~
      // event 1
      ("e1-id" -> eventPair.e1.id) ~
      ("e1-label" -> eventPair.e1.eventLabel) ~
      ("e1-sentence-text" -> eventPair.e1.sentenceText) ~
      ("e1-sentence-index" -> eventPair.e1.sentence) ~
      ("e1-sentence-tokens" -> eventPair.e1.sentenceObj.words.toList) ~
      // can be used to highlight event span in annotation UI
      ("e1-start" -> eventPair.e1.start) ~
      ("e1-end" -> eventPair.e1.end) ~
      ("e1-trigger" -> eventPair.e1.trigger.text) ~
      ("e1-trigger-start" -> eventPair.e1.trigger.start) ~
      ("e1-trigger-end" -> eventPair.e1.trigger.end) ~
      // event 2
      ("e2-id" -> eventPair.e2.id) ~
      ("e2-label" -> eventPair.e2.eventLabel) ~
      ("e2-sentence-text" -> eventPair.e2.sentenceText) ~
      ("e2-sentence-index" -> eventPair.e2.sentence) ~
      ("e2-sentence-tokens" -> eventPair.e2.sentenceObj.words.toList) ~
      // can be used to highlight event span in annotation UI
      ("e2-start" -> eventPair.e2.start) ~
      ("e2-end" -> eventPair.e2.end) ~
      ("e2-trigger" -> eventPair.e2.trigger.text) ~
      ("e2-trigger-start" -> eventPair.e2.trigger.start) ~
      ("e2-trigger-end" -> eventPair.e2.trigger.end) ~
      // these will be filled out during annotation
      ("annotator-id" -> eventPair.annotatorID) ~
      ("relation" -> eventPair.relation) ~
      //("confidence" -> confidence) ~ // TODO: I don't know where is an error. So skip it for now.
      // additional features
      ("cross-sentence" -> eventPair.isCrossSentence) ~
      ("inter-sentence-tokens" -> interSentences) ~
      ("paper-id" -> eventPair.pmid) ~
      // annotation notes
      ("notes" -> eventPair.notes.getOrElse("")) ~
      ("e1-sentence-entities" -> e1SentEntities) ~
      ("e2-sentence-entities" -> e2SentEntities) ~
      ("inter-sentence-entities" -> interSentEntities)
  }
}

/**
  *
  * This is used to write the event-pairs.json with the matched mentions to the mcc_new older.
  */
object WriteCorpusWithMatchedMentions extends App with LazyLogging {
  val config = ConfigFactory.load()

  val oldDirTrain  = config.getString("assembly.corpus.corpusDirOldTrain")
  val interDirTrain  = config.getString("assembly.corpus.corpusDirInterTrain")
  val newDirTrain = config.getString("assembly.corpus.corpusDirNewTrain")

  val oldDirEval  = config.getString("assembly.corpus.corpusDirOldEval")
  val interDirEval  = config.getString("assembly.corpus.corpusDirInterEval")
  val newDirEval = config.getString("assembly.corpus.corpusDirNewEval")

  def writeCorpus(oldDir:String, interDir:String, newDir:String): Unit ={
    val epsOld: Seq[EventPair] = CorpusReader.readCorpus(oldDir).instances
    val newMentions = Corpus.loadMentions(interDir)
    val eps = Corpus.softAlign(epsOld, newMentions)

    val corpus = Corpus(eps)
    corpus.writeJSON(newDir, pretty = true)
  }

  writeCorpus(oldDirTrain, interDirTrain, newDirTrain)
  writeCorpus(oldDirEval, interDirEval, newDirEval)
}

/**
  * This is a debugging function to check how to access the entites in the event pairs.
  *
  * Contribution by Zhengzhong
  */

object WriteEntities extends App {

  val config = ConfigFactory.load()

  val oldDirTrain = config.getString("assembly.corpus.corpusDirOldTrain")
  val newDirTrain = config.getString("assembly.corpus.corpusDirNewTrain")

  val oldDirEval = config.getString("assembly.corpus.corpusDirOldEval")
  val newDirEval = config.getString("assembly.corpus.corpusDirNewEval")

  writeEntityList(oldDirTrain, newDirTrain)

  def writeEntityList(oldDir:String, newDir:String):Unit = {

    val epsOld: Seq[EventPair] = CorpusReader.readCorpus(oldDir).instances
    val newMentions = Corpus.loadMentions(newDir)
    val eps = Corpus.softAlign(epsOld, newMentions)

    for (ep <- eps){
      println("="*20)
      println(ep.e1.entities.toSeq)
      println(ep.e1.sentenceObj.entities.get.toSeq)
      println(ep.e2.entities.toSeq)
      println(ep.e2.sentenceObj.entities.get.toSeq)
      scala.io.StdIn.readLine()
    }

  }

}


