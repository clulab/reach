package org.clulab.reach.assembly


import org.json4s.jackson.JsonMethods._
import org.json4s._
import org.json4s.jackson.Serialization.write

import java.io.File

import com.typesafe.config.ConfigFactory
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.{ReachSystem, context}
import ai.lum.common.FileUtils._
import java.nio.charset.StandardCharsets.UTF_8

import org.clulab.reach.mentions.serialization.json._
import org.clulab.reach.mentions.{BioMention, CorefMention}

import scala.math.{max, min}
import org.clulab.utils.Serializer

object RegenerateEventPair extends App {

  val config = ConfigFactory.load()
  val corpusDirOldTrain = config.getString("assembly.corpus.corpusDirOldTrain")
  val corpusDirOldEval = config.getString("assembly.corpus.corpusDirOldEval")


  val corpusDirNewTrain = config.getString("assembly.corpus.corpusDirNewTrain")
  val corpusDirNewEval = config.getString("assembly.corpus.corpusDirNewEval")


  def getEventPairs(corpusDir:String):Seq[JValue] = {
    println(s"Loading old event pairs from ${corpusDir} ...")
    val epsJAST = parse(new File(corpusDir, s"event-pairs.json"))
    epsJAST.extract[Seq[JValue]]
  }

  def getMentionIDTextSeq(corpusDir:String):Seq[(String, Seq[String])] = {
    println(s"Loading mention data from ${corpusDir} ...")
    val mentionDataDir = new File(corpusDir, "mention-data")
    val cms: Seq[CorefMention] = mentionDataDir.listFiles.par
      .flatMap(JSONSerializer.toCorefMentionsFilterEmpty).seq
    // Don't forget to normalize the text before returning it. I think this is already handled by .toLowerCase()
    cms.map{x =>
      (x.id , x.text.toLowerCase().split(" ").toSeq)
    }
  }

  def editDistance(textSeq1: Seq[String], textSeq2: Seq[String]):Int = {
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

  def getBestMatchedMentionID(queryTokens:Seq[String], paperMentionSeq: Seq[(String, Seq[String])]): String= {
    // I choose to use edit distance to compare two strings.
    val queryNormedText = queryTokens.map(x => x.toLowerCase())
    val editDistances = paperMentionSeq.map(x => editDistance(queryNormedText, x._2))
    val minDistanceIdx = editDistances.indexOf(editDistances.min)

    paperMentionSeq(minDistanceIdx)._1
  }

  def writeEventPairs(eventPairs: Seq[Any], corpusDir:String): Unit ={
    implicit val formats = DefaultFormats
    val f = new File(corpusDir, s"event-pairs.json")
    f.writeString(string = write(eventPairs), charset = UTF_8, append = false, gzipSupport = false)

  }

  def updateEventPair(corpusDirOld:String, corpusDirNew:String):Unit = {
    // TODO: before actually run it, rethink what corpus directory we want to use.
    val mentionIDTextSeq = getMentionIDTextSeq(corpusDirNew) // This should be corpusDirNew actually.
    val oldEventPairs = getEventPairs(corpusDirOld)

    //Useful fields:
    // e1-sentence-text, e1-start, e1-end, e2 ...
    // TODO: how to get the event text and modify it? Not sure.
    var totalNumEvent = 0
    var diffSent = 0
    val eventPairsUpdated = scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, JValue]]()
    for (oldEventPair <- oldEventPairs) {
      totalNumEvent+=1
      val oldEventPairParsed = oldEventPair.extract[Map[String, JValue]]
      val e1SentIndex = oldEventPairParsed("e1-sentence-index").extract[String].toInt
      val e2SentIndex = oldEventPairParsed("e2-sentence-index").extract[String].toInt

      val e1Start = oldEventPairParsed("e1-start").extract[String].toInt
      val e1End = oldEventPairParsed("e1-end").extract[String].toInt
      val oldEventText1 = oldEventPairParsed("e1-sentence-tokens").extract[Seq[String]].slice(e1Start, e1End)

      val e2Start = oldEventPairParsed("e2-start").extract[String].toInt
      val e2End = oldEventPairParsed("e2-end").extract[String].toInt
      val oldEventText2 = oldEventPairParsed("e2-sentence-tokens").extract[Seq[String]].slice(e2Start, e2End)

      val e1MatchedID = getBestMatchedMentionID(oldEventText1, mentionIDTextSeq)
      val e2MatchedID = getBestMatchedMentionID(oldEventText2, mentionIDTextSeq)

      // The following conversion method is copied from:
      // https://stackoverflow.com/questions/5042878/how-can-i-convert-immutable-map-to-mutable-map-in-scala
      val newEventPairParsed = scala.collection.mutable.Map(oldEventPairParsed.toSeq: _*)
      newEventPairParsed("e1-id") = JString(e1MatchedID)
      newEventPairParsed("e2-id") = JString(e2MatchedID)

      eventPairsUpdated.append(newEventPairParsed)
    }
    writeEventPairs(eventPairsUpdated, corpusDirNew)
    println(s"Event pairs updated! Total number of updated events ${totalNumEvent}.")
  }
  //getEventPairs(corpusDirOldTrain)
  //getMentionIDTextSeq(corpusDirOldTrain)
  updateEventPair(corpusDirOldTrain, corpusDirNewTrain)
  updateEventPair(corpusDirOldEval, corpusDirNewEval)



}
