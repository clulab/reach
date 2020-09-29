package org.clulab.reach.assembly

import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.TestLoadNewMentions.config
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}
import org.clulab.reach.mentions
import org.clulab.struct.CorefMention

object TestMatchMention extends App {
  val config = ConfigFactory.load()
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirOldTrain")).instances


  val newMentions = Corpus.loadMentions(config.getString("assembly.corpus.corpusDirNewTrain"))


  //val X = Corpus.softAlign(eps, newMentions)
  softAlginPrototype(eps, newMentions)

  def softAlginPrototype(eps:Seq[EventPair], newMentions: Map[String, Seq[mentions.CorefMention]]):Unit = {
    var n_pairs_due_to_missing_paper = 0
    var n_pairs_due_to_missing_sentence = 0
    for (ep <- eps){
      val docID = ep.e1.document.id.get.split("_")(0)
      if (newMentions.contains(docID)){
        if (!findSentence(ep, newMentions(docID))){
          n_pairs_due_to_missing_sentence+=1
        }

      }
      else {
        n_pairs_due_to_missing_paper+=1
      }

    }
    println(s"n pairs due to missing paper: ${n_pairs_due_to_missing_paper}")
    println(s"n pairs due to unmatched sentence: ${n_pairs_due_to_missing_sentence}")
  }

  def findSentence(ep:EventPair, candidateMentions:Seq[mentions.CorefMention]):Boolean = {
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

}


