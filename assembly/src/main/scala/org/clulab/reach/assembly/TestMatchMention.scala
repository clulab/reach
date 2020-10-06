package org.clulab.reach.assembly

import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.TestLoadNewMentions.config
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}
import org.clulab.reach.mentions
import org.clulab.struct.CorefMention

import scala.collection.mutable.ArrayBuffer



object TestMatchMention extends App {
  val config = ConfigFactory.load()
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirOldEval")).instances


  val newMentions = Corpus.loadMentions(config.getString("assembly.corpus.corpusDirNewEval"))

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
        val e1MatchedSentenceIndex = findEventSentenceIndex(ep.e1, newMentions(docID))
        val e2MatchedSentenceIndex = findEventSentenceIndex(ep.e2, newMentions(docID))

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

      println("-"*20)
      println("exact match")
      debugPrintBestMatchedCandidate(originalMention, exactMatchResult.get)

      exactMatchResult
    }
    else{
      println("-"*20)

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

      if (allMentionScores.min<=0.6) {
        n_mention_soft_match+=1

        println("soft matched distance")
        debugPrintBestMatchedCandidate(originalMention, bestMatchedMention)

        Some(bestMatchedMention)
      }

      else { // I think this hyper parameter is reasonable.
       // None
        //if (bestMatchedMention.text.contains(originalMention.text)) {

        if (bestMatchedMention.text.contains(originalMention.text) || originalMention.text.contains(bestMatchedMention.text)) {
          n_mention_soft_match+=1

          println("soft matched containment")
          debugPrintBestMatchedCandidate(originalMention, bestMatchedMention)

          Some(bestMatchedMention)
        }
        else{

          println("not matched")
          debugPrintBestMatchedCandidate(originalMention, bestMatchedMention)

          None
        }

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


