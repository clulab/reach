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
    val matchingResultE1 = candidateMentions.map{x => if (ep.e1.sentenceObj.words==x.sentenceObj.words) {x.sentence} else -1}
    val matchingResultE2 = candidateMentions.map{x => if (ep.e2.sentenceObj.words==x.sentenceObj.words) {x.sentence} else -1}
    println(s"matched results: ${matchingResultE1.toSet}, ${matchingResultE2.toSet}")
    if (matchingResultE1.toSet.size==1 && matchingResultE2.toSet.size==1){
      true
    }
    else{
      false
    }

  }

}


