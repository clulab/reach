package org.clulab.reach.assembly

import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.TestLoadNewMentions.config
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}

object TestMatchMention extends App {
  val config = ConfigFactory.load()
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirOldTrain")).instances


  val newMentions = Corpus.loadMentions(config.getString("assembly.corpus.corpusDirNewTrain"))


  val X = Corpus.softAlign(eps, newMentions)

}


