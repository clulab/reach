package org.clulab.reach.assembly

import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}

object TestLoadNewMentions extends App {

  val config = ConfigFactory.load()

  val newMentions = Corpus.loadMentions(config.getString("assembly.corpus.corpusDirNewTrain"))

  println(newMentions)
  println("New mentions loaded!")
}
