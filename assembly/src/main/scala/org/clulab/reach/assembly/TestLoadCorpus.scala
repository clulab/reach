import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.relations.corpus.{CorpusReader, EventPair}

object TestLoadCorpus extends App {


  val config = ConfigFactory.load()
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirOldTrain")).instances

  println("Corpus loading finished!")
}
