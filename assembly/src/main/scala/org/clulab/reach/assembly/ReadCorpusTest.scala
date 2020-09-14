import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.RunAnnotationEval.config
import org.clulab.reach.assembly.relations.corpus.{CorpusReader, EventPair}
import org.json4s.DefaultFormats

object ReadCorpusTest extends App {
  implicit val formats = DefaultFormats

  val config = ConfigFactory.load()
  val corpusDirOldTrain = config.getString("assembly.corpus.corpusDirOldTrain")

  val eps: Seq[EventPair] = CorpusReader.readCorpus(corpusDirOldTrain).instances
}
