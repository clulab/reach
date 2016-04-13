package edu.arizona.sista.assembly

import java.io.File
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.assembly.relations.{Corpus, TrainingInstance}
import edu.arizona.sista.assembly.representations.Event
import edu.arizona.sista.assembly.sieves.Constraints
import edu.arizona.sista.odin._
import org.apache.commons.io.FileUtils
import edu.arizona.sista.utils.Serializer
import edu.arizona.sista.reach.PaperReader
import edu.arizona.sista.reach.PaperReader.Dataset
import scala.util.{Try,Success,Failure}

/**
 * RELATION CORPUS REQUIREMENTS:
 * find events occurring in the same or neighboring sentences
 * events should share >= 1 participant
 * events must be "complete" (only ComplexEvent or Binding)
 * event mentions should not share the same trigger (this avoids coordinations)
*/

/**
 * Utility functions for building the assembly relation corpus
 */
object CorpusBuilder {

  /**
   * Find mentions in sentences of interest. <br>
   * Mentions must match the label "Event".
   * @param mns a sequence of Odin Mentions
   * @param sentIndices indices for the sentences of interest
   * @return a Sequence of Odin Mentions allowed in the assembly corpus
   */
  def getValidMentionsForSentences(
    mns: Seq[Mention],
    sentIndices: Seq[Int]
  ): Seq[Mention] = {
    mns.filter { m =>
      // event must be a "complete" event (ComplexEvent or Binding)
      ((AssemblyManager.getResolvedForm(m) matches "ComplexEvent") || (AssemblyManager.getResolvedForm(m) matches "Binding")) &&
        // must be an event and must be from a relevant sentence
        (sentIndices contains m.sentence) &&
        // the mentions must be valid in assembly
        AssemblyManager.isValidMention(m)
    }
  }

  /** Creates PubMed paper url for the source of an Odin Mention */
  def PMIDurlFromMention(m: Mention): String = {
    val pmid = getPMID(m)
    s"http://www.ncbi.nlm.nih.gov/pmc/articles/$pmid/"
  }

  /** Retrieves PubMed ID from Document.id of an Odin Mention */
  def getPMID(mention: Mention): String = getPMID(mention.document.id.get)
  /** Retrieves PubMed ID from Document.id of an Odin Mention */
  def getPMID(docid: String): String = docid.split("_")(0)

}


/**
 * Builds a relation corpus from a serialized dataset. <br>
 * Corpus is written as json
 */
object BuildCorpus extends App {

  import CorpusBuilder._

  val config = ConfigFactory.load()
  val outFile = new File(config.getString("assembly.corpusFile"))

  val papersDir = config.getString("ReadPapers.papersDir")
  val datasetSource = config.getString("ReadPapers.serializedPapers")

  println(s"Loading dataset ...")

  def loadDataset(f: String): Try[Dataset] = Try(Serializer.load(datasetSource))

  // generate Dataset from papers
//  val dataset: Dataset = loadDataset(datasetSource) match {
//    case Success(ds) => ds
//    case Failure(f) => PaperReader.readPapers(papersDir)
//  }
  val dataset: Dataset = Serializer.load(datasetSource)

  println(s"processing ${dataset.values.map(_.size).sum} mentions from ${dataset.size} papers ...")
  // get training instances
  val trainingInstances: Seq[TrainingInstance] = for {
    (pmid, mentions) <- dataset.toSeq
    // track mentions
    am = AssemblyManager(mentions)
    // iterate over pairs of sentences in each doc
    (doc, mns) <- mentions.groupBy(m => m.document)
    i <- -1 until doc.sentences.length
    j = i + 1
    // could be SimpleEvent, Reg, or Activation...
    // make sure mentions can be handled by AssemblyManager
    // iterate over pairs of the mentions in each pair of sentences
    mentionsOfInterest = getValidMentionsForSentences(mns, Seq(i, j))
    m1 <- mentionsOfInterest
    m2 <- mentionsOfInterest
    if m1 != m2
    _r1 = am.getEER(m1)
    _r2 = am.getEER(m2)
    // a Generic_event may link to a Complex...
    if _r1.isInstanceOf[Event] && _r2.isInstanceOf[Event]
    r1 = _r1.asInstanceOf[Event]
    r2 = _r2.asInstanceOf[Event]
    // EERs must share at least one arg
    if Constraints.shareArg(r1, r2)
    // check if mention pair is valid
    if Constraints.isValidRelationPair(m1, m2)
    // create training instance
    ti = TrainingInstance(Set(m1, m2))
    // triggers should not be the same
    if ti.e1.trigger != ti.e2.trigger
  } yield ti

  val distinctTrainingInstances: Set[TrainingInstance] = trainingInstances
    .groupBy(ti => (ti.e1.eventLabel, ti.e1.trigger, ti.e2.eventLabel, ti.e2.trigger))
    .values.map(_.head)
    .toSet

  println(s"Found ${distinctTrainingInstances.size} examples for relation corpus ...")
  val sortedTIs: Seq[TrainingInstance] =
    distinctTrainingInstances
      .toSeq
      .sortBy(t => (t.doc.id.get, t.sentenceIndices.head))
  val corpus = Corpus(sortedTIs)
  val content = corpus.toJSON
  FileUtils.writeStringToFile(outFile, content)
  println(s"Wrote corpus to ${outFile.getAbsolutePath}")
}