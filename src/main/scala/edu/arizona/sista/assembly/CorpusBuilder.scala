package edu.arizona.sista.assembly

import java.io.File
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.odin._
import edu.arizona.sista.utils.Serializer
import scala.util.hashing.MurmurHash3._
import edu.arizona.sista.reach.PaperReader
import edu.arizona.sista.reach.PaperReader.Dataset
import org.apache.commons.io.FileUtils
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native._
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

  /** Checks if two EERs share at least one SimpleEntity with the same grounding ID */
  def shareArg(r1: Event, r2: Event): Boolean = r1.I exists {
    case entity: SimpleEntity =>
      r2.hasApproximateArgument(entity)
    case complex: Complex =>
      complex.flattenMembers exists { ent => r2.hasApproximateArgument(ent) }
  }



  /** Use this check to automatically label negative examples **/
  def shareControlleds(mention1: Mention, mention2: Mention): Boolean = {
    // resolve both event mentions
    val m1 = AssemblyManager.getResolvedForm(mention1)
    val m2 = AssemblyManager.getResolvedForm(mention2)
    (m1, m2) match {
      // are teh controlleds identical?
      case (ce1: Mention, ce2: Mention) if (ce1 matches "ComplexEvent") && (ce2 matches "ComplexEvent") =>
        val c1 = AssemblyManager.getResolvedForm(ce1.arguments("controlled").head)
        val c2 = AssemblyManager.getResolvedForm(ce2.arguments("controlled").head)
        c1.text == c2.text
      case _ => false
    }
  }

  /** Ensure that pair of event mentions meeting corpus constraints. <br>
    * Requirements: <br>
    * 1. the text of the two mentions should not be the same <br>
    * 2. a regulation should not be paired with its controlled
    * */
  def isValidPair(mention1: Mention, mention2: Mention): Boolean = {
    // resolve both event mentions
    val m1 = AssemblyManager.getResolvedForm(mention1)
    val m2 = AssemblyManager.getResolvedForm(mention2)
    // a regulation should not be paired with its controlled
    // ex: "inhibited" neg-regs "activation". remove interactions between Regulations and their Controlled
    val ceConstraint: Boolean = (m1, m2) match {

      // two complex events should not share their controlled (activation-specific check)
      case (a1: Mention, a2: Mention) if (a1 matches "ActivationEvent") && (a2 matches "ActivationEvent") =>
        // controlled arg for each Activation mention should not be identical (according to grounding id)
        val controlled1 = AssemblyManager.getResolvedForm(a1.arguments("controlled").head)
        val controlled2 = AssemblyManager.getResolvedForm(a2.arguments("controlled").head)
        // grounding ids should be distinct
        controlled1.nsId() != controlled2.nsId()

      // neither of the two complex events should be the controlled of the other
      case (ce1: Mention, ce2: Mention) if (ce1 matches "ComplexEvent") && (ce2 matches "ComplexEvent") =>
        val controlled1 = AssemblyManager.getResolvedForm(ce1.arguments("controlled").head)
        val controlled2 = AssemblyManager.getResolvedForm(ce2.arguments("controlled").head)
        controlled1.text != ce2.text && controlled2.text != ce1.text

      // general checks for complex events (fall-through)
      case (m: Mention, ce: Mention) if ce matches "ComplexEvent" =>
        m.text != ce.arguments("controlled").head.text
      case (ce: Mention, m: Mention) if ce matches "ComplexEvent" =>
        m.text != ce.arguments("controlled").head.text
      case _ => true
    }
    // text spans should be unique
    (m1.words != m2.words) && ceConstraint
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

/** Storage class for Event */
case class E(m: Mention) {
  val eventLabel: String = m.label
  val text: String = m.sentenceObj.getSentenceText()
  val trigger = findTrigger(m)

  /** Retrieve trigger from Mention */
  private def findTrigger(m: Mention): TextBoundMention = m match {
    case event: EventMention =>
      event.trigger
    case rel: RelationMention if (rel matches "ComplexEvent") && rel.arguments("controlled").nonEmpty =>
      // could be nested ...
      findTrigger(rel.arguments("controlled").head)
  }
}

/** Storage class for training instances (i.e., a single example for the relation corpus) */
case class TrainingInstance(mentions: Set[Mention]) {
  import CorpusBuilder._
  val sentenceIndices = mentions.map(_.sentence).toSeq.sorted
  val doc = mentions.head.document
  val pmid = getPMID(doc.id.get)

  val text: String = {
    val sentences = for {
      i <- sentenceIndices
    } yield doc.sentences(i).getSentenceText()

    sentences.mkString("  ")
  }

  val e1: E = {
    val m = mentions.toSeq.sortBy(m => (m.sentence, m.tokenInterval)).head
    E(m)
  }

  val e2: E = {
    val m = mentions.toSeq.sortBy(m => (m.sentence, m.tokenInterval)).last
    E(m)
  }

  def isCrossSentence = sentenceIndices.length > 1

  /** Create a unique hash to identify the event.
    * Does not include information related to the event's args
    * */
  def eventHash(e: E): Int = {
    // the seed (not counted in the length of finalizeHash)
    val h0 = stringHash("edu.arizona.sista.assembly.E")
    // get event label
    val h1 = mix(h0, e.eventLabel.hashCode)
    // get trigger text
    val h2 = mix(h1, e.trigger.text.hashCode)
    // get token span
    val h3 = mix(h2, e.trigger.tokenInterval.start)
    val h4 = mix(h3, e.trigger.tokenInterval.end)
    // get sentence
    val h5 = mix(h4, e.m.sentence)
    finalizeHash(h5, 5)
  }

  /** Create a unique hash to identify this training instance */
  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    val h0 = stringHash("edu.arizona.sista.assembly.TrainingInstance")
    // get hashes for each event
    val h1 = mix(h0, eventHash(e1))
    val h2 = mix(h1, eventHash(e2))
    // is it cross-sentence?
    val h3 = mix(h2, isCrossSentence.hashCode)
    // the text of the sentences containing the two event mentions
    val h4 = mix(h3, text.hashCode)
    // what paper did this come from?
    val h5 = mixLast(h4, pmid.hashCode)
    finalizeHash(h5, 5)
  }
}

/** Corpus is a sequence of TrainingInstances */
case class Corpus(instances: Seq[TrainingInstance]) {

  import CorpusBuilder._

  def toJSON: String = {
    val examples = instances.map { ti =>
      // if the two event mentions have the same controlled, this is a negative example
      val relationLabel = if (shareControlleds(ti.e1.m, ti.e2.m)) "None" else ""
      // build json
      ("id" -> ti.equivalenceHash) ~
      ("text" -> ti.text) ~
      // event 1
      ("e1-label" -> ti.e1.eventLabel) ~
      ("e1-sentence" -> ti.e1.text) ~
      ("e1-sentence-index" -> ti.e1.m.sentence) ~
      ("e1-tokens" -> ti.e1.m.sentenceObj.words.toList) ~
      // can be used to highlight event span in annotation UI
      ("e1-start" -> ti.e1.m.start) ~
      ("e1-end" -> ti.e1.m.end) ~
      ("e1-trigger" -> ti.e1.trigger.text) ~
      ("e1-trigger-start" -> ti.e1.trigger.start) ~
      ("e1-trigger-end" -> ti.e1.trigger.end) ~
      // event 2
      ("e2-label" -> ti.e2.eventLabel) ~
      ("e2-sentence" -> ti.e2.text) ~
      ("e2-sentence-index" -> ti.e2.m.sentence) ~
      ("e2-tokens" -> ti.e2.m.sentenceObj.words.toList) ~
      // can be used to highlight event span in annotation UI
      ("e2-start" -> ti.e2.m.start) ~
      ("e2-end" -> ti.e2.m.end) ~
      ("e2-trigger" -> ti.e2.trigger.text) ~
      ("e2-trigger-start" -> ti.e2.trigger.start) ~
      ("e2-trigger-end" -> ti.e2.trigger.end) ~
      // these will be filled out during annotation
      ("annotator-id" -> "") ~
      ("relation" -> relationLabel) ~
      ("cross-sentence" -> ti.isCrossSentence) ~
      ("paper-id" -> ti.pmid)
    }
    // Dump to json
    val jValue = JArray(examples.toList)
    prettyJson(renderJValue(jValue))
  }
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
    if shareArg(r1, r2)
    // check if mention pair is valid
    if isValidPair(m1, m2)
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
