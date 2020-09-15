package org.clulab.reach.assembly.relations.corpus

import org.clulab.processors.Document
import org.clulab.reach.assembly.relations.classifier.AssemblyRelationClassifier
import org.clulab.reach.assembly.sieves.Constraints
import org.clulab.reach.mentions.CorefMention
import org.clulab.reach.mentions.serialization.json.{MentionJSONOps, REACHMentionSeq, JSONSerializer}
import org.clulab.serialization.json.JSONSerialization
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s._
import scala.util.hashing.MurmurHash3._
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils.forceMkdir
import ai.lum.common.FileUtils._
import java.io.File


/** Storage class for an event pair (i.e., a single example for the relation corpus) */
case class EventPair(
  e1: CorefMention,
  e2: CorefMention,
  relation: String,
  confidence: Double = AnnotationUtils.HIGH,
  annotatorID: String = "",
  notes: Option[String] = None
) extends JSONSerialization {

  import CorpusBuilder._

  require(e1.document == e2.document, "Documents must be the same for e1 and e2")
  val text = getSententialSpan(e1, e2)
  val sentenceIndices = Seq(e1.sentence, e2.sentence).distinct.sorted
  val doc = e1.document
  val pmid = getPMID(doc.id.get)

  /** whether or not the annotation requires coref */
  val coref = CorpusBuilder.requiresCoref(e1, e2)

  def isCrossSentence = sentenceIndices.length > 1

  /** Create a unique hash to identify this training instance */
  def equivalenceHash: Int = {
    // the seed (not counted in the length of finalizeHash)
    val h0 = stringHash("org.clulab.assembly.TrainingInstance")
    // get hashes for each event
    val h1 = mix(h0, e1.equivalenceHash)
    val h2 = mix(h1, e2.equivalenceHash)
    // is it cross-sentence?
    val h3 = mix(h2, isCrossSentence.hashCode)
    // the text of the sentences containing the two event mentions
    val h4 = mix(h3, text.hashCode)
    // what paper did this come from?
    val h5 = mixLast(h4, pmid.hashCode)
    finalizeHash(h5, 5)
  }

  def copy(
    before: CorefMention = this.e1,
    after: CorefMention = this.e2,
    relation: String = this.relation,
    confidence: Double = this.confidence,
    annotatorID: String = this.annotatorID,
    notes: Option[String] = this.notes
  ): EventPair = EventPair(before, after, relation, confidence, annotatorID, notes)


  def jsonAST: JValue = {
    // build json
    ("id" -> this.equivalenceHash) ~
    ("text" -> this.text) ~
    ("coref" -> this.coref) ~
    // event 1
    ("e1-id" -> this.e1.id) ~
    ("e1-label" -> this.e1.eventLabel) ~
    ("e1-sentence-text" -> this.e1.sentenceText) ~
    ("e1-sentence-index" -> this.e1.sentence) ~
    ("e1-sentence-tokens" -> this.e1.sentenceObj.words.toList) ~
    // can be used to highlight event span in annotation UI
    ("e1-start" -> this.e1.start) ~
    ("e1-end" -> this.e1.end) ~
    ("e1-trigger" -> this.e1.trigger.text) ~
    ("e1-trigger-start" -> this.e1.trigger.start) ~
    ("e1-trigger-end" -> this.e1.trigger.end) ~
    // event 2
    ("e2-id" -> this.e2.id) ~
    ("e2-label" -> this.e2.eventLabel) ~
    ("e2-sentence-text" -> this.e2.sentenceText) ~
    ("e2-sentence-index" -> this.e2.sentence) ~
    ("e2-sentence-tokens" -> this.e2.sentenceObj.words.toList) ~
    // can be used to highlight event span in annotation UI
    ("e2-start" -> this.e2.start) ~
    ("e2-end" -> this.e2.end) ~
    ("e2-trigger" -> this.e2.trigger.text) ~
    ("e2-trigger-start" -> this.e2.trigger.start) ~
    ("e2-trigger-end" -> this.e2.trigger.end) ~
    // these will be filled out during annotation
    ("annotator-id" -> this.annotatorID) ~
    ("relation" -> this.relation) ~
    ("confidence" -> confidence) ~
    // additional features
    ("cross-sentence" -> this.isCrossSentence) ~
    ("paper-id" -> this.pmid) ~
    // annotation notes
    ("notes" -> this.notes.getOrElse(""))
  }
}

object EventPair {

  def apply(mentions: Set[CorefMention]): EventPair = {
    require(mentions.size == 2, "EventPair takes exactly two Mentions")
    val mns = mentions.toSeq.sortWith((m1, m2) => m1 precedes m2)
    val before = mns.head
    val after = mns.last

    apply(before, after)
  }

  def apply(before: CorefMention, after: CorefMention): EventPair = {

    EventPair(e1 = before, e2 = after, relation = "")
  }
}

/** Corpus is a sequence of TrainingInstances */
case class Corpus(instances: Seq[EventPair]) extends JSONSerialization {

  val mentions: Seq[CorefMention] = instances.flatMap(ti => Seq(ti.e1, ti.e2)).distinct

  /** AST of event pairs */
  def jsonAST: JValue = instances.map(_.jsonAST)

  private def compressMentions(mentions: Seq[CorefMention]): Seq[CorefMention] = {
    mentions.map(_.document).toSet.foreach { doc: Document =>
      // prune text from mention docs
      doc.text = None
      //      for (s <- doc.sentences) {
      //        // compress graph map for each sentence
      //        s.dependenciesByType = s.dependenciesByType.get(GraphMap.STANFORD_COLLAPSED) match {
      //          case Some(g) => GraphMap(Map(GraphMap.STANFORD_COLLAPSED -> g))
      //          case None => s.dependenciesByType
      //        }
      //      }
    }
    mentions
  }

  def writeJSON(corpusPath: String, pretty: Boolean): Unit = writeJSON(new File(corpusPath), pretty)
  def writeJSON(corpusDir: File, pretty: Boolean): Unit = {
    val dmLUT: Map[String, Seq[CorefMention]] = mentions.groupBy(m => getPMID(m))
    val mentionDataDir = new File(corpusDir, Corpus.MENTION_DATA)
    // create data dir
    if (! mentionDataDir.exists) forceMkdir(mentionDataDir)
    // for each doc, write doc + mentions to a json file
    for ((paperID, cms) <- dmLUT) {
      val of = new File(mentionDataDir, s"$paperID-mention-data.json")
      of.writeString(cms.json(pretty), java.nio.charset.StandardCharsets.UTF_8)
    }
    // write event pair info to json file
    val epf = new File(corpusDir, s"${Corpus.EVENT_PAIRS}.json")
    epf.writeString(this.json(pretty), java.nio.charset.StandardCharsets.UTF_8)
  }
}

object Corpus extends LazyLogging {

  implicit val formats = DefaultFormats

  private val MENTION_DATA = "mention-data"
  private val EVENT_PAIRS = "event-pairs"

  def apply(corpusDir: String): Corpus = apply(new File(corpusDir))
  def apply(corpusDir: File): Corpus = {
    val mentionDataDir = new File(corpusDir, MENTION_DATA)
    logger.info(s"Deserializing mention-data...")
    // load docs in parallel
    // retain old IDs to avoid conflicts
    val cms: Map[String, CorefMention] = mentionDataDir.listFiles.par.flatMap(JSONSerializer.toCorefMentionsMap).seq.toMap
    val epsJAST = parse(new File(corpusDir, s"$EVENT_PAIRS.json"))
    logger.info(s"Building event-pairs...")
    Corpus(getEventPairs(epsJAST, cms))
  }

  def loadMentions(corpusDir:String):Map[String, CorefMention] = {
    val mentionDataDir = new File( new File(corpusDir), MENTION_DATA)

    // TODO: slice 10 examples for debugging. Change this later.
    mentionDataDir.listFiles.slice(0,10).par.flatMap(JSONSerializer.toCorefMentionsMapFilterEmpty).seq.toMap
  }

  /**
   * eps represents the "old" event pairs 
   * and cms represents "new" results from Reach for the same paper/doc.
  */
  def softAlign(eps: Seq[EventPair], cms: Seq[CorefMention]): Seq[EventPair] = {
    // consider the following:
    // 1. edit distance
    // 2. event/relation label
    // 3. event/relation args (roles & labels or roles?)
    // 4. modifications (grounding IDs, PTMs, hedging, etc.)
    ???
  }

  private def getEventPairs(epsjson: JValue, cms: Map[String, CorefMention]): Seq[EventPair] = {
    val mentionMap = cms
    for {
      aa <- epsjson.extract[Seq[AssemblyAnnotation]]
    } yield EventPair(
      e1 = mentionMap(aa.`e1-id`),
      e2 = mentionMap(aa.`e2-id`),
      relation = aa.relation,
      confidence = aa.confidence,
      annotatorID = aa.`annotator-id`,
      notes = aa.notes
    )
  }

  private case class AssemblyAnnotation(
    id: Int,
    text: String,
    relation: String,
    `annotator-id`: String,
    coref: Boolean,
    `e1-id`: String,
    `e2-id`: String,
    confidence: Double,
    notes: Option[String]
  )
}
