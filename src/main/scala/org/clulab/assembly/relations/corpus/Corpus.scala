package org.clulab.assembly.relations.corpus

import org.clulab.assembly.relations.classifier.AssemblyRelationClassifier
import org.clulab.assembly.sieves.Constraints
import org.clulab.reach.mentions.CorefMention
import org.clulab.reach.mentions.serialization.json.CorefMentionSeq
import org.clulab.serialization.json.JSONSerialization
import org.json4s.JsonDSL._
import scala.util.hashing.MurmurHash3._


/** Storage class for an event pair (i.e., a single example for the relation corpus) */
case class EventPair(text: String, e1: CorefMention, e2: CorefMention, relation: String) {

  import CorpusBuilder._

  require(e1.document == e2.document, "Documents must be the same for e1 and e2")
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
    text: String = this.text,
    before: CorefMention = this.e1,
    after: CorefMention = this.e2,
    relation: String = this.relation
  ): EventPair = EventPair(text, before, after, relation)
}

object EventPair {

  def apply(text: String, mentions: Set[CorefMention]): EventPair = {
    val before = mentions.toSeq.sortWith((m1, m2) => m1 precedes m2).head
    val after = mentions.toSeq.sortWith((m1, m2) => m1 precedes m2).last

    apply(text, before, after)
  }

  def apply(text: String, before: CorefMention, after: CorefMention): EventPair = {

    // if the two event mentions have the same controlled, this is a negative example
    val rel: String = if (Constraints.shareControlleds(before, after)) AssemblyRelationClassifier.NEG else ""

    EventPair(text, e1 = before, e2 = after, rel)
  }
}

/** Corpus is a sequence of TrainingInstances */
case class Corpus(instances: Seq[EventPair]) extends JSONSerialization {

  val mentions: Seq[CorefMention] = instances.flatMap(ti => Seq(ti.e1, ti.e2)).distinct

  def jsonAST = {

    import CorpusBuilder.EventOps

    val examples = instances.map { ti =>
      // TODO: Should this json be condensed?
      // build json
      ("id" -> ti.equivalenceHash) ~
      ("text" -> ti.text) ~
      ("coref" -> ti.coref) ~
      // event 1
      ("e1-id" -> ti.e1.id) ~
      ("e1-label" -> ti.e1.eventLabel) ~
      ("e1-sentence" -> ti.e1.sentenceText) ~
      ("e1-sentence-index" -> ti.e1.sentence) ~
      ("e1-tokens" -> ti.e1.sentenceObj.words.toList) ~
      // can be used to highlight event span in annotation UI
      ("e1-start" -> ti.e1.start) ~
      ("e1-end" -> ti.e1.end) ~
      ("e1-trigger" -> ti.e1.trigger.text) ~
      ("e1-trigger-start" -> ti.e1.trigger.start) ~
      ("e1-trigger-end" -> ti.e1.trigger.end) ~
      // event 2
      ("e2-id" -> ti.e2.id) ~
      ("e2-label" -> ti.e2.eventLabel) ~
      ("e2-sentence" -> ti.e2.text) ~
      ("e2-sentence-index" -> ti.e2.sentence) ~
      ("e2-tokens" -> ti.e2.sentenceObj.words.toList) ~
      // can be used to highlight event span in annotation UI
      ("e2-start" -> ti.e2.start) ~
      ("e2-end" -> ti.e2.end) ~
      ("e2-trigger" -> ti.e2.trigger.text) ~
      ("e2-trigger-start" -> ti.e2.trigger.start) ~
      ("e2-trigger-end" -> ti.e2.trigger.end) ~
      // these will be filled out during annotation
      ("annotator-id" -> "") ~
      ("relation" -> ti.relation) ~
      ("confidence" -> AnnotationUtils.HIGH) ~
      // additional features
      ("cross-sentence" -> ti.isCrossSentence) ~
      ("paper-id" -> ti.pmid) ~
      // annotation notes
      ("notes" -> "")
    }

    // TODO: find a way to do this without adding "mention-data"
    ("mention-data" -> mentions.jsonAST) ~ ("event-pairs" -> examples)
  }
}
