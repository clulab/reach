package edu.arizona.sista.assembly.relations

import edu.arizona.sista.assembly.sieves.Constraints
import edu.arizona.sista.assembly.CorpusBuilder
import edu.arizona.sista.odin.{RelationMention, EventMention, TextBoundMention, Mention}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native._
import scala.util.hashing.MurmurHash3._

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
case class TrainingInstance(text: String, mentions: Set[Mention]) {
  import CorpusBuilder._
  val sentenceIndices = mentions.map(_.sentence).toSeq.sorted
  val doc = mentions.head.document
  val pmid = getPMID(doc.id.get)

  val e1: E = {
    val before = mentions.toSeq.sortWith( (m1, m2) => m1 precedes m2).head
    E(before)
  }

  val e2: E = {
    val after = mentions.toSeq.sortWith( (m1, m2) => m1 precedes m2).last
    E(after)
  }

  /** whether or not the annotation requires coref */
  val coref = CorpusBuilder.requiresCoref(e1.m, e2.m)

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

  def toJSON: String = {
    val examples = instances.map { ti =>
      // if the two event mentions have the same controlled, this is a negative example
      val relationLabel = if (Constraints.shareControlleds(ti.e1.m, ti.e2.m)) "None" else ""
      // build json
      ("id" -> ti.equivalenceHash) ~
        ("text" -> ti.text) ~
        ("coref" -> ti.coref) ~
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