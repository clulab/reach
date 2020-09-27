package org.clulab.reach.assembly.relations.corpus

import org.clulab.processors.Document
import org.clulab.reach.assembly.relations.classifier.AssemblyRelationClassifier
import org.clulab.reach.assembly.sieves.Constraints
import org.clulab.reach.mentions.CorefMention
import org.clulab.reach.mentions.serialization.json.{JSONSerializer, MentionJSONOps, REACHMentionSeq}
import org.clulab.serialization.json.JSONSerialization
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s._

import scala.util.hashing.MurmurHash3._
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils.forceMkdir
import ai.lum.common.FileUtils._
import java.io.File

import org.clulab.reach.assembly.AssemblyManager

import scala.collection.mutable.ArrayBuffer


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

  val validLabels = Seq("ComplexEvent","Binding")

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

  def loadMentions(corpusDir:String):Map[String, Seq[CorefMention]] = {
    logger.info(s"Loading new mentions ...")
    val mentionDataDir = new File( new File(corpusDir), MENTION_DATA)

    // TODO: slice 10 examples for debugging. Change this later.
    val newMentionSeq = mentionDataDir.listFiles.par.flatMap(JSONSerializer.toCorefMentionsMapFilterEmpty).seq.toMap.values.toSeq
    logger.info(s"Loading new mentions finished. Total number of mentions: ${newMentionSeq.length}")

    //
    val mentionGroupedByPaperID = scala.collection.mutable.Map[String, Seq[CorefMention]]()
    for (m <- newMentionSeq){
      val mentionID = m.document.id.get.slice(3, m.document.id.get.length)
      if (!mentionGroupedByPaperID.contains(mentionID)){
        mentionGroupedByPaperID(mentionID) = ArrayBuffer[CorefMention]()
      }
      else{
        mentionGroupedByPaperID(mentionID).asInstanceOf[ArrayBuffer[CorefMention]].append(m)
      }
    }
    mentionGroupedByPaperID.toMap
  }

  /**
   * eps represents the "old" event pairs 
   * and cms represents "new" results from Reach for the same paper/doc.
  */
  def softAlign(eps: Seq[EventPair], cms: Map[String, Seq[CorefMention]]): Seq[EventPair] = {
    // consider the following:
    // 1. edit distance
    // 2. event/relation label
    // 3. event/relation args (roles & labels or roles?)
    // 4. modifications (grounding IDs, PTMs, hedging, etc.)
    val validLabels = Seq("ComplexEvent", "Binding")

    logger.info(s"Matching old mentions with new mentions ...")

    // TODO: This is for the debugging purpose, for printing out the triggers where the mentions are not found.
    //  because I have problems directly accessing the triggers through the annotated mentions.
    val epsJAST = parse(new File(new File("/work/zhengzhongliang/2020_ASKE/20200831/mcc_old/train"), s"$EVENT_PAIRS.json")).extract[Seq[Map[String, JValue]]]

    logger.info(s"They should have the same length ${epsJAST.length}, ${eps.length}")

    var nMissingPaper = 0
    var nMissingMention = 0
    var nInvalidLabel =0
    val triggerCount = new ArrayBuffer[String]()
    val eventPairsUpdated = new ArrayBuffer[EventPair]()
    for ((ep, idx) <- eps.zipWithIndex){
      val e1DocID = ep.e1.document.id.get.split("_")(0)
      val e2DocID = ep.e2.document.id.get.split("_")(0)

//      println("-"*20)
//      println(ep.e1.label)
//      println(ep.e1.labels)
      if (e1DocID==e2DocID && cms.contains(e1DocID)){
        val e1Matched = getMatchedMention(ep.e1, cms(e1DocID), "compositionalFilter")
        val e2Matched = getMatchedMention(ep.e2, cms(e2DocID), "compositionalFilter")
        if (e1Matched.isDefined && e2Matched.isDefined){
          if (validLabels.exists(label => e1Matched.get matches label) && validLabels.exists(label => e2Matched.get matches label) && AssemblyManager.isValidMention(e1Matched.get) && AssemblyManager.isValidMention(e2Matched.get)){
            // TODO: debug, check whether the matched event is the true event.
            println("="*20)
            println("e1 characteristics")
            println(s"\te1 text: ${ep.e1.text}, mention bound: (${ep.e1.start},${ep.e1.end})")
            println(s"\te1 sent idx: ${ep.e1.sentence}, sent words: ${ep.e1.sentenceObj.words.toSeq}")
            println(s"\te1 labels:${ep.e1.labels}") // Trigger is not printed because the function to print trigger is a little problematic.
            println(s"\te1 arguments")
            ep.e1.arguments.toSeq.foreach{x=>println(s"\t\t(${x._1},${x._2.head.text})")}
            println(s"\te1 modifications")
            ep.e1.modifications.foreach{x=> println(s"\t\t${x.label}")}
            println("\n")
            println(s"\tmatched text: ${e1Matched.get.text}, mention bound: (${e1Matched.get.start},${e1Matched.get.end})")
            println(s"\tmatched sent idx: ${e1Matched.get.sentence}, sent words: ${e1Matched.get.sentenceObj.words.toSeq}")
            println(s"\tmatched labels:${e1Matched.get.labels}")
            println(s"\tmatched arguments")
            e1Matched.get.arguments.toSeq.foreach{x=>println(s"\t\t(${x._1},${x._2.head.text})")}
            println(s"\tmatched modifications")
            e1Matched.get.modifications.foreach{x=> println(s"\t\t${x.label}")}

            println("-"*20)
            println("e2 characteristics")
            println(s"\te2 text: ${ep.e2.text}, mention bound: (${ep.e2.start},${ep.e2.end})")
            println(s"\te2 sent idx: ${ep.e2.sentence}, sent words: ${ep.e2.sentenceObj.words.toSeq}")
            println(s"\te2 labels:${ep.e2.labels}")
            println(s"\te2 arguments")
            ep.e2.arguments.toSeq.foreach{x=>println(s"\t\t(${x._1},${x._2.head.text})")}
            println(s"\te2 modifications")
            ep.e2.modifications.foreach{x=> println(s"\t\t${x.label}")}
            println("\n")
            println(s"\tmatched text: ${e2Matched.get.text}, mention bound: (${e2Matched.get.start},${e2Matched.get.end})")
            println(s"\tmatched sent idx: ${e2Matched.get.sentence}, sent words: ${e2Matched.get.sentenceObj.words.toSeq}")
            println(s"\tmatched labels:${e2Matched.get.labels}")
            println(s"\tmatched arguments")
            e2Matched.get.arguments.toSeq.foreach{x=>println(s"\t\t(${x._1},${x._2.head.text})")}
            println(s"\tmatched modifications")
            e2Matched.get.modifications.foreach{x=> println(s"\t\t${x.label}")}

            scala.io.StdIn.readLine()

            eventPairsUpdated.append(
              new EventPair(
                e1 = e1Matched.get,
                e2 = e2Matched.get,
                relation = ep.relation,
                confidence = ep.confidence,
                annotatorID = ep.annotatorID,
                notes = ep.notes
              )
            )
          }
          else{nInvalidLabel+=1}
        }
        else {
          nMissingMention+=1
          if (!e1Matched.isDefined) {
            if (epsJAST(idx).contains("e1-trigger")){triggerCount.append(epsJAST(idx)("e1-trigger").toString)}
          }
          if (!e2Matched.isDefined) {
            if (epsJAST(idx).contains("e2-trigger")){triggerCount.append(epsJAST(idx)("e2-trigger").toString)}
          }
        }
      }
      else {nMissingPaper+=1}
    }
    println("matching finished")
    //println(triggerCount.groupBy(identity).mapValues(_.size).toSeq.sortWith(_._2 > _._2))

    logger.info(s"Matching finished! Total pairs ${eps.length}, matched pairs: ${eventPairsUpdated.length}")
    logger.info(s"\tn missing paper: ${nMissingPaper}, n missing mention: ${nMissingMention}, n invalid label: ${nInvalidLabel}")
    eventPairsUpdated
  }

  private def getMatchedMention(queryMention:CorefMention, candidateMentions:Seq[CorefMention], matchMethod:String):Option[CorefMention] = {
    val oldMentionText = queryMention.text.toLowerCase()

//    println("="*20)
//    println(oldMentionText)
//    println(candidateMentionsText)
    //scala.io.StdIn.readLine()

    // We would like to test at least the following schemes:
    // 1, mention text exact match => "mentionTextExactMatch"
    // 2, mention text edit distance => "mentionTextEditDistance"
    // 3, mention text exact match + label exact match => "mentionTextAndLabelExactMatch"
    // 4, mention text edit distance + labels edit distance => "mentionTextAndLabelsEditDistance"

    matchMethod match {
      case "mentionTextExactMatch" =>{
        candidateMentions.find(x => x.text.toLowerCase()==oldMentionText)
      }
      case "mentionTextEditDistance" => {
        val textDistance = candidateMentions.map{m => editDistance(oldMentionText, m.text.toLowerCase())}
        val minDistanceIdx = textDistance.indexOf(textDistance.min)
        if (textDistance.min/oldMentionText.length<0.3){
          Some(candidateMentions(minDistanceIdx))
        }
        else{
          None
        }
      }
      case "mentionTextAndLabelExactMatch" => {
        candidateMentions.find(m => (m.text.toLowerCase()==oldMentionText && m.label == queryMention.label))
      }
      case "mentionTextAndLabelsEditDistance" => {
        ???
      }
      case "compositionalFilter" => {
        // Matching logic:
        // 1, the labels of the new mention should contain either ComplexEvent or Binding.
        // 2, TODO: check with Mihai, should we enforce the new mention label to be the same as the old label? How is the label assigned?
        // 3, Get the mention with the minimum edit distance, and the distance is not too large.
        val candidateMentionsWithValidLabels = candidateMentions.filter(m => validLabels.exists(lb => m matches lb) && AssemblyManager.isValidMention(m))
        val textDistance = candidateMentionsWithValidLabels.map{m => editDistance(oldMentionText, m.text.toLowerCase())}
        val minDistanceIdx = textDistance.indexOf(textDistance.min)
        if (textDistance.min/oldMentionText.length<0.3){
          Some(candidateMentionsWithValidLabels(minDistanceIdx))
        }
        else{
          None
        }
      }
      case _ => {
        logger.error(s"No matching method specified")
        None
      }

    }

  }

  private def editDistance(textSeq1: Seq[Char], textSeq2: Seq[Char]):Int = {
    //This is found from here: https://www.reddit.com/r/scala/comments/7sqtyf/scala_edit_distance_implementation/
    // Use simple text cases to verify it:
    // println(editDistance(Seq("I", "have","a","dream"), Seq("I", "have","a", "good", "dream")))
    // println(editDistance(Seq("I", "have","a","dream"), Seq("I", "have","a", "very", "good", "dream")))
    val startRow = (0 to textSeq2.size).toList
    textSeq1.foldLeft(startRow) { (prevRow, aElem) =>
      (prevRow.zip(prevRow.tail).zip(textSeq2)).scanLeft(prevRow.head + 1) {
        case (left, ((diag, up), bElem)) => {
          val aGapScore = up + 1
          val bGapScore = left + 1
          val matchScore = diag + (if (aElem == bElem) 0 else 1)
          List(aGapScore, bGapScore, matchScore).min
        }
      }
    }.last
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

  private case class TriggerAnnotation(`e1-trigger`:String, `e2-trigger`:String)

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
