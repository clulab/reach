package org.clulab.reach.assembly.relations.corpus

import org.clulab.odin.Mention
import org.clulab.reach.mentions._
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.sieves.{ Constraints, SieveUtils }
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods
import org.json4s.JsonAST.JValue
import com.typesafe.scalalogging.LazyLogging
import java.io.File


object CorpusReader extends LazyLogging {

  // needed for .extract
  implicit val formats = DefaultFormats

  private def getJAST(f: String): JValue = f match {
    case url if url.startsWith("http") =>
      logger.info(s"Downloading $url")
      val fileContents = scala.io.Source.fromURL(url).mkString
      logger.info(s"Parsing json")
      JsonMethods.parse(fileContents)
    case other =>
      val f = new File(other)
      logger.info(s"Parsing json")
      JsonMethods.parse(f)
  }

  /** set all labels not in the set of positive labels to NEG */
  def filterRelations(
    eps: Seq[EventPair],
    positiveLabels: Set[String]
  ): Seq[EventPair] = eps flatMap {
    // keep subsumption annotations
    case valid if positiveLabels contains valid.relation => Seq(valid)
    // ignore bugs
    case bug if bug.relation == "Bug" => Nil
    // set relation to NEG
    case other =>
      Seq(other.copy(relation = SieveUtils.NEG))
  }

  /** Finds mention matching label and trigger text */
  def findMention(mns: Seq[Mention], label: String, triggerText: String): Mention = {
    mns.filter{ m =>
      // label and trigger text should match
      (m matches label) && (SieveUtils.findTrigger(m).text == triggerText)
    }.head
  }

  /** Find equivalent event pairs in a given window **/
  def equivalentEventPairs(
    eps: Seq[EventPair],
    dsLUT: Map[String, Seq[CorefMention]],
    kWindow: Int): Seq[EventPair] = {

    // TODO: make sure this is called with the filtered event pairs (i.e., only those pairs with one of the "precedes" label)
    val epsLUT: Map[String, Seq[EventPair]] = eps.groupBy(_.pmid)
    val proximalEquivalentPairs = for {
      (paperID: String, pairs: Seq[EventPair]) <- epsLUT
      epMentions: Seq[CorefMention] = pairs.flatMap{case pair: EventPair => Seq(pair.e1, pair.e2)}
      mns = dsLUT(paperID)
      // initialize an AssemblyManager with all relevant mentions for the paper
      am = AssemblyManager(mns ++ epMentions)
      // find equivalent mentions for the pair within this paper
      p <- pairs
      e1EER = am.getEER(p.e1)
      e1EquivMentions = am.getEquivalentEERs(e1EER, ignoreMods = false).flatMap(_.evidence).toSeq
      e2EER = am.getEER(p.e2)
      e2EquivMentions = am.getEquivalentEERs(e2EER, ignoreMods = false).flatMap(_.evidence).toSeq
      e1equiv <- e1EquivMentions
      e2equiv <- e2EquivMentions
      // TODO: must both of these be true?
      if e1equiv != p.e1
      if e2equiv != p.e2
      // impose window constraints
      // TODO: check these
      if Constraints.withinWindow(e1equiv, e2equiv, kWindow)
      if Constraints.withinWindow(e1equiv, p.e1, kWindow) || Constraints.withinWindow(e1equiv, p.e2, kWindow)
      if Constraints.withinWindow(e2equiv, p.e1, kWindow) || Constraints.withinWindow(e2equiv, p.e2, kWindow)
    // we don't know which is textually precedes the other
    } yield EventPair(Set(e1equiv.toCorefMention, e2equiv.toCorefMention))
    proximalEquivalentPairs.toSeq
  }

  def readCorpus(corpusDir: String): Corpus = Corpus(corpusDir)
}

/** Attempt to recover mentions from legacy annotation format */
object LegacyAnnotationReader extends LazyLogging {

  def readLegacyAnnotations(annoFile: File, jsonDir: File): Seq[EventPair] = {
    logger.info(s"deserializing mentions...")
    val dsLUT = datasetLUT(jsonDir)
    readLegacyAnnotations(annoFile, dsLUT)
  }

  def readLegacyAnnotations(annoFile: File, doc2cmsLUT: Map[String, Seq[CorefMention]]): Seq[EventPair] = {
    logger.info(s"reading annotations from ${annoFile.getAbsolutePath}")
    val aas = assemblyAnnotationsFromFile(annoFile)
    logger.info(s"Building EventPairs for annotations...")
    findEventPairs(aas, doc2cmsLUT)
  }

  private case class AssemblyAnnotation(
    text: String,
    relation: String,
    `annotator-id`: String,
    coref: Boolean,
    `paper-id`: String,
    //event 1
    `e1-trigger`: String,
    `e1-label`: String,
    `e1-sentence-index`: Int,
    `e1-start`: Int,
    `e1-end`: Int,
    `e1-tokens`: Seq[String],
    // event 2
    `e2-trigger`: String,
    `e2-label`: String,
    `e2-sentence-index`: Int,
    `e2-start`: Int,
    `e2-end`: Int,
    `e2-tokens`: Seq[String]
  ) {
    val sententialDistance: Int = Math.abs(this.`e1-sentence-index` - this.`e2-sentence-index`)
  }

  private def assemblyAnnotationsFromFile(f: File): Seq[AssemblyAnnotation] = {
    implicit val formats = DefaultFormats
    val json = JsonMethods.parse(f)
    json.extract[Seq[AssemblyAnnotation]]
  }

  private def isExactMatch(aa: AssemblyAnnotation, e1: CorefMention, e2: CorefMention): Boolean = {
    aa.`e1-sentence-index` == e1.sentence && aa.`e2-sentence-index` == e2.sentence &&
      aa.`e1-start` == e1.start && aa.`e2-start` == e2.start &&
      aa.`e1-end` == e1.end && aa.`e2-end` == e2.end
  }

  private def findEventPairs(aas: Seq[AssemblyAnnotation], cmsLUT: Map[String, Seq[CorefMention]]): Seq[EventPair] = {
    for {
      aa <- aas
      candidates = cmsLUT(aa.`paper-id`)
      e1Candidates = candidates.filter(_ matches aa.`e1-label`)
      e2Candidates = candidates.filter(_ matches aa.`e2-label`)
      e1c <- e1Candidates
      e2c <- e2Candidates
      // check sentential distance
      mnDist: Int = Math.abs(e1c.sentence - e2c.sentence)
      if mnDist == aa.sententialDistance
      // check for matching triggers
      //if e1c.text contains aa.`e1-trigger`
      if SieveUtils.findTrigger(e1c).text == aa.`e1-trigger`
      //if e2c.text contains aa.`e2-trigger`
      if SieveUtils.findTrigger(e2c).text == aa.`e2-trigger`
      // Do the mentions share an argument?
      if Constraints.shareEntityGrounding(e1c, e2c)
      // final check
      if Constraints.isValidRelationPair(e1c, e2c)
    } yield {

      // estimate confidence in match
      val confidence: Double = (e1c, e2c) match {
        // is the annotation an exact match?
        case exactMatch if isExactMatch(aa, exactMatch._1, exactMatch._2) =>
          AnnotationUtils.HIGH
        case inexact =>
          val weight = 0.9
          val overlapE1 = aa.`e1-tokens`.count(tok => e1c.words contains tok).toDouble / Seq(e1c.words.size, aa.`e1-tokens`.size).max
          val overlapE2 = aa.`e2-tokens`.count(tok => e2c.words contains tok).toDouble / Seq(e2c.words.size, aa.`e2-tokens`.size).max
          (overlapE1 + overlapE2) / 2.0  * weight
      }
      EventPair(
        e1c,
        e2c,
        aa.relation,
        confidence,
        annotatorID  = aa.`annotator-id`,
        notes = None
      )
    }
  }

  def updateEventPairs(annotatedEPs: Seq[EventPair], unannotatedEPs: Seq[EventPair]): Seq[EventPair] = {
    val annoLUT: Map[(CorefMention, CorefMention), EventPair] = annotatedEPs.map(eps => (eps.e1, eps.e2) -> eps).toMap
    for {
      ep <- unannotatedEPs
      k = (ep.e1, ep.e2)
    } yield {
      annoLUT contains k match {
        // use the annotated (old) ep
        case true => annoLUT(k)
        case false => ep
      }
    }
  }
}

/**
  * Houses values for three confidence bins, etc.
  */
object AnnotationUtils {
  val LOW = 0.25
  val NORMAL = 0.75
  val HIGH = 1.0
}