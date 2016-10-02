package org.clulab.assembly.relations.corpus

import com.typesafe.scalalogging.LazyLogging
import org.clulab.assembly.AssemblyManager
import org.clulab.assembly.relations.classifier.AssemblyRelationClassifier
import org.clulab.assembly.sieves.{Constraints, SieveUtils}
import org.clulab.odin.Mention
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods
import org.clulab.reach.mentions._
import org.json4s.JsonAST.JValue
import java.io.File


object CorpusReader extends LazyLogging {

  // default label for negative class
  val NEG = AssemblyRelationClassifier.NEG

  val precedenceRelations =  Set("E1 precedes E2", "E2 precedes E1")
  val subsumptionRelations = Set("E1 specifies E2", "E2 specifies E1")
  val equivalenceRelations = Set("Equivalent")
  val noRelations = Set(NEG)

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
      Seq(other.copy(relation = NEG))
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
      e1EquivMentions = am.getEquivalentEERs(e1EER).flatMap(_.evidence).toSeq
      e2EER = am.getEER(p.e2)
      e2EquivMentions = am.getEquivalentEERs(e2EER).flatMap(_.evidence).toSeq
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
      text: String = CorpusBuilder.getSententialSpan(e1equiv, e2equiv)
    // we don't know which is textually precedes the other
    } yield EventPair(text, Set(e1equiv.toCorefMention, e2equiv.toCorefMention))
    proximalEquivalentPairs.toSeq
  }

  def readCorpus(corpusDir: String): Corpus = Corpus(corpusDir)
}

/**
  * Houses values for three confidence bins, etc.
  */
object AnnotationUtils {
  val LOW = 0.25
  val NORMAL = 0.75
  val HIGH = 1.0
}