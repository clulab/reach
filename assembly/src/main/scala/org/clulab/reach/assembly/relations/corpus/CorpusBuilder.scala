package org.clulab.reach.assembly.relations.corpus

import org.clulab.reach.mentions._
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.sieves.Constraints
import org.clulab.odin._
import scala.util.Try
import collection.JavaConversions._
import org.apache.commons.io.FileUtils
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import java.io.File


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

  val config = ConfigFactory.load()
  val kWindow = config.getInt("assembly.windowSize")
  val validLabels: Set[String] = config.getStringList("assembly.corpus.validLabels").toSet

  /**
    * Find mentions in sentences of interest. <br>
    * @param mns a sequence of Odin Mentions
    * @return a Sequence of Odin Mentions allowed in the assembly corpus
    */
  def getValidMentionsForSentences(mns: Seq[Mention]): Seq[Mention] = {
    mns.filter { m =>
      // event must be a "complete" event (ComplexEvent or Binding)
      ((AssemblyManager.getResolvedForm(m) matches "ComplexEvent") || (AssemblyManager.getResolvedForm(m) matches "Binding")) &&
        // the mentions must be valid in assembly
        AssemblyManager.isValidMention(m)
    }
  }

  /** Creates PubMed paper url for the source of an Odin Mention */
  def PMIDurlFromMention(m: Mention): String = {
    val pmid = getPMID(m)
    s"http://www.ncbi.nlm.nih.gov/pmc/articles/$pmid/"
  }

  /**
    * Get the shortest sentential span of text that includes any sources for coref resolution in the two mentions
    */
  def getSententialSpan(m1: Mention, m2: Mention): String = {
    val doc = m1.document
    // get sentences of resolved form of arguments
    // NOTE: arguments may involve coref
    val sentenceIndices = getResolvedSentenceIndices(m1) ++ getResolvedSentenceIndices(m2) ++
      // get sentence indices of resolved forms of mentions
      Set(AssemblyManager.getResolvedForm(m1).sentence, AssemblyManager.getResolvedForm(m2).sentence) ++
      // include unresolved indices
      Set(m1.sentence, m2.sentence)
    // get first and last sentence
    val start = sentenceIndices.min
    val end = sentenceIndices.max

    val sentences = for {
      i <- start to end
    } yield doc.sentences(i).getSentenceText

    sentences.mkString("  ")
  }

  /** get sentence indices of resolved forms of arguments (NOTE: arguments may involve coref */
  def getResolvedSentenceIndices(m: Mention): Set[Int] = {
    AssemblyManager.getResolvedForm(m)
      .arguments.values.flatten
      .map(a => AssemblyManager.getResolvedForm(a).sentence)
      .toSet
  }

  /** whether or not either mention in the pair involves coref */
  def requiresCoref(m1: Mention, m2: Mention): Boolean = {
    AssemblyManager.involvesCoreference(m1) || AssemblyManager.involvesCoreference(m2)
  }

  def findValidMentions(mns: Seq[CorefMention]): Seq[CorefMention] = mns.filter { m =>
    val resolvedForm = AssemblyManager.getResolvedForm(m)
    // mention's label must be deemed valid
    validLabels.exists(v => resolvedForm matches v) &&
      // mention must be handled by assembly
      AssemblyManager.isValidMention(m)
  }

  def selectEventPairs(cms: Seq[CorefMention]): Seq[EventPair] = {
    // track mentions
    val am = AssemblyManager(cms)
    // select event pairs
    val eps = for {
    // iterate over pairs of sentences in each doc
      (doc, corefmentions) <- cms.groupBy(m => m.document)
      candidates = findValidMentions(corefmentions)
      // iterate over pairs of the mentions
      m1 <- candidates
      m2 <- candidates
      // the mentions should be distinct
      if m1 != m2
      // the mentions must be within the acceptable sentential window
      if Constraints.withinWindow(m1, m2, kWindow)
      // check if mention pair meets corpus constraints
      // make sure mentions can be handled by AssemblyManager
      // could be SimpleEvent, Reg, or Activation...
      if Constraints.isValidRelationPair(m1, m2)
      // EERs must share at least one arg
      if Constraints.shareArg(m1, m2)
      // create training instance
      ep = EventPair(Set(m1, m2))
      // triggers should not be the same
      if ep.e1.trigger != ep.e2.trigger
    } yield ep

    eps.toSeq
  }

  def distinctEventPairs(eps: Seq[EventPair]): Seq[EventPair] = {
    val distinctEPs: Seq[EventPair] = eps
      .groupBy(ti => (ti.e1.eventLabel, ti.e1.trigger, ti.e2.eventLabel, ti.e2.trigger))
      .values.map(_.head)
      .toSet.toSeq
    // sort
    distinctEPs.sortBy{ ep => (ep.doc.id.getOrElse(""), ep.sentenceIndices.head) }
  }
}


/**
  * Builds a relation corpus from a serialized dataset. <br>
  * Corpus is written as json
  */
object BuildCorpus extends App with LazyLogging {

  import CorpusBuilder._

  logger.info(s"Loading dataset ...")
  val jsonFiles = new File(config.getString("assembly.corpus.jsonDir")).listFiles.par
  val dataset: Map[String, Seq[CorefMention]] = datasetLUT(jsonFiles)

  logger.info(s"processing ${dataset.values.map(_.size).sum} mentions from ${dataset.size} papers ...")
  // get training instances
  val eps: Seq[EventPair] = for {
    (pmid, mentions) <- dataset.toSeq
    ep <- selectEventPairs(mentions)
  } yield ep

  // distinct and sort
  val distincteps: Seq[EventPair] = distinctEventPairs(eps)
  logger.info(s"Found ${distincteps.size} examples for relation corpus ...")

  val outDir = new File(config.getString("assembly.corpus.corpusDir"))
  // create corpus and write to file
  val corpus = Corpus(distincteps)
  corpus.writeJSON(outDir, pretty = false)
}