package org.clulab.reach.assembly.relations.corpus

import org.clulab.reach.mentions._
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.sieves.Constraints
import org.clulab.odin._
import ai.lum.common.ConfigUtils._
import ai.lum.common.RandomUtils._
import ai.lum.common.FileUtils._

import collection.JavaConversions._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

import java.io.File
import org.clulab.reach.mentions.serialization.json.{JSONSerializer => ReachJSONSerializer}
import org.clulab.utils.ThreadUtils

import scala.collection.parallel.ForkJoinTaskSupport


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
object CorpusBuilder extends LazyLogging {

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
      // are these events of interest?
      if validLabels.exists(label => m1 matches label)
      if validLabels.exists(label => m2 matches label)
      // the mentions must be within the acceptable sentential window
      if Constraints.withinWindow(m1, m2, kWindow)
      // check if mention pair meets corpus constraints
      // make sure mentions can be handled by AssemblyManager
      // could be SimpleEvent, Reg, or Activation...
      if Constraints.isValidRelationPair(m1, m2)
      // EERs must share at least one arg
      if Constraints.shareEntityGrounding(m1, m2)
      // create training instance
      ep = EventPair(Set(m1, m2))
      // triggers should not be the same
      if ep.e1.trigger != ep.e2.trigger
    } yield ep

    distinctEventPairs(eps.toSeq)
  }

  def distinctEventPairs(eps: Seq[EventPair]): Seq[EventPair] = {
    eps.distinct.groupBy(ep =>
      // distinct by...
      (ep.e1.sentence, ep.e2.trigger, ep.e1.label, ep.e1.text, ep.e2.sentence, ep.e2.trigger, ep.e2.label, ep.e2.text)
    ).values.map(_.head) // get one value for each key
      .toSeq
      .sortBy{ ep => (ep.doc.id.getOrElse(""), ep.sentenceIndices.head) }
  }

  def findRedundantEPs(eps: Seq[EventPair], minSeen:Int = 2): Seq[EventPair] = {

    val am = AssemblyManager(eps.flatMap(ep => Seq(ep.e1, ep.e2)))
    def countEquivalentEPs(ep: EventPair): Int = {
      eps.count{ other =>
        // relaxed equivalence (modifications may differ)
        ( am.getEER(other.e1).isEquivalentTo(am.getEER(ep.e1), ignoreMods = true) &&
          am.getEER(other.e2).isEquivalentTo(am.getEER(ep.e2), ignoreMods = true)
        ) ||
        // an equivalent pair may not necessarily appear in the same order
        ( am.getEER(other.e1).isEquivalentTo(am.getEER(ep.e2), ignoreMods = true) &&
          am.getEER(other.e2).isEquivalentTo(am.getEER(ep.e1), ignoreMods = true)
        )
      }
    }

    // filter pairs
    for {
      ep <- eps
      if countEquivalentEPs(ep) >= minSeen
    } yield ep
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

  logger.info(s"Found ${eps.size} examples for relation corpus ...")

  val outDir = new File(config.getString("assembly.corpus.corpusDir"))
  // create corpus and write to file
  val corpus = Corpus(eps)
  corpus.writeJSON(outDir, pretty = false)
}

/**
  * Builds a relation corpus from a serialized dataset. <br>
  * Corpus is written as json
  */
object BuildCorpusWithRedundancies extends App with LazyLogging {

  import CorpusBuilder._

  // corpus constraints
  val skip: Set[String] = config[List[String]]("assembly.corpus.constraints.skip").toSet
  val minSeen = config[Int]("assembly.corpus.constraints.minSeen")

  val jsonFiles: Seq[File] = new File(config.getString("assembly.corpus.jsonDir")).listFiles.toSeq

  import ai.lum.common.RandomUtils._

  val random = new LumAICommonRandomWrapper(new scala.util.Random(42L))

  val sampleSize = 1000

  val threadLimit: Int = config[Int]("threadLimit")

  logger.info(s"skipping ${skip.size} files")
  logger.info(s"minSeen:\t$minSeen")
  logger.info(s"Using $threadLimit threads")
  logger.info(s"Valid labels: $validLabels")
  logger.info(s"Sample size: $sampleSize")


  val sampledFiles = ThreadUtils.parallelize(random.sample[File, Seq](jsonFiles, sampleSize, withReplacement = false), threadLimit)
  // prepare corpus
  logger.info(s"Loading dataset ...")

  val eps: Seq[EventPair] = sampledFiles.flatMap{ f =>
    val cms = ReachJSONSerializer.toCorefMentions(f)
    val paperID = getPMID(cms.head)
    // should this paper be skipped?
    skip.contains(paperID) match {
      case false =>
        val candidateEPs = selectEventPairs(cms)
        val validPairs = findRedundantEPs(candidateEPs, minSeen)
        if (validPairs.nonEmpty) logger.info(s"Found ${validPairs.size} valid pairs in $paperID")
        validPairs
      case true => Nil
    }
  }.seq

  logger.info(s"Found ${eps.size} examples for relation corpus ...")

  val outDir: File = config[File]("assembly.corpus.corpusDir")
  // create corpus and write to file
  val corpus = Corpus(eps)
  corpus.writeJSON(outDir, pretty = false)
}