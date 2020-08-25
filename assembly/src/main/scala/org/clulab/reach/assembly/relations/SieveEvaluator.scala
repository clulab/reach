package org.clulab.reach.assembly.relations

import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.relations.corpus.{ AnnotationUtils, Corpus, CorpusReader, EventPair }
import org.clulab.odin._
import org.clulab.reach.assembly.{ AssemblyManager, PrecedenceRelation }
import org.clulab.reach.assembly.sieves._
import SieveUtils._
import ai.lum.common.FileUtils._
import ai.lum.common.RandomUtils._
import com.typesafe.scalalogging.LazyLogging
import java.io.File


object SieveEvaluator {

  case class Performance (sieve: String, rule: String, p: Double, r: Double, f1: Double, tp: Int, fp: Int, fn: Int) {
    def mkRow = f"$sieve\t$rule\t$p%1.3f\t$r%1.3f\t$f1%1.3f\t$tp\t$fp\t$fn"
  }

  // see RunAnnotationEval

  val config = ConfigFactory.load()
  lazy val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDir")).instances

  implicit class EventPairOps(ep: EventPair) {

    def eventPairToEvidence: Set[Mention] = ep match {

      case prec if SieveUtils.precedenceRelations contains prec.relation =>
        val precedenceMentionLabels = Seq(SieveUtils.precedenceMentionLabel)

        val args = prec.relation match {
          case `E1PrecedesE2` =>
            Map[String, Seq[Mention]](SieveUtils.beforeRole -> Seq(ep.e1), SieveUtils.afterRole -> Seq(ep.e2))
          case `E2PrecedesE1` =>
            Map[String, Seq[Mention]](SieveUtils.beforeRole -> Seq(ep.e2), SieveUtils.afterRole -> Seq(ep.e1))
        }

        val mention = ep.e1.sentence == ep.e2.sentence match {

          case true => new RelationMention(
            precedenceMentionLabels,
            args,
            ep.e1.sentence,
            ep.e1.document,
            true,
            "<MANUAL>"
          )

          case false =>
            new CrossSentenceMention(
              labels = precedenceMentionLabels,
              anchor = ep.e1,
              neighbor = ep.e2,
              arguments = args,
              document = ep.e1.document,
              keep = true,
              foundBy = "<MANUAL>"
          )
      }
        Set(mention)

      case _ => Set.empty
    }
  }

  implicit class CorpusOps(corpus: Corpus) {

    def precedenceRelations: Seq[PrecedenceRelation] = for {
      ep <- corpus.instances
      e1 = ep.e1
      e2 = ep.e2
      // is this a precedence relation?
      if SieveUtils.precedenceRelations contains ep.relation
    } yield {
      val am = AssemblyManager()
      am.trackMentions(Seq(e1, e2))

      ep.relation match {
        case `E1PrecedesE2` =>
          PrecedenceRelation(am.getEER(e1), am.getEER(e2), Set.empty[Mention], "gold")
        case `E2PrecedesE1` =>
          PrecedenceRelation(am.getEER(e2), am.getEER(e1), Set.empty[Mention], "gold")
      }
    }

    def filterPRsByText(text: String): Seq[(String, String, PrecedenceRelation)] = precedenceRelations
      .filter(_.before.sourceMention.get.sentenceObj.getSentenceText contains text)
      // get (before's label, after's label, and PR)
      .map(pr => (pr.before.sourceMention.get.label, pr.after.sourceMention.get.label, pr))
  }

  /**
    * Applies each Assembly Sieve to mentions and returns and updated AssemblyManager for each.
    *
    * @param mentions a Seq of Odin Mentions
    * @return an AssemblyManager
    */
  def applyEachSieve(mentions: Seq[Mention]): Map[String, AssemblyManager] = {

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val availableSieves: Map[String, AssemblySieve] = Map(
      //"reichenbachPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.reichenbachPrecedence)),
      "bioDRBpatterns" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.bioDRBpatterns)),
      "combinedRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.combinedRBPrecedence))
    )

    val ams = for {
      (lbl, s) <- availableSieves.par
      am = s.apply(mentions)
    } yield lbl -> am

    ams.seq //++ Map("all" -> applySieves(mentions))
  }


  def summarizePrecedenceRelations(pr: Seq[PrecedenceRelation]): String = {
    val crossSentenceCount = pr.count(pr => pr.before.sourceMention.get.sentence != pr.after.sourceMention.get.sentence)
    val eventPairs = pr.map{ pr =>
      val before = pr.before.sourceMention
      val after = pr.after.sourceMention

      (before, after) match {

        case (Some(b), Some(a)) =>
          val rm = AssemblyActions.mkPrecedenceMention(b, a, pr.evidence.toSeq.map(_.foundBy).mkString(", "))
          AssemblyActions.summarizeBeforeAfter(rm)

        case other =>
          s"""NO EVIDENCE FOUND
              |${pr.before.toString}
              |${pr.after.toString}
          """.stripMargin
      }
    }.mkString("\n")

    s"""TOTAL:\t${pr.size}
       |-------------------------------------
       |# CROSS-SENTENCE:\t$crossSentenceCount
       |# INTRASENTENCE:\t${pr.size - crossSentenceCount}
       |
       |$eventPairs
     """.stripMargin
  }

  def evaluateSieves(posGold: Seq[PrecedenceRelation], results: Map[String, AssemblyManager]): Seq[Performance] = {

    println("sieve\trule\tp\tr\tf1\ttp\tfp\tfn")

    val performanceOfEachSieve = for {
      (lbl, sieveResult) <- results
    } yield {
      val predicted = sieveResult.getPrecedenceRelations
      val smoothing = 0.00001
      val im = false
      // get the sets of PrecedenceRelations corresponding to tp, fp, and fn
      val tp = predicted.filter(p => posGold exists(g => g.isEquivalentTo(p, ignoreMods = im)))
      val fp = predicted.filter(p => ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = im)))
      val fn = posGold.filter(g => ! predicted.exists(p => p.isEquivalentTo(g, ignoreMods = im)))
      // ensure there is no overlap
      require(tp.toSet.intersect(fn.toSet).size == 0, s"TP and FN for $lbl must not intersect")

      // micro performance
      val p = tp.size / (tp.size + fp.size + smoothing)
      val r = tp.size / (tp.size + fn.size + smoothing)
      val f1 = (2 * p * r) / (p + r + smoothing)

      // for the whole sieve
      val sievePerformance = Performance(lbl, "**ALL**", p, r, f1, tp.size, fp.size, fn.size)

      // write true positives, false positives, and false negatives to files
      new File(s"$lbl-sieve-true-positives.txt").writeString(summarizePrecedenceRelations(tp.toSeq), java.nio.charset.StandardCharsets.UTF_8)
      new File(s"$lbl-sieve-false-positives.txt").writeString(summarizePrecedenceRelations(fp.toSeq), java.nio.charset.StandardCharsets.UTF_8)
      new File(s"$lbl-sieve-false-negatives.txt").writeString(summarizePrecedenceRelations(fn), java.nio.charset.StandardCharsets.UTF_8)

      val rulePerformance: Seq[Performance] = {
        val rulePs = predicted.groupBy(pr => (pr.foundBy, pr.evidence.head.foundBy))
        val allRtp = rulePs.mapValues(_.count(p => posGold exists(g => g.isEquivalentTo(p, ignoreMods = im))))
        val allRfp = rulePs.mapValues{_.count{p =>
          val isFP = ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = im))
          //if(isFP) displayMention(p.evidence.head)
          isFP
        }
        }
        val allRfn = {
          val res = for {
            (foundBy, group) <- rulePs
            gold = posGold.count(g => ! group.exists(p => p.isEquivalentTo(g, ignoreMods = im)))
          } yield (foundBy, gold)
          res
        }

        val rp = for {
          foundBy <- rulePs.keys
        } yield {
          val tp = allRtp.getOrElse(foundBy, 0)
          val fp = allRfp.getOrElse(foundBy, 0)
          val fn = allRfn.getOrElse(foundBy, 0)

          // micro performance
          val p = tp / (tp + fp + smoothing)
          val r = tp / (tp + fn + smoothing)
          val f1 = (2 * p * r) / (p + r + smoothing)

          // for the rule
          Performance (foundBy._1, foundBy._2, p, r, f1, tp, fp, fn)
        }
        rp.toSeq
      }

      (rulePerformance.sortBy(_.rule) :+ sievePerformance).foreach(perf => println(perf.mkRow))
      println()
      sievePerformance
    }

    performanceOfEachSieve.toSeq
  }

}

object RunRBSieveAnalysis extends App with LazyLogging {

  import SieveEvaluator._
  import ai.lum.common.ConfigUtils._

  val config = ConfigFactory.load()
  val corpusDir: String = config[String]("assembly.corpus.corpusDir")
  logger.info(s"Reading corpus from $corpusDir")
  val corpus = Corpus(corpusDir)
  logger.info("Applying sieves")
  val sieveResults = SieveEvaluator.applyEachSieve(corpus.mentions)
  val preformanceForEachSieve = SieveEvaluator.evaluateSieves(corpus.precedenceRelations, sieveResults)

}

object GenerateRBSieveScoreFiles extends App with LazyLogging {

  import org.apache.commons.io.FilenameUtils
  import org.clulab.reach.assembly.relations.classifier.CrossValidateAssemblyRelationClassifier.{logger => _, _}
  import org.clulab.reach.assembly.relations.classifier.{Evaluator, LabelPair}

  val config = ConfigFactory.load()

  def evaluateRuleBasedSieves(eps: Seq[EventPair]): Unit = {

    val results = config.getString("assembly.classifier.results")

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val availableSieves: Map[String, AssemblySieve] = Map(
      "reichenbachPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.reichenbachPrecedence)),
      //"bioDRBpatterns" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.bioDRBpatterns)),
      "combinedRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.combinedRBPrecedence))
    )

    val WRONG = "WRONG"
    for ((sieveName, s) <- availableSieves) {
      // evaluate each sieve
      val scores: Seq[LabelPair[String]] = for {
        (ep, i) <- eps.zipWithIndex
        if Constraints.isValidRelationPair(ep.e1, ep.e2)
        mentions = Seq(ep.e1, ep.e2)
        am = s.apply(mentions)
      } yield {
        ep.relation match {
          case `NEG` =>
            val label = if ((!am.successorsOf(ep.e2).exists(eer => eer.isEquivalentTo(ep.e1, ignoreMods = false))) && (!am.successorsOf(ep.e1).exists(eer => eer.isEquivalentTo(ep.e2, ignoreMods = false)))) NEG else WRONG
            LabelPair[String](i, gold = ep.relation, predicted = label)
          case `E2PrecedesE1` =>
            val label = if (am.successorsOf(ep.e2).exists(eer => eer.isEquivalentTo(ep.e1, ignoreMods = false))) E2PrecedesE1 else WRONG
            LabelPair[String](i, gold = ep.relation, predicted = label)
          case `E1PrecedesE2` =>
            val label = if (am.successorsOf(ep.e1).exists(eer => eer.isEquivalentTo(ep.e2, ignoreMods = false))) E1PrecedesE2 else WRONG
            LabelPair[String](i, gold = ep.relation, predicted = label)
        }
      }
      val performance = Evaluator.calculatePerformance(scores)
      // save results to file
      val outFile = s"${FilenameUtils.removeExtension(results)}-$sieveName.${FilenameUtils.getExtension(results)}"
      logger.info(s"writing '$sieveName' scores to $outFile")
      Evaluator.writeScoresToTSV(scores, outFile)
    }
  }

  // gather precedence relations corpus
  val precedenceAnnotations = CorpusReader.filterRelations(SieveEvaluator.eps, precedenceRelations)

  // evaluate rule-based sieves
  evaluateRuleBasedSieves(precedenceAnnotations)
}

/**
  * Apply rules to documents and produce corpus of likely precedence relations
  */
object ApplyRulesToDocuments extends App with LazyLogging {

  import org.clulab.reach.mentions._
  import ai.lum.common.ConfigUtils._
  import ai.lum.common.RandomUtils._
  import org.clulab.reach.assembly.relations.corpus._
  import org.clulab.reach.assembly.relations.corpus.CorpusBuilder.selectEventPairs
  import org.clulab.reach.mentions.serialization.json.{ JSONSerializer => ReachJSONSerializer }
  import scala.collection.parallel.ForkJoinTaskSupport


  val dedup = new DeduplicationSieves()
  val precedence = new PrecedenceSieves()
  val rbSieve = AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.combinedRBPrecedence)

  /**
    * Convert precedence relations to EventPairs
    * @param prs a Seq[PrecedenceRelation]
    * @return a Seq[EventPair]
    */
  def precedenceRelationsToEventPairs(prs: Seq[PrecedenceRelation]): Seq[EventPair] = for {
    pr <- prs
    e <- pr.evidence
    if e.arguments contains "before"
    if e.arguments contains "after"
    // create before/after pairs
    b <- e.arguments("before")
    a <- e.arguments("after")
  } yield {
    val mns = Seq(b,a).sortWith((m1, m2) => m1 precedes m2)
    val before = mns.head
    val after = mns.last

    EventPair(
      e1 = before.toCorefMention,
      e2 = after.toCorefMention,
      relation = if (b precedes a) E1PrecedesE2 else E2PrecedesE1,
      confidence = AnnotationUtils.HIGH,
      annotatorID = e.foundBy,
      notes = Some(e.foundBy)
    )
  }

  /**
    * Note that though this will work, it is an expensive way to apply the rules
    */
  def getPrecedenceRelations(cms: Seq[CorefMention]): Seq[PrecedenceRelation] = {
    val eps = selectEventPairs(cms)
    for {
      ep <- eps
      mns = Seq(ep.e1, ep.e2)
      am = rbSieve.apply(mns)
      pr <- am.getPrecedenceRelations
    } yield pr
  }

  val config = ConfigFactory.load()

  // corpus constraints
  // avoid this papers
  val skip: Set[String] = config[List[String]]("assembly.corpus.constraints.skip").toSet
  val kWindow = config.getInt("assembly.windowSize")
  val validLabels: Set[String] = config[List[String]]("assembly.corpus.validLabels").toSet

  val jsonFiles: Seq[File] = new File(config.getString("assembly.corpus.jsonDir")).listFiles.toSeq

  val random = new scala.util.Random(42L)

  val sampleSize = 1000

  val threadLimit: Int = config[Int]("threadLimit")

  logger.info(s"skipping ${skip.size} files")
  logger.info(s"Using $threadLimit threads")
  logger.info(s"Valid labels: $validLabels")
  logger.info(s"Sample size: $sampleSize")


  val sampledFiles = random.sample[File, Seq](jsonFiles, sampleSize, withReplacement = false).par
  // prepare corpus
  logger.info(s"Loading dataset ...")

  sampledFiles.tasksupport =
    new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))

  val eps: Seq[EventPair] = sampledFiles.flatMap{ f =>
    val cms = ReachJSONSerializer.toCorefMentions(f)
    val paperID = getPMID(cms.head)
    // should this paper be skipped?
    skip.contains(paperID) match {
      case false =>
        val prs = getPrecedenceRelations(cms)
        val candidateEPs = precedenceRelationsToEventPairs(prs)
        logger.info(s"Rules found ${candidateEPs.size} matches for $paperID")
        candidateEPs
      case true => Nil
    }
  }.seq

  logger.info(s"Found ${eps.size} matches from precedence rules ...")

  val outDir: File = config[File]("assembly.corpus.corpusDir")
  logger.info(s"Writing rule-derived pairs to ${outDir.getCanonicalPath}")

  // create corpus and write to file
  val corpus = Corpus(eps)
  corpus.writeJSON(outDir, pretty = false)
}