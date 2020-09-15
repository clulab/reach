package org.clulab.reach.assembly

import org.apache.commons.io.FilenameUtils
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}
import org.clulab.odin.Mention
import org.clulab.reach.PaperReader
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json._
import org.clulab.utils.Serializer

import scala.collection.parallel.ForkJoinTaskSupport
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import java.io.File

import org.clulab.reach.assembly.relations.SieveEvaluator
import org.clulab.reach.assembly.relations.SieveEvaluator.Performance


object RunAnnotationEval extends App with LazyLogging {

  val config = ConfigFactory.load()
//  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDir")).instances

  // gather precedence relations corpus
  val evalGoldPath = config.getString("assembly.evalGold")
  val evalMentionsPath = config.getString("assembly.evalMentions")

  val (posGold, testMentions) = {

    if(new File(evalGoldPath).exists & new File(evalMentionsPath).exists) {
      logger.info("Serialized files exist")
      val pg = Serializer.load[Seq[PrecedenceRelation]](evalGoldPath)
      val tm = Serializer.load[Seq[Mention]](evalMentionsPath)
      (pg, tm)
    } else {
      logger.info("Serialized files not found")
      val epsOld: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirOldTrain")).instances
      val newMentions = Corpus.loadMentions(config.getString("assembly.corpus.corpusDirNewTrain"))
      val eps = Corpus.softAlign(epsOld, newMentions)
      // gather precedence relations corpus
//      val precedenceAnnotations = CorpusReader.filterRelations(eps, precedenceRelations)
//      val noneAnnotations = CorpusReader.filterRelations(eps, noRelations ++ subsumptionRelations ++ equivalenceRelations)

      val (posGoldNested, testMentionsNested) = (for {
        ep <- eps
        e1 = ep.e1
        e2 = ep.e2
      } yield {
        // short-term assembly manager to get at mentions easier
        val am = AssemblyManager()
        am.trackMentions(Seq(e1, e2))
        val goldRel = ep.relation match {
          case "E1 precedes E2" =>
            Seq(PrecedenceRelation(am.getEER(e1), am.getEER(e2), Set.empty[Mention], "gold"))
          case "E2 precedes E1" =>
            Seq(PrecedenceRelation(am.getEER(e2), am.getEER(e1), Set.empty[Mention], "gold"))
          case _ => Nil
        }
        (goldRel, Seq(e1, e2))
      }).unzip

      val pg = posGoldNested.flatten.seq
      val tm = testMentionsNested.flatten.distinct.seq

      (pg, tm)
    }
  }

  println("sieve\trule\tp\tr\tf1\ttp\tfp\tfn")

  for {
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(testMentions)
  } {
    val predicted = sieveResult.getPrecedenceRelations
    val smoothing = 0.00001
    val tp = predicted.count(p => posGold exists(g => g.isEquivalentTo(p, ignoreMods = false)))
    val fp = predicted.count(p => ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = false)))
    val fn = posGold.count(g => ! predicted.exists(p => p.isEquivalentTo(g, ignoreMods = false)))

    // micro performance
    val p = tp / (tp + fp + smoothing)
    val r = tp / (tp + fn + smoothing)
    val f1 = (2 * p * r) / (p + r + smoothing)

    // for the whole sieve
    val sievePerformance = Performance(lbl, "**ALL**", p, r, f1, tp, fp, fn)

    val rulePerformance: Seq[Performance] = {
      val rulePs = predicted.groupBy(pr => (pr.foundBy, pr.evidence.head.foundBy))
      val allRtp = rulePs.mapValues(_.count(p => posGold exists(g => g.isEquivalentTo(p, ignoreMods = false))))
      val allRfp = rulePs.mapValues{_.count{p =>
        val isFP = ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = false))
        //if(isFP) displayMention(p.evidence.head)
        isFP
      }
      }
      val allRfn = {
        val res = for {
          (foundBy, group) <- rulePs
          gold = posGold.count(g => ! group.exists(p => p.isEquivalentTo(g, ignoreMods = false)))
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

    (rulePerformance :+ sievePerformance).sortBy(_.p).foreach(perf => println(perf.mkRow))
  }
}

/**
  * Serialize each paper in a directory to json
  */
object SerializePapersToJSON extends App with LazyLogging {

  import org.clulab.reach.mentions.serialization.json._

  val config = ConfigFactory.load()
  val papersDir = new File(config.getString("papersDir"))
  val outDir = new File(config.getString("outDir"))
  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")

  logger.info(s"papersDir: ${papersDir.getAbsolutePath}")
  logger.info(s"outDir: ${outDir.getAbsolutePath}")
  logger.info(s"threads: $threadLimit")
  val papers = papersDir.listFiles.par
  papers.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))

  for {
    paper <- papers
    paperID = FilenameUtils.removeExtension(paper.getName)
    outFile = new File(s"$outDir/$paperID.json")
    if !outFile.exists
  } {
    val mentions = PaperReader.getMentionsFromPaper(paper)
    val cms: Seq[CorefMention] = mentions.map(_.toCorefMention)
    logger.info(s"extracted ${mentions.size} mentions for $paperID")
    cms.saveJSON(outFile, pretty = true)
    logger.info(s"saved json to $outFile")
  }
}