package org.clulab.reach.assembly.relations

import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}
import org.clulab.odin._
import org.clulab.reach.assembly.{AssemblyManager, PrecedenceRelation}
import org.clulab.reach.assembly.sieves.{AssemblySieve, DeduplicationSieves, PrecedenceSieves, SieveUtils}
import SieveUtils._


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


  def evaluateRBSieves(mentions: Seq[Mention], goldData: Seq[EventPair]): Seq[Performance] = {
    // TODO: evaluate precedence labels

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
//        // find precedence using TAM rules
//        AssemblySieve(precedence.reichenbachPrecedence) andThen
        AssemblySieve(precedence.discourseRBPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    ???
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
      //"intrasententialRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.intrasententialRBPrecedence)),
      //"intersententialRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.intersententialRBPrecedence)),
      "discourseRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.discourseRBPrecedence))
    )

    val ams = for {
      (lbl, s) <- availableSieves.par
      am = s.apply(mentions)
    } yield lbl -> am

    ams.seq //++ Map("all" -> applySieves(mentions))
  }


  def evaluateSieves(posGold: Seq[PrecedenceRelation], results: Map[String, AssemblyManager]): Seq[Performance] = {

    println("sieve\trule\tp\tr\tf1\ttp\tfp\tfn")

    val performanceOfEachSieve = for {
      (lbl, sieveResult) <- results
    } yield {
      val predicted = sieveResult.getPrecedenceRelations
      val smoothing = 0.00001
      val tp = predicted.count(p => posGold exists(g => g.isEquivalentTo(p)))
      val fp = predicted.count(p => ! posGold.exists(g => g.isEquivalentTo(p)))
      val fn = posGold.count(g => ! predicted.exists(p => p.isEquivalentTo(g)))

      // micro performance
      val p = tp / (tp + fp + smoothing)
      val r = tp / (tp + fn + smoothing)
      val f1 = (2 * p * r) / (p + r + smoothing)

      // for the whole sieve
      val sievePerformance = Performance(lbl, "**ALL**", p, r, f1, tp, fp, fn)

      val rulePerformance: Seq[Performance] = {
        val rulePs = predicted.groupBy(pr => (pr.foundBy, pr.evidence.head.foundBy))
        val allRtp = rulePs.mapValues(_.count(p => posGold exists(g => g.isEquivalentTo(p))))
        val allRfp = rulePs.mapValues{_.count{p =>
          val isFP = ! posGold.exists(g => g.isEquivalentTo(p))
          //if(isFP) displayMention(p.evidence.head)
          isFP
        }
        }
        val allRfn = {
          val res = for {
            (foundBy, group) <- rulePs
            gold = posGold.count(g => ! group.exists(p => p.isEquivalentTo(g)))
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

object RunRBSieveEval extends App {

  import SieveEvaluator._

  val corpusDir = "/Users/gus/repos/reach-resources/wip"
  val corpus = Corpus(corpusDir)

  val sieveResults = SieveEvaluator.applyEachSieve(corpus.mentions)
  val preformanceForEachSieve = SieveEvaluator.evaluateSieves(corpus.precedenceRelations, sieveResults)

}
