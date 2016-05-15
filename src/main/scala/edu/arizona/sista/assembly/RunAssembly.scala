package edu.arizona.sista.assembly

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.assembly.export.{Row, AssemblyExporter}
import edu.arizona.sista.assembly.relations.corpus.{AssemblyAnnotation, CorpusReader}
import edu.arizona.sista.assembly.sieves._
import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.PaperReader
import edu.arizona.sista.reach.PaperReader.Dataset
import edu.arizona.sista.utils.Serializer
import scala.reflect.io.File


/**
  * Utilities for running assembly sieves on a Dataset and writing their output.
  */
object AssemblyRunner {
  /**
    * Applies Assembly Sieves to mentions and returns and updated AssemblyManager.
    *
    * @param mentions a Seq of Odin Mentions
    * @return an AssemblyManager
    */
  def applySieves(mentions: Seq[Mention]): AssemblyManager = {

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence relations using rules
        AssemblySieve(precedence.withinRbPrecedence) andThen
        AssemblySieve(precedence.reichenbachPrecedence) andThen
        AssemblySieve(precedence.betweenRbPrecedence) andThen
        AssemblySieve(precedence.featureBasedClassifier)

    // apply the sieves and return the manager
    val am: AssemblyManager = orderedSieves.apply(mentions)

    am
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

    val availableSieves = Map(
      "withinRbPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.withinRbPrecedence)),
      "reichenbachPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.reichenbachPrecedence)),
      "betweenRbPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.betweenRbPrecedence))
    )

    val ams = for {
      (lbl, s) <- availableSieves.par
      am = s.apply(mentions)
    } yield lbl -> am

    ams.toMap.seq //++ Map("all" -> applySieves(mentions))
  }

  /**
    * Produces sieve-based assembly output from a serialized dataset.
    *
    * @param serMentions a serialized [[Dataset]]
    * @param outFolder the folder where output is written
    */
  def writeOutputFromSerializedMentions(
    serMentions: String,
    outFolder: String
  ): Unit = {
    // load serialized dataset
    val dataset = Serializer.load[Dataset](serMentions)
    // generate assembly output and write to disk
    writeOutputFromDataset(dataset, outFolder)
  }

  /**
    * Produces sieve-based assembly output from a [[Dataset]]
    *
    * @param dataset a [[Dataset]]
    * @param outFolder the folder where output is written
    */
  def writeOutputFromDataset(dataset: Dataset, outFolder: String): Unit = {
    // write output for each paper
    println(s"Beginning assembly of ${dataset.size} papers ...")
    for {
      (pmid, mentions) <- dataset
    } {
      try {
        val am = applySieves(mentions)
        val ae = new AssemblyExporter(am)
        val outFile = s"$outFolder/$pmid-assembly-out.tsv"
        val outFile2 = s"$outFolder/$pmid-assembly-out-unconstrained.tsv"
        ae.writeTSV(outFile, AssemblyExporter.MITREfilter)
        println(s"Wrote assembly output for $pmid to $outFile")
        ae.writeTSV(outFile2, (rows: Set[Row]) => rows.filter(_.seen > 0))
        println(s"Wrote assembly output for $pmid to $outFile2")
      } catch {
        case e: Exception =>
          println(s"Error processing $pmid")
          println(e.printStackTrace)
      }
    }

    // get all mentions (across papers)
    val mentions = dataset.values.flatten.toSeq
    val am = applySieves(mentions)
    val ae = new AssemblyExporter(am)
    val outFile = s"$outFolder/across-papers-assembly-out.tsv"
    ae.writeTSV(outFile, AssemblyExporter.MITREfilter)
    println(s"Wrote cross-paper assembly output to $outFile")

    // get all mentions (across papers, where event has non-zero evidence)
    val am2 = applySieves(mentions)
    val ae2 = new AssemblyExporter(am2)
    val outFile2 = s"$outFolder/across-papers-assembly-out-no-constraints.tsv"
    ae2.writeTSV(outFile2, (rows: Set[Row]) => rows.filter(_.seen > 0))
    println(s"Wrote cross-paper assembly output to $outFile2")
  }
}

/**
  * Runnable for producing sieve-based assembly output from a directory of papers (.csv or .nxml files)
  */
object RunAssembly extends App {

  import AssemblyRunner._

  val config = ConfigFactory.load()
  val outFolder = config.getString("assembly.outFolder")

  val papersDir = config.getString("assembly.papers")

  // generate Dataset from papers
  val dataset = PaperReader.readPapers(papersDir)
  // write assembly output files to directory
  writeOutputFromDataset(dataset, outFolder)
}

/**
  * Runnable for producing sieve-based assembly output from a serialized [[Dataset]].
  */
object AssembleFromDataset extends App {

  import AssemblyRunner._

  val config = ConfigFactory.load()
  val outFolder = config.getString("assembly.outFolder")
  val serMentionsPath = config.getString("assembly.serializedDataset")

  writeOutputFromSerializedMentions(serMentionsPath, outFolder)
}

object RunAnnotationEval extends App {

  import edu.arizona.sista.assembly.AssemblyRunner._
  import CorpusReader._
  import edu.arizona.sista.reach.display._

  case class Performance (sieve: String, rule: String, p: Double, r: Double, f1: Double, tp: Int, fp: Int, fn: Int) {
    def mkRow = f"$sieve\t$rule\t$p%1.3f\t$r%1.3f\t$f1%1.3f\t$tp\t$fp\t$fn"
  }

  val config = ConfigFactory.load()
  val evalGoldPath = config.getString("assembly.evalGold")
  val evalMentionsPath = config.getString("assembly.evalMentions")

  val (posGold, testMentions) = {

    if(File(evalGoldPath).exists & File(evalMentionsPath).exists) {
      println("Serialized files exist")
      val pg = Serializer.load[Seq[PrecedenceRelation]](evalGoldPath)
      val tm = Serializer.load[Seq[Mention]](evalMentionsPath)
      (pg, tm)
    } else {
      println("Serialized files not found")
      val annotationsPath = config.getString("assembly.corpusFile")
      val annotations: Seq[AssemblyAnnotation] = annotationsFromFile(annotationsPath)
      // gather precedence relations corpus
      val precedenceAnnotations = CorpusReader.filterRelations(annotations, precedenceRelations)
      val noneAnnotations = CorpusReader.filterRelations(annotations, noRelations ++ subsumptionRelations ++ equivalenceRelations)

      val (posGoldNested, testMentionsNested) = (for {
        anno <- precedenceAnnotations.par
        e1e2 = getE1E2(anno)
        if e1e2.nonEmpty
      } yield {
        val (e1, e2) = e1e2.get
        // short-term assembly manager to get at mentions easier
        val am = AssemblyManager()
        am.trackMentions(Seq(e1, e2))
        val goldRel = anno.relation match {
          case "E1 precedes E2" =>
            Seq(PrecedenceRelation(am.getEER(e1).equivalenceHash, am.getEER(e2).equivalenceHash, Set.empty[Mention], "gold"))
          case "E2 precedes E1" =>
            Seq(PrecedenceRelation(am.getEER(e2).equivalenceHash, am.getEER(e1).equivalenceHash, Set.empty[Mention], "gold"))
          case _ => Nil
        }
        (goldRel, Seq(e1, e2))
      }).unzip

      val pg = posGoldNested.flatten.seq
      val tm = testMentionsNested.flatten.distinct.seq

      Serializer.save[Seq[PrecedenceRelation]](pg, evalGoldPath)
      Serializer.save[Seq[Mention]](tm, evalMentionsPath)

      (pg, tm)
    }
  }

  println("sieve\trule\tp\tr\tf1\ttp\tfp\tfn")

  for {
    (lbl, sieveResult) <- applyEachSieve(testMentions)
  } {
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
        res.toMap
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