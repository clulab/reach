package edu.arizona.sista.assembly

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.assembly.sieves.{AssemblySieve, Sieves}
import edu.arizona.sista.odin.{RelationMention, EventMention, TextBoundMention, Mention}
import edu.arizona.sista.reach.PaperReader
import edu.arizona.sista.reach.PaperReader.Dataset
import edu.arizona.sista.utils.Serializer
import edu.arizona.sista.assembly.relations.{CorpusReader, PrecedenceAnnotation}

import scala.annotation.tailrec


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

    val sieves = new Sieves(mentions)

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(sieves.trackMentions) andThen
        // find precedence relations using rules
        AssemblySieve(sieves.ruleBasedPrecedence) andThen
        AssemblySieve(sieves.tamPrecedence) andThen
        AssemblySieve(sieves.intersententialPrecedence)

    // apply the sieves and return the manager
    val am: AssemblyManager = orderedSieves.apply(mentions)

    am
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

  import edu.arizona.sista.assembly.relations.CorpusReader._
  import edu.arizona.sista.assembly.AssemblyRunner._

  /** Retrieve trigger from Mention */
  @tailrec
  def findTrigger(m: Mention): TextBoundMention = m match {
    case event: EventMention =>
      event.trigger
    case rel: RelationMention if (rel matches "ComplexEvent") && rel.arguments("controlled").nonEmpty =>
      // could be nested ...
      findTrigger(rel.arguments("controlled").head)
  }

  def findMention(mns: Seq[Mention], label: String, triggerText: String): Mention = {
    mns.filter{ m =>
      // label and trigger text should match
      (m matches label) && (findTrigger(m).text == triggerText)
    }.head
  }

  val config = ConfigFactory.load()
  val annotationsPath = config.getString("assembly.corpusFile")
  val annotations: Seq[PrecedenceAnnotation] = annotationsFromFile(annotationsPath)
  // gather precedence relations corpus
  val precedenceAnnotations = CorpusReader.filterRelations(annotations, precedenceRelations)
  val noneAnnotations = CorpusReader.filterRelations(annotations, noRelations)

  val posGold: Set[PrecedenceRelation] = (for {
    anno <- precedenceAnnotations
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
    goldRel
  }).flatten.toSet

  val testMentions = (for {
    anno <- precedenceAnnotations ++ noneAnnotations
    e1e2 = getE1E2(anno)
    if e1e2.nonEmpty
    (e1, e2) = e1e2.get
  } yield Seq(e1, e2)).flatten

  val predicted = applySieves(testMentions).getPrecedenceRelations

  val tp = predicted.count(p => posGold exists(g => g.isEquivalentTo(p)))
  val fp = predicted.count(p => ! posGold.exists(g => g.isEquivalentTo(p)))
  val fn = posGold.count(g => ! predicted.exists(p => p.isEquivalentTo(g)))
  val tn = noneAnnotations.length - predicted.size

  println(s"tp: $tp\nfp: $fp\ntn: $tn\nfn: $fn")

}