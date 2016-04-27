package edu.arizona.sista.assembly

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.assembly.sieves.{AssemblySieve, Sieves}
import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.PaperReader
import edu.arizona.sista.reach.PaperReader.Dataset
import edu.arizona.sista.utils.Serializer

/**
 * Utilities for running assembly sieves on a Dataset and writing their output.
 */
object AssemblyRunner {
  /**
   * Applies Assembly Sieves to mentions and returns and updated AssemblyManager.
   * @param mentions a Seq of Odin Mentions
   * @return an AssemblyManager
   */
  def applySieves(mentions: Seq[Mention]): AssemblyManager = {

    val sieves = new Sieves(mentions)

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(sieves.trackMentions) andThen
        // find precedence relations using rules
        AssemblySieve(sieves.ruleBasedPrecedence)

    // apply the sieves and return the manager
    val am: AssemblyManager = orderedSieves.apply(mentions)

    am
  }

  /**
   * Produces sieve-based assembly output from a serialized dataset.
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
