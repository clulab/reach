package edu.arizona.sista.assembly

import java.io.File
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.nxml.NxmlReader
import edu.arizona.sista.reach.utils.CSVParser
import edu.arizona.sista.reach.{context, ReachSystem}
import edu.arizona.sista.reach.context.ContextEngineFactory.Engine
import edu.arizona.sista.reach.PaperReader.Dataset
import edu.arizona.sista.utils.Serializer
import org.apache.commons.io.FilenameUtils
import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray

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

  // for context engine
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  val contextConfig = config.getConfig("contextEngine.params").root
  val contextEngineParams: Map[String, String] =
    context.createContextEngineParams(contextConfig)

  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")
  val papersDir = config.getString("assembly.papers")
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala

  // systems for reading papers
  val nxmlReader = new NxmlReader(ignoreSections)
  val csvReader = new CSVParser()

  // initialize ReachSystem with appropriate context engine
  val rs = new ReachSystem(contextEngineType = contextEngineType, contextParams = contextEngineParams)

  def readPapers(path: String): Dataset = readPapers(new File(path))

  /**
   * Produces Dataset from a directory of nxml and csv papers.
   * @param dir a directory of nxml and csv papers
   * @return a Dataset (PaperID -> Mentions)
   */
  def readPapers(dir: File): Dataset = {
    require(dir.isDirectory, s"'${dir.getCanonicalPath}' is not a directory")
    // read papers in parallel
    val files = dir.listFiles.par
    // limit parallelization
    files.tasksupport =
      new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))
    // build dataset
    val data: ParArray[(String, Vector[Mention])] = for {
      file <- dir.listFiles.par // read papers in parallel
      // allow either nxml or csv files
      if file.getName.endsWith(".nxml") || file.getName.endsWith(".csv")
    } yield file match {
        case nxml if nxml.getName.endsWith(".nxml") =>
          readNXMLPaper(nxml)
        case csv if csv.getName.endsWith(".csv") =>
          readCSVPaper(csv)
    }
    data.seq.toMap
  }

  /**
   * Produces Mentions from .csv papers using [[CSVParser]] and [[ReachSystem]]
   * @param file a File with the .csv extension
   * @return (PaperID, Mentions)
   */
  def readCSVPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".csv"), s"Given ${file.getAbsolutePath}, but readCSVPaper only handles .csv files!")
    val paperID: String = file.getName.replace(".csv", "")
    println(s"reading paper $paperID ...")
    val mentions: Seq[Mention] =
      rs.extractFrom(csvReader.toFriesEntries(file))
    (paperID, mentions.toVector)
  }

  /**
   * Produces Mentions from .nxml papers using [[NxmlReader]] and [[ReachSystem]]
   * @param file a File with the .csv extension
   * @return (PaperID, Mentions)
   */
  def readNXMLPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".nxml"), s"Given ${file.getAbsolutePath}, but readNXMLPaper only handles .nxml files!")
    val paperID = FilenameUtils.removeExtension(file.getName)
    println(s"reading paper $paperID ...")
    val mentions = for {
      entry <- nxmlReader.readNxml(file)
      mention <- rs.extractFrom(entry)
    } yield mention
    paperID -> mentions.toVector
  }

  // generate Dataset from papers
  val dataset = readPapers(papersDir)
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
