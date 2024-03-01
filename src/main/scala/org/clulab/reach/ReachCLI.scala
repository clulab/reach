package org.clulab.reach

import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.{FileUtils, FilenameUtils}

import java.io.{BufferedReader, File, FileInputStream}
import java.util.Date
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import ai.lum.common.FileUtils._
import ai.lum.common.ConfigUtils._
import org.clulab.reach.RuleReader.readResource
import org.clulab.reach.`export`.VisualAnalyticsDataExporter
//import jline.internal.InputStreamReader
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.reach.`export`.arizona.ArizonaOutputter
import org.clulab.reach.`export`.cmu.CMUExporter
import org.clulab.reach.assembly._
import org.clulab.reach.assembly.export.{AssemblyExporter, AssemblyRow, ExportFilters}
import org.clulab.reach.export.OutputDegrader
import org.clulab.reach.export.TrainingDataExporter
import org.clulab.reach.export.fries.FriesOutput
import org.clulab.reach.export.indexcards.IndexCardOutput
import org.clulab.reach.export.serial.SerialJsonOutput
import org.clulab.reach.mentions.CorefMention
import org.clulab.reach.utils.MentionManager
import org.clulab.utils.Serializer

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Date
import scala.collection.JavaConverters._

/**
  * Class to run Reach reading and assembly and then produce FRIES format output
  * from a group of input files.
  *   Written by: Gus Hahn-Powell and Tom Hicks. 5/9/2016.
  *   Last Modified: 
  *   - 12/28/2020: Added assembly back: we need causal precedence and the `arizona` format 
  *   - Remove assembly functionality (including output formats relying on assembly).
  */
class ReachCLI (
  override val papersDir: File,
  override val outputDir: File,
  val outputFormats: Seq[String],
  override val statsKeeper: ProcessingStats = new ProcessingStats,
  override val encoding: Charset = UTF_8,
  override val restartFile: Option[File] = None
) extends CLI(papersDir, outputDir, statsKeeper, encoding, restartFile) {


  /** Lock file object for restart file. */
  private val restartFileLock = new AnyRef

  def prepareMentionsForMITRE (mentions: Seq[Mention]): Seq[CorefMention] = {
    // NOTE: We're already doing this in the exporter, but the mentions given to the
    // Assembler probably need to match since flattening results in a loss of information
    OutputDegrader.prepareForOutput(mentions)
  }

  def doAssembly (mns: Seq[Mention]): Assembler = Assembler(mns)

  def deserializeDoc(file: File) = {
    val br = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    val entryStr = new StringBuilder()

    var line = br.readLine()
    while (line != "#FS#") {
      entryStr ++= line + "\n"
      line = br.readLine()
    }

    val tokens = entryStr.toString.split('\t')

    // If we don't have the paper id available, take if from the paper
    val paperId =
      file.getName.split("\\.").dropRight(1).mkString(".")


    val entry = FriesEntry.mkFriesEntry(paperId, tokens.last)

    val serializer = new ReachDocumentSerializer
    val doc = serializer.load(br)
    br.close()
    doc.id = Some(entry.name)
    doc.text = Some(entry.text)
    (entry, doc)
  }

  // Returns count of outputFormats which errored.
  override def processPaper (file: File, withAssembly: Boolean): Int = {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startNS = System.nanoTime
    val startTime = ReachCLI.now

    val isSerialized = file.getExtension() == "ser"

    logger.info(s"$startTime: Starting $paperId")
    logger.debug(s"  ${ durationToS(startNS, System.nanoTime) }s: $paperId: started reading")

    val (entry, mentions) =
      if(isSerialized){
        // Load pre-annotated objects
        val (entry: FriesEntry, doc: Document) = deserializeDoc(file)
        val mentions = PaperReader.reachSystem.extractFrom(doc)
        (entry, mentions)
      }
      else{
        // entry must be kept around for outputter
        val entry = PaperReader.getEntryFromPaper(file)
        val mentions = PaperReader.getMentionsFromEntry(entry)
        (entry, mentions)
      }


    logger.debug(s"  ${ durationToS(startNS, System.nanoTime) }s: $paperId: finished reading")

    // generate outputs
    // NOTE: Assembly can't be run before calling this method without additional refactoring,
    // as different output formats apply different filters before running assembly
    val errorCount = outputFormats
      .map { outputFormat =>
        try {
          outputMentions(mentions, entry, paperId, startTime, outputDir, outputFormat, withAssembly)
          0
        }
        catch {
          case throwable: Throwable =>
            reportException(file, throwable)
            1
        }
      }.sum

    // elapsed time: processing + writing output
    val endTime = ReachCLI.now
    val endNS = System.nanoTime
    val duration = durationToS(startNS, endNS)
    val elapsed = durationToS(statsKeeper.startNS, endNS)
    val avg = statsKeeper.update(duration)

    logger.debug(s"  ${duration}s: $paperId: finished writing JSON to ${outputDir.getCanonicalPath}")
    val outcome =
      if (errorCount == 0) "successfully"
      else "with errors"
    logger.info(s"$endTime: Finished $paperId $outcome (${duration} seconds)")
    logger.info(s"$endTime: PapersDone: ${avg(0)}, ElapsedTime: ${elapsed}, Average: ${avg(1)}")

    if (errorCount == 0)
      // record successful processing of input file for possible batch restart
      fileSucceeded(file)
    errorCount
  }

  /** Write output for mentions originating from a single FriesEntry. */
  def outputMentions (
    mentions: Seq[Mention],
    entry: FriesEntry,
    paperId: String,
    startTime: Date,
    outputDir: File,
    outputType: String,
    withAssembly: Boolean
  ) = {


    val outFile = s"${outputDir.getAbsolutePath}${File.separator}$paperId"
    (outputType.toLowerCase, withAssembly) match {

      case ("text", _) =>
        val mentionMgr = new MentionManager()
        val lines = mentionMgr.sortMentionsToStrings(mentions)
        val outFile = new File(outputDir, s"$paperId.txt")
        FileUtils.writeLines(outFile, lines.asJavaCollection)

      // Handle FRIES-style output (w/ assembly)
      case ("fries", true) =>
        val mentionsForOutput = prepareMentionsForMITRE(mentions)
        val assembler = doAssembly(mentionsForOutput)
        // time elapsed (w/ assembly)
        val procTime = ReachCLI.now
        val outputter = new FriesOutput()
        outputter.writeJSON(paperId, mentionsForOutput, Seq(entry), startTime, procTime, outFile)

      // Handle FRIES-style output (w/o assembly)
      case ("fries", false) =>
        val mentionsForOutput = prepareMentionsForMITRE(mentions)
        // time elapsed (w/o assembly)
        val procTime = ReachCLI.now
        val outputter = new FriesOutput()
        outputter.writeJSON(paperId, mentionsForOutput, Seq(entry), startTime, procTime, outFile)

      // Handle Index cards (NOTE: outdated!)
      case ("indexcard", _) =>
        // time elapsed
        val procTime = ReachCLI.now
        val outputter = new IndexCardOutput()
        outputter.writeJSON(paperId, mentions, Seq(entry), startTime, procTime, outFile)

      // Handle Serial-JSON output format (w/o assembly)
      case ("serial-json", _) =>
        val procTime = ReachCLI.now
        val outputter = new SerialJsonOutput(encoding)
        outputter.writeJSON(paperId, mentions, Seq(entry), startTime, procTime, outFile)

      // assembly output
      case ("assembly-tsv", _) =>
        val assembler = doAssembly(mentions)
        val ae = new AssemblyExporter(assembler.am)
        val outFile = new File(outputDir, s"$paperId-assembly-out.tsv")
        val outFile2 = new File(outputDir, s"$paperId-assembly-out-unconstrained.tsv")
        // MITRE's requirements
        ae.writeRows(outFile, AssemblyExporter.DEFAULT_COLUMNS, AssemblyExporter.SEP, ExportFilters.MITREfilter)
        // no filter
        ae.writeRows(outFile2, AssemblyExporter.DEFAULT_COLUMNS, AssemblyExporter.SEP, (rows: Seq[AssemblyRow]) => rows.filter(_.seen > 0))

      // Arizona's custom tabular output for assembly
      case ("arizona", _) =>
        val output = ArizonaOutputter.tabularOutput(mentions)
        val outFile = new File(outputDir, s"$paperId-arizona-out.tsv")
        outFile.writeString(output, java.nio.charset.StandardCharsets.UTF_8)

      // CMU's custom tabular output for assembly
      case ("cmu", _) =>
        val output = CMUExporter.tabularOutput(mentions)
        val outFile = new File(outputDir, s"$paperId-cmu-out.tsv")
        outFile.writeString(output, java.nio.charset.StandardCharsets.UTF_8)

      case ("training-data", _) =>
        val output = TrainingDataExporter.jsonOutput(mentions,
          allowedLabels = Some(Set("Positive_activation", "Negative_activation", "Activation",
            "Positive_regulation", "Negative_regulation", "Regulation")),
        )
        val outFile = new File(outputDir, s"$paperId-classifictaion-out.json")
        outFile.writeString(output, java.nio.charset.StandardCharsets.UTF_8)

      case ("rule-learning", _) =>
        val rulesDictionary = PaperReader.reachSystem.rulePatternsMap
        val output = TrainingDataExporter.jsonOutput(mentions,
          allowedLabels = Some(Set("Positive_activation", "Negative_activation", "Activation")),
          includeRule = true,
          rulesDictionary = Some(rulesDictionary))
        // Only look at activations
        val outFile = new File(outputDir, s"$paperId-rule_learning-out.json")
        outFile.writeString(output, java.nio.charset.StandardCharsets.UTF_8)

      case ("visual-analytics", _) =>
        val output = VisualAnalyticsDataExporter.jsonOutput(mentions)
        val outFile = new File(outputDir, s"$paperId-va.json")
        outFile.writeString(output, java.nio.charset.StandardCharsets.UTF_8)

      case _ => throw new RuntimeException(s"Output format ${outputType.toLowerCase} not yet supported!")
    }
  }
}


/**
  * Legacy companion object to initialize a processing class with the given parameters and
  * run it on the papers in the specified directory.
  */
object ReachCLI {
  /** Return a new timestamp each time called. */
  def now = new Date()

  /** legacy constructor for a single output format */
  def apply (
    papersDir: File,
    outputDir: File,
    outputFormat: String,
    statsKeeper: ProcessingStats = new ProcessingStats,
    encoding: Charset = UTF_8,
    restartFile: Option[File] = None
  ): ReachCLI =
    new ReachCLI(papersDir, outputDir, Seq(outputFormat), statsKeeper, encoding, restartFile)
}


/**
  * Object which reads the configuration file, initializes a processing class from it,
  * and processes a directory of papers.
  */
object RunReachCLI extends App with LazyLogging {
  // to set a custom conf file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
  val config = ConfigFactory.load()

  val papersDir: File = config[File]("papersDir")
  logger.debug(s"(ReachCLI.init): papersDir=${papersDir}")
  val outDir: File = config[File]("outDir")
  logger.debug(s"(ReachCLI.init): outDir=${outDir}")
  // seq of output types
  val outputTypes: List[String] = config[List[String]]("outputTypes")
  logger.debug(s"(ReachCLI.init): outputTypes=${outputTypes.mkString(", ")}")
  val encoding: String = config[String]("encoding")
  logger.debug(s"(ReachCLI.init): encoding=${encoding}")

  // should assembly be performed?
  val withAssembly: Boolean = config[Boolean]("withAssembly")
  logger.debug(s"(ReachCLI.init): withAssembly=${withAssembly}")

  // configure the optional restart capability
  val useRestart: Boolean = config[Boolean]("restart.useRestart")
  logger.debug(s"(ReachCLI.init): useRestart=${useRestart}")
  val restartFile: File = config[File]("restart.logfile")
  logger.debug(s"(ReachCLI.init): restartFile=${restartFile}")

  // the number of threads to use for parallelization
  val threadLimit: Int = config[Int]("threadLimit")
  logger.debug(s"(ReachCLI.init): threadLimit=${threadLimit}")

  logger.info("ReachCLI begins...")

  // if input papers directory does not exist there is nothing to do
  if (!papersDir.exists) {
    sys.error(s"${papersDir.getCanonicalPath} does not exist")
  }

  // if output directory does not exist create it
  if (!outDir.exists) {
    logger.info(s"Creating output directory: ${outDir.getCanonicalPath}")
    FileUtils.forceMkdir(outDir)
  } else if (!outDir.isDirectory) {
    sys.error(s"${outDir.getCanonicalPath} is not a directory")
  }

  // insure existance of the specified restart file
  if (useRestart)
    restartFile.createNewFile()

  // create a new processing class and process the specified batch of input papers
  val cli = new ReachCLI(
    papersDir,
    outDir,
    outputTypes,
    encoding = UTF_8,
    restartFile = if (useRestart) Some(restartFile) else None
  )

  cli.processPapers(Some(threadLimit), withAssembly)
}
