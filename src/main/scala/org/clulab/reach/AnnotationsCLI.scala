package org.clulab.reach

import util.{Failure, Success, Try}
import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.io.Source
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.{FileUtils, FilenameUtils}

import java.io.File
import java.util.Date
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import ai.lum.common.FileUtils._
import ai.lum.common.ConfigUtils._
import org.clulab.odin._
import org.clulab.reach.`export`.arizona.ArizonaOutputter
import org.clulab.reach.`export`.cmu.CMUExporter
import org.clulab.reach.assembly._
import org.clulab.reach.assembly.export.{AssemblyExporter, AssemblyRow, ExportFilters}
import org.clulab.reach.export.OutputDegrader
import org.clulab.reach.export.fries.FriesOutput
import org.clulab.reach.export.indexcards.IndexCardOutput
import org.clulab.reach.export.serial.SerialJsonOutput
import org.clulab.reach.mentions.CorefMention
import org.clulab.reach.mentions.serialization.json._
import org.clulab.reach.utils.MentionManager
import org.clulab.utils.Serializer

/**
  * Class to run Reach reading and assembly and then produce FRIES format output
  * from a group of input files.
  *   Written by: Gus Hahn-Powell and Tom Hicks. 5/9/2016.
  *   Last Modified: 
  *   - 12/28/2020: Added assembly back: we need causal precedence and the `arizona` format 
  *   - Remove assembly functionality (including output formats relying on assembly).
  */
class AnnotationsCLI(
  val papersDir: File,
  val outputDir: File,
  val statsKeeper: ProcessingStats = new ProcessingStats,
  val encoding: Charset = UTF_8,
  val restartFile: Option[File] = None
) extends LazyLogging {

  /** Return a (possibly empty) set of filenames for input file (papers) which have
      already been successfully processed and which can be skipped. */
  val skipFiles: Set[String] = restartFile match {
    case None => Set.empty[String]
    case Some(f) =>
      // get set of nonempty lines
      val lines: Set[String] = f.readString(encoding).split("\n").filter(_.nonEmpty).toSet
      lines
  }

  /** Lock file object for restart file. */
  private val restartFileLock = new AnyRef

  /** In the restart log file, record the given file as successfully completed. */
  def fileSucceeded (file: File): Unit = if (restartFile.nonEmpty) {
    restartFileLock.synchronized {
      restartFile.get.writeString(
        string = s"${file.getName}\n",
        charset = encoding,
        append = true,
        gzipSupport = false
      )
    }
  }

  def reportException(file: File, e: Throwable): Unit = {
    val filename = file.getName
    val paperID = FilenameUtils.removeExtension(filename)
    val report =
      s"""
         |==========
         |
         | ¡¡¡ NxmlReader error !!!
         |
         |paper: $paperID
         |
         |error:
         |${e.toString}
         |
         |stack trace:
         |${e.getStackTrace.mkString("\n")}
         |
         |==========
         |""".stripMargin
    logger.error(report)
  }

  /** Process papers **/
  def preAnnotatePapers(threadLimit: Option[Int]): Int = {
    logger.info("Initializing Reach ...")

    val files = papersDir.listFilesByRegex(pattern=ReachInputFilePattern, caseInsensitive = true, recursive = true).toVector.par

    // limit parallelization
    if (threadLimit.nonEmpty) {
      files.tasksupport =
        new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit.get))
    }

    val errorCounts = for {
      file <- files
      filename = file.getName
      if ! skipFiles.contains(filename)
    } yield {
      val error: Int = try {
        // Count the number of failed files, not failed formats.
        math.signum(preAnnotatePaper(file))
      } catch {
        case e: Exception =>
          // The reading itself, rather than the format, could have failed.
          reportException(file, e)
          1
      }
      error
    }
    val paperCount = errorCounts.length
    val errorCount = errorCounts.sum
    val message =
      if (errorCount > 0)
        s"Reach encountered $errorCount error(s) with the $paperCount paper(s).  Please check the log."
      else
        s"Reach encountered no errors with the $paperCount paper(s)."
    logger.info(message)
    errorCount
  }

  def prepareMentionsForMITRE (mentions: Seq[Mention]): Seq[CorefMention] = {
    // NOTE: We're already doing this in the exporter, but the mentions given to the
    // Assembler probably need to match since flattening results in a loss of information
    OutputDegrader.prepareForOutput(mentions)
  }

  def doAssembly (mns: Seq[Mention]): Assembler = Assembler(mns)

  // Returns count of outputFormats which errored.
  def preAnnotatePaper(file: File): Int = {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startNS = System.nanoTime
    val startTime = AnnotationsCLI.now

    logger.info(s"$startTime: Starting $paperId")
    logger.debug(s"  ${ durationToS(startNS, System.nanoTime) }s: $paperId: started reading")

    // entry must be kept around for outputter
    val entry = PaperReader.getEntryFromPaper(file)
    val doc = PaperReader.reachSystem.mkDoc(entry.text, entry.name, entry.chunkId)

    logger.debug(s"  ${ durationToS(startNS, System.nanoTime) }s: $paperId: finished annotating")

    // generate outputs
    // NOTE: Assembly can't be run before calling this method without additional refactoring,
    // as different output formats apply different filters before running assembly
    val serializationOutcome =
      Try(Serializer.save(doc, new File(outputDir, s"$paperId.ser")))


    // elapsed time: processing + writing output
    val endTime = AnnotationsCLI.now
    val endNS = System.nanoTime
    val duration = durationToS(startNS, endNS)
    val elapsed = durationToS(statsKeeper.startNS, endNS)
    val avg = statsKeeper.update(duration)

    logger.debug(s"  ${duration}s: $paperId: finished writing JSON to ${outputDir.getCanonicalPath}")
    val (outcome, errorCount) =
      serializationOutcome match {
        case Success(_) =>
         ( "successfully", 0)
        case Failure(ex) =>
          reportException(file, ex)
          ("with errors", 1)
      }

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
        val procTime = AnnotationsCLI.now
        val outputter = new FriesOutput()
        outputter.writeJSON(paperId, mentionsForOutput, Seq(entry), startTime, procTime, outFile)

      // Handle FRIES-style output (w/o assembly)
      case ("fries", false) =>
        val mentionsForOutput = prepareMentionsForMITRE(mentions)
        // time elapsed (w/o assembly)
        val procTime = AnnotationsCLI.now
        val outputter = new FriesOutput()
        outputter.writeJSON(paperId, mentionsForOutput, Seq(entry), startTime, procTime, outFile)

      // Handle Index cards (NOTE: outdated!)
      case ("indexcard", _) =>
        // time elapsed
        val procTime = AnnotationsCLI.now
        val outputter = new IndexCardOutput()
        outputter.writeJSON(paperId, mentions, Seq(entry), startTime, procTime, outFile)

      // Handle Serial-JSON output format (w/o assembly)
      case ("serial-json", _) =>
        val procTime = AnnotationsCLI.now
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

      case _ => throw new RuntimeException(s"Output format ${outputType.toLowerCase} not yet supported!")
    }
  }

  /** Return the duration, in seconds, between the given nanosecond time values. */
  private def durationToS (startNS:Long, endNS:Long): Long = (endNS - startNS) / 1000000000L
}


/**
  * Legacy companion object to initialize a processing class with the given parameters and
  * run it on the papers in the specified directory.
  */
object AnnotationsCLI {
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
  ): AnnotationsCLI =
    new AnnotationsCLI(papersDir, outputDir, statsKeeper, encoding, restartFile)
}


/**
  * Object which reads the configuration file, initializes a processing class from it,
  * and processes a directory of papers.
  */
object RunAnnotationsCLI extends App with LazyLogging {
  // to set a custom conf file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
  val config = ConfigFactory.load()

  val papersDir: File = config[File]("papersDir")
  logger.debug(s"(RunAnnotationsCLI.init): papersDir=${papersDir}")
  val outDir: File = config[File]("outDir")
  logger.debug(s"(RunAnnotationsCLI.init): outDir=${outDir}")

  val encoding: String = config[String]("encoding")
  logger.debug(s"(RunAnnotationsCLI.init): encoding=${encoding}")

  // configure the optional restart capability
  val useRestart: Boolean = config[Boolean]("restart.useRestart")
  logger.debug(s"(RunAnnotationsCLI.init): useRestart=${useRestart}")
  val restartFile: File = config[File]("restart.logfile")
  logger.debug(s"(RunAnnotationsCLI.init): restartFile=${restartFile}")

  // the number of threads to use for parallelization
  val threadLimit: Int = config[Int]("threadLimit")
  logger.debug(s"(RunAnnotationsCLI.init): threadLimit=${threadLimit}")

  logger.info("RunAnnotationsCLI begins...")

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
  val cli = new AnnotationsCLI(
    papersDir,
    outDir,
    encoding = UTF_8,
    restartFile = if (useRestart) Some(restartFile) else None
  )

  cli.preAnnotatePapers(Some(threadLimit))
}
