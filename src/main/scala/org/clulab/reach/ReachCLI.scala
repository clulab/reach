package org.clulab.reach

import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.io.Source
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

import org.apache.commons.io.{ FileUtils, FilenameUtils }
import java.io.File
import java.util.Date

import ai.lum.common.FileUtils._
import ai.lum.common.ConfigUtils._

import org.clulab.odin._
import org.clulab.reach.export.OutputDegrader
import org.clulab.reach.export.fries.FriesOutput
import org.clulab.reach.export.indexcards.IndexCardOutput
import org.clulab.reach.export.serial.SerialJsonOutput
import org.clulab.reach.mentions.CorefMention
import org.clulab.reach.mentions.serialization.json._
import org.clulab.reach.utils.MentionManager

/**
  * Class to run Reach reading and then produce FRIES format output
  * from a group of input files.
  *   Written by: Gus Hahn-Powell and Tom Hicks. 5/9/2016.
  *   Last Modified: Remove assembly functionality (including output formats relying on assembly).
  */
class ReachCLI (
  val papersDir: File,
  val outputDir: File,
  val outputFormats: Seq[String],
  val statsKeeper: ProcessingStats = new ProcessingStats,
  val encoding: String = "utf-8",
  val restartFile: Option[File] = None
) extends LazyLogging {

  /** Return a (possibly empty) set of filenames for input file (papers) which have
      already been successfully processed and which can be skipped. */
  val skipFiles: Set[String] = restartFile match {
    case None => Set.empty[String]
    case Some(f) =>
      val src = Source.fromFile(f, encoding)
      // get set of nonempty lines
      val lines: Set[String] = src.getLines().filter(_.nonEmpty).toSet
      // close the file
      src.close()
      lines
  }

  /** Lock file object for restart file. */
  private val restartFileLock = new AnyRef

  /** In the restart log file, record the given file as successfully completed. */
  def fileSucceeded (file: File): Unit = if (restartFile.nonEmpty) {
    restartFileLock.synchronized {
      restartFile.get.writeString(s"${file.getName}\n", charset = encoding, append = true)
    }
  }

  /** Process papers **/
  def processPapers (threadLimit: Option[Int]): Int = {
    logger.info("Initializing Reach ...")

    val files = papersDir.listFilesByRegex(pattern=ReachInputFilePattern, caseSensitive=false, recursive=true).toVector.par

    // limit parallelization
    if (threadLimit.nonEmpty) {
      files.tasksupport =
        new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit.get))
    }

    val errorCount = for {
      file <- files
      filename = file.getName
      paperID = FilenameUtils.removeExtension(filename)
      if ! skipFiles.contains(filename)
    } yield {
      val error: Int = try {
        processPaper(file)
        0                                   // no error
      } catch {
        case e: Exception =>
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
          1
      }
      error
    }
    errorCount.sum
  }

  def prepareMentionsForMITRE (mentions: Seq[Mention]): Seq[CorefMention] = {
    OutputDegrader.prepareForOutput(mentions)
  }

  def processPaper (file: File): Unit = {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startNS = System.nanoTime
    val startTime = ReachCLI.now

    logger.info(s"$startTime: Starting $paperId")
    logger.debug(s"  ${ durationToS(startNS, System.nanoTime) }s: $paperId: started reading")

    // entry must be kept around for outputter
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)

    logger.debug(s"  ${ durationToS(startNS, System.nanoTime) }s: $paperId: finished reading")

    // generate outputs
    outputFormats.foreach(outputFormat => outputMentions(mentions, entry, paperId, startTime, outputDir, outputFormat))

    // elapsed time: processing + writing output
    val endTime = ReachCLI.now
    val endNS = System.nanoTime
    val duration = durationToS(startNS, endNS)
    val elapsed = durationToS(statsKeeper.startNS, endNS)
    val avg = statsKeeper.update(duration)

    logger.debug(s"  ${duration}s: $paperId: finished writing JSON to ${outputDir.getCanonicalPath}")
    logger.info(s"$endTime: Finished $paperId successfully (${duration} seconds)")
    logger.info(s"$endTime: PapersDone: ${avg(0)}, ElapsedTime: ${elapsed}, Average: ${avg(1)}")

    // record successful processing of input file for possible batch restart
    fileSucceeded(file)
  }

  /** Write output for mentions originating from a single FriesEntry. */
  def outputMentions (
    mentions: Seq[Mention],
    entry: FriesEntry,
    paperId: String,
    startTime: Date,
    outputDir: File,
    outputType: String
  ) = {


    val outFile = s"${outputDir.getAbsolutePath}${File.separator}$paperId"
    outputType.toLowerCase match {

      case "text" =>
        val mentionMgr = new MentionManager()
        val lines = mentionMgr.sortMentionsToStrings(mentions)
        val outFile = new File(outputDir, s"$paperId.txt")
        FileUtils.writeLines(outFile, lines.asJavaCollection)

      // Handle FRIES-style output
      case "fries" =>
        val mentionsForOutput = prepareMentionsForMITRE(mentions)
        // time elapsed
        val procTime = ReachCLI.now
        val outputter = new FriesOutput()
        outputter.writeJSON(paperId, mentionsForOutput, Seq(entry), startTime, procTime, outFile)

      // Handle Index cards (NOTE: outdated!)
      case "indexcard" =>
        // time elapsed
        val procTime = ReachCLI.now
        val outputter = new IndexCardOutput()
        outputter.writeJSON(paperId, mentions, Seq(entry), startTime, procTime, outFile)

      // Handle Serial-JSON output format
      case "serial-json" =>
        val procTime = ReachCLI.now
        val outputter = new SerialJsonOutput(encoding)
        outputter.writeJSON(paperId, mentions, Seq(entry), startTime, procTime, outFile)

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
object ReachCLI {
  /** Return a new timestamp each time called. */
  def now = new Date()

  /** legacy constructor for a single output format */
  def apply (
    papersDir: File,
    outputDir: File,
    outputFormat: String,
    statsKeeper: ProcessingStats = new ProcessingStats,
    encoding: String = "utf-8",
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
    encoding = encoding,
    restartFile = if (useRestart) Some(restartFile) else None
  )

  cli.processPapers(Some(threadLimit))
}
