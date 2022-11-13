package org.clulab.reach

import util.{Failure, Success, Try}
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.{FileUtils, FilenameUtils}

import java.io.{File, PrintWriter}
import java.util.Date
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import ai.lum.common.ConfigUtils._
import org.clulab.processors.Document
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.Serializer

import java.nio.file.Path

/**
  * Class to run Reach reading and assembly and then produce FRIES format output
  * from a group of input files.
  *   Written by: Gus Hahn-Powell and Tom Hicks. 5/9/2016.
  *   Last Modified: 
  *   - 12/28/2020: Added assembly back: we need causal precedence and the `arizona` format 
  *   - Remove assembly functionality (including output formats relying on assembly).
  */
class AnnotationsCLI(
  override val papersDir: File,
  override val outputDir: File,
  override val statsKeeper: ProcessingStats = new ProcessingStats,
  override val encoding: Charset = UTF_8,
  override val restartFile: Option[File] = None
) extends CLI(papersDir, outputDir, statsKeeper, encoding, restartFile) {



  /** Lock file object for restart file. */
  private val restartFileLock = new AnyRef


  // Returns count of outputFormats which errored.
  override def processPaper(file: File, withAssembly:Boolean = false): Int = {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startNS = System.nanoTime
    val startTime = AnnotationsCLI.now
    val documentSerializer = new DocumentSerializer

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
      Try {
        serializeDoc(paperId, documentSerializer, entry, doc)
      }


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

  /**
    * Serialized the annotated document and entry objects using DocumentSerializator
    *
    * @param paperId
    * @param documentSerializer
    * @param entry
    * @param doc
    */
  private def serializeDoc(paperId: String, documentSerializer: DocumentSerializer, entry: FriesEntry, doc: Document): Unit = {
    val pw = new PrintWriter(new File(outputDir, s"$paperId.ser"))
    pw.println(entry.toString())
    pw.println(AnnotationsCLI.serializationSeparator)
    documentSerializer.save(doc, pw)
    pw.close()
  }

}


/**
  * Legacy companion object to initialize a processing class with the given parameters and
  * run it on the papers in the specified directory.
  */
object AnnotationsCLI {
  /** Return a new timestamp each time called. */
  def now = new Date()

  val serializationSeparator: String = "#FS#"

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

  cli.processPapers(Some(threadLimit), withAssembly = false)
}
