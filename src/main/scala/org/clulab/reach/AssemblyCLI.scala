package org.clulab.reach

import java.io.File
import java.util.Date
import scala.collection.parallel.ForkJoinTaskSupport
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import org.clulab.assembly._
import org.clulab.odin._
import org.clulab.reach.extern.export.fries._
import org.clulab.reach.nxml._


/**
  * Class to run Reach reading and assembly then produce FRIES format output
  * from a group of input files.
  *   Written by: Gus Hahn-Powell and Tom Hicks. 5/9/2016.
  *   Last Modified: Add timing and logging.
  */
class AssemblyCLI(
  val papersDir: File,
  val outputDir: File,
  val logFile: File,
  val verbose: Boolean = false
) {

  /** Process papers **/
  def processPapers(threadLimit: Option[Int]): Unit = {
    if (verbose)
      println("Initializing Reach ...")

    val _ = PaperReader.rs.extractFrom("Blah", "", "")
    val files = papersDir.listFiles.par

    // limit parallelization
    if (threadLimit.nonEmpty) {
      files.tasksupport =
        new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit.get))
    }

    for (file <- files) {
      val paperId = FilenameUtils.removeExtension(file.getName)
      val startNS = System.nanoTime
      val startTime = AssemblyCLI.now

      FileUtils.writeStringToFile(logFile, s"${startTime}: Starting $paperId\n", true)
      if (verbose)
        println(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: starting reading")

      // entry must be kept around for outputter
      val entry = PaperReader.getEntryFromPaper(file)
      val mentions = PaperReader.getMentionsFromEntry(entry)

      // NOTE: We're already doing this in the exporter, but the mentions given to the Assembler probably
      // need to match since flattening results in a loss of information
      val mentionsForOutput = OutputDegrader.prepareForOutput(mentions)
      if (verbose)
        println(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: finished reading")

      val assemblyAPI = new Assembler(mentionsForOutput) // do assembly

      if (verbose)
        println(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: finished initializing Assembler")

      val procTime = AssemblyCLI.now

      // generate output
      outputMentions(mentionsForOutput, entry, paperId, startTime, procTime, outputDir, assemblyAPI)

      val endTime = AssemblyCLI.now
      val endNS = System.nanoTime
      if (verbose)
        println(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: finished writing JSON to ${outputDir.getCanonicalPath()}")
      FileUtils.writeStringToFile(
        logFile, s"${endTime}: Finished $paperId successfully (${nsToS(startNS, endNS)} seconds)\n", true)
    }
  }

  /**
    * Write output for mentions originating from a single FriesEntry
    */
  def outputMentions(
    mentions: Seq[Mention],
    entry: FriesEntry,
    paperId: String,
    startTime: Date,
    endTime: Date,
    outputDir: File,
    assemblyAPI: Assembler
  ) = {
    val outFile = s"${outputDir.getAbsolutePath}${File.separator}$paperId"
    val outputter:FriesOutput = new FriesOutput()
    outputter.writeJSON(paperId, mentions, Seq(entry), startTime, endTime, outFile, assemblyAPI)
  }

  private def nsToS (startNS:Long, endNS:Long): Long = (endNS - startNS) / 1000000000L
}

object AssemblyCLI extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val verbose = config.getBoolean("assembly.verbose")

  val papersDir = new File(config.getString("assembly.papers"))
  val friesDir = new File(config.getString("friesDir"))
  val logFile = new File(config.getString("logFile"))

  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")

  // lets start a new log file
  if (logFile.exists) {
    FileUtils.forceDelete(logFile)
  }
  FileUtils.writeStringToFile(logFile, s"$now: AssemblyCLI begins ...\n")

  // if papersDir does not exist there is nothing to do
  if (!papersDir.exists) {
    sys.error(s"${papersDir.getCanonicalPath} does not exist")
  }

  // if friesDir does not exist create it
  if (!friesDir.exists) {
    if (verbose)
      println(s"Creating output directory: ${friesDir.getCanonicalPath}")
    FileUtils.forceMkdir(friesDir)
  } else if (!friesDir.isDirectory) {
    sys.error(s"${friesDir.getCanonicalPath} is not a directory")
  }

  val cli = new AssemblyCLI(papersDir, friesDir, logFile, verbose)

  cli.processPapers(Some(threadLimit))

  def now = new Date()
}
