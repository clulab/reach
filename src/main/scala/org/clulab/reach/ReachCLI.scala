package org.clulab.reach

import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.io.Source
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import java.io.{ File, FileOutputStream, OutputStreamWriter, PrintWriter }
import java.util.Date

import ai.lum.common.FileUtils._
import org.clulab.odin._
import org.clulab.reach.assembly._
import org.clulab.reach.assembly.export.{ AssemblyExporter, AssemblyRow, ExportFilters }
import org.clulab.reach.export.OutputDegrader
import org.clulab.reach.export.arizona.ArizonaOutputter
import org.clulab.reach.export.cmu.CMUExporter
import org.clulab.reach.export.fries._
import org.clulab.reach.export.indexcards.IndexCardOutput
import org.clulab.reach.mentions.CorefMention
import org.clulab.reach.utils.MentionManager

/**
  * Class to run Reach reading and assembly then produce FRIES format output
  * from a group of input files.
  *   Written by: Gus Hahn-Powell and Tom Hicks. 5/9/2016.
  *   Last Modified: Add more processing statistics to logging via new class.
  */
class ReachCLI (
  val papersDir: File,
  val outputDir: File,
  val outputFormat: String,
  val skipFiles: Set[String] = Set.empty[String],
  val statsKeeper: ProcessingStats = new ProcessingStats
) extends LazyLogging {

  /** Process papers **/
  def processPapers(threadLimit: Option[Int], withAssembly: Boolean): Int = {
    logger.info("Initializing Reach ...")

    val _ = PaperReader.rs.extractFrom("Blah", "", "")
    val files = papersDir.listFiles.par

    // limit parallelization
    if (threadLimit.nonEmpty) {
      files.tasksupport =
        new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit.get))
    }

    val errorCount = for {
      file <- files
      filename = file.getName
      paperID = FilenameUtils.removeExtension(filename)
      if (!skipFiles.contains(filename))
    } yield {
      val error: Int = try {
        processPaper(file, withAssembly)
        // no error
        0
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

  def prepareMentionsForMITRE(mentions: Seq[Mention]): Seq[CorefMention] = {
    // NOTE: We're already doing this in the exporter, but the mentions given to the
    // Assembler probably need to match since flattening results in a loss of information
    OutputDegrader.prepareForOutput(mentions)
  }

  def doAssembly(mns: Seq[Mention]): Assembler = Assembler(mns)

  def processPaper(file: File, withAssembly: Boolean): Unit = {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startNS = System.nanoTime
    val startTime = ReachCLI.now

    logger.info(s"$startTime: Starting $paperId")
    logger.debug(s"  ${ durationToS(startNS, System.nanoTime) }s: $paperId: started reading")

    // entry must be kept around for outputter
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)

    logger.debug(s"  ${ durationToS(startNS, System.nanoTime) }s: $paperId: finished reading")

    // generate output
    outputMentions(mentions, entry, paperId, startTime, outputDir, outputFormat, withAssembly)

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
    ReachCLI.fileSucceeded(file)
  }

  /**
    * Write output for mentions originating from a single FriesEntry
    */
  def outputMentions(
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

      // never perform assembly for "text" output
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
        outputter.writeJSON(paperId, mentionsForOutput, Seq(entry), startTime, procTime, outFile, assembler)

      // Handle FRIES-style output (w/o assembly)
      case ("fries", false) =>
        val mentionsForOutput = prepareMentionsForMITRE(mentions)
        // time elapsed (w/o assembly)
        val procTime = ReachCLI.now
        val outputter = new FriesOutput()
        outputter.writeJSON(paperId, mentionsForOutput, Seq(entry), startTime, procTime, outFile)

      // Handle Index cards (NOTE: outdated!)
      case ("indexcard", _)=>
        // time elapsed (w/o assembly)
        val procTime = ReachCLI.now
        val outputter =new IndexCardOutput()
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


object ReachCLI extends App with LazyLogging {
  private val restartFileLock = new AnyRef  // lock file object for restart file

  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val papersDir = new File(config.getString("papersDir"))
  logger.debug(s"(ReachCLI.init): papersDir=${papersDir}")
  val outDir = new File(config.getString("outDir"))
  logger.debug(s"(ReachCLI.init): outDir=${outDir}")
  val outputType = config.getString("outputType")
  logger.debug(s"(ReachCLI.init): outputType=${outputType}")
  val encoding = config.getString("encoding")
  logger.debug(s"(ReachCLI.init): encoding=${encoding}")

  // should assembly be performed?
  val withAssembly = config.getBoolean("withAssembly")
  logger.debug(s"(ReachCLI.init): withAssembly=${withAssembly}")

  // configure the optional restart capability
  val useRestart = config.getBoolean("restart.useRestart")
  logger.debug(s"(ReachCLI.init): useRestart=${useRestart}")
  val restartFile = new File(config.getString("restart.logfile"))
  logger.debug(s"(ReachCLI.init): restartFile=${restartFile}")
  val skipFiles = if (useRestart && restartFile.exists)
    Source.fromFile(restartFile, encoding).getLines().toSet
  else
    Set.empty[String]

  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")
  logger.debug(s"(ReachCLI.init): threadLimit=${threadLimit}")

  logger.info(s"ReachCLI (${if (withAssembly) "w/" else "w/o"} assembly) begins ...")

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

  // create a new batch class and process the input papers
  val cli = new ReachCLI(papersDir, outDir, outputType, skipFiles)
  cli.processPapers(Some(threadLimit), withAssembly)


  /** Return a new timestamp each time called. */
  def now = new Date()

  /** In the restart log file, record the given file as successfully completed. */
  def fileSucceeded (file: File): Unit = {
    if (useRestart) {
      restartFileLock.synchronized {        // sync multiple class instances
        val pw = new PrintWriter(
          new OutputStreamWriter(
            new FileOutputStream(restartFile, true), // true=append
              encoding), true)              // charsetName, true=autoFlush
        try pw.println(file.getName) finally pw.close()
      }
    }
  }

}
