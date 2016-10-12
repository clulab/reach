package org.clulab.reach

import java.io.File
import java.util.Date
import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.{FileUtils, FilenameUtils}
import org.clulab.reach.assembly._
import org.clulab.reach.assembly.export.{AssemblyExporter, ExportFilters, Row}
import org.clulab.odin._
import org.clulab.reach.assembly.sieves.{AssemblySieve, DeduplicationSieves}
import org.clulab.reach.export.OutputDegrader
import org.clulab.reach.utils.MentionManager
import org.clulab.reach.export.fries._
import org.clulab.reach.export.indexcards.IndexCardOutput
import org.clulab.reach.mentions.CorefMention


/**
  * Class to run Reach reading and assembly then produce FRIES format output
  * from a group of input files.
  *   Written by: Gus Hahn-Powell and Tom Hicks. 5/9/2016.
  *   Last Modified: Add timing and logging.
  */
class ReachCLI(
  val papersDir: File,
  val outputDir: File,
  val outputFormat: String,
  val logFile: File
//  val verbose: Boolean = false
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
      paperID = FilenameUtils.removeExtension(file.getName)
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
          FileUtils.writeStringToFile(logFile, report, true)
          1
      }
      error
    }
    errorCount.sum
  }

  def prepareMentionsForMITRE(mentions: Seq[Mention]): Seq[CorefMention] = {
    // NOTE: We're already doing this in the exporter, but the mentions given to the Assembler probably
    // need to match since flattening results in a loss of information
    OutputDegrader.prepareForOutput(mentions)
  }


  def doAssembly(mns: Seq[Mention]): Assembler = Assembler(mns)

  def processPaper(file: File, withAssembly: Boolean): Unit = {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startNS = System.nanoTime
    val startTime = ReachCLI.now

    FileUtils.writeStringToFile(logFile, s"$startTime: Starting $paperId\n", true)

    logger.debug(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: starting reading")

    // entry must be kept around for outputter
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)


    logger.debug(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: finished reading")

    // generate output
    outputMentions(mentions, entry, paperId, startTime, outputDir, outputFormat, withAssembly)

    // time elapsed (processing + writing output)
    val endTime = ReachCLI.now
    val endNS = System.nanoTime

    logger.debug(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: finished writing JSON to ${outputDir.getCanonicalPath}")

    FileUtils.writeStringToFile(
      logFile, s"$endTime: Finished $paperId successfully (${nsToS(startNS, endNS)} seconds)\n", true
    )
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
        logger.info(s"writing ${outFile.getName} ...")
        FileUtils.writeLines(outFile, lines.asJavaCollection)

      // Handle FRIES-style output (w/ assembly)
      case ("fries", true) =>
        val mentionsForOutput = prepareMentionsForMITRE(mentions)
        val assembler = doAssembly(mentionsForOutput)
        // time elapsed (w/ assembly)
        val procTime = ReachCLI.now
        //if (verbose) {println (s"  ${nsToS (startNS, System.nanoTime)}s: $paperId: finished initializing Assembler")}
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
        ae.writeRows(outFile2, AssemblyExporter.DEFAULT_COLUMNS, AssemblyExporter.SEP, (rows: Set[Row]) => rows.filter(_.seen > 0))

      // Arizona's custom output for assembly
      case ("arizona", _) =>
        // perform deduplication
        val dedup = new DeduplicationSieves()
        val orderedSieves =
        // track relevant mentions
          AssemblySieve(dedup.trackMentions)
        val am: AssemblyManager = orderedSieves.apply(mentions)
        val ae = new AssemblyExporter(am)
        val outFile = new File(outputDir, s"$paperId-arizona-out.tsv")
        // no filter
        // FIXME: specify columns and sep
        val cols = Seq(
          AssemblyExporter.INPUT,
          AssemblyExporter.OUTPUT,
          AssemblyExporter.CONTROLLER,
          AssemblyExporter.EVENT_ID,
          AssemblyExporter.EVENT_LABEL,
          AssemblyExporter.NEGATED,
          AssemblyExporter.INDIRECT,
          // context
          AssemblyExporter.CONTEXT_SPECIES,
          AssemblyExporter.CONTEXT_ORGAN,
          AssemblyExporter.CONTEXT_CELL_LINE,
          AssemblyExporter.CONTEXT_CELL_TYPE,
          AssemblyExporter.CONTEXT_CELLULAR_COMPONENT,
          AssemblyExporter.CONTEXT_TISSUE_TYPE,
          // evidence
          AssemblyExporter.SEEN,
          AssemblyExporter.EVIDENCE,
          AssemblyExporter.SEEN_IN
        )
        def arizonaFilter(rows: Set[Row]): Set[Row] = rows.filter { r =>
          // remove unseen
          (r.seen > 0) &&
          // keep only the events
          ExportFilters.isEvent(r)
        }
        ae.writeRows(outFile, cols, AssemblyExporter.SEP, arizonaFilter)

      case _ => throw new RuntimeException(s"Output format ${outputType.toLowerCase} not yet supported!")
    }
  }

  private def nsToS (startNS:Long, endNS:Long): Long = (endNS - startNS) / 1000000000L
}

object ReachCLI extends App with LazyLogging {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val papersDir = new File(config.getString("papersDir"))
  val outDir = new File(config.getString("outDir"))
  // should assembly be performed?
  val withAssembly = config.getBoolean("withAssembly")
  val outputType = config.getString("outputType")
  val logFile = new File(config.getString("logging.logfile"))

  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")

  // lets start a new log file
  if (logFile.exists) {
    FileUtils.forceDelete(logFile)
  }
  FileUtils.writeStringToFile(logFile, s"$now: ReachCLI (${if (withAssembly) "w/" else "w/o"} assembly) begins ...\n")

  // if papersDir does not exist there is nothing to do
  if (!papersDir.exists) {
    sys.error(s"${papersDir.getCanonicalPath} does not exist")
  }

  // if friesDir does not exist create it
  if (!outDir.exists) {
    logger.info(s"Creating output directory: ${outDir.getCanonicalPath}")
    FileUtils.forceMkdir(outDir)
  } else if (!outDir.isDirectory) {
    sys.error(s"${outDir.getCanonicalPath} is not a directory")
  }

  val cli = new ReachCLI(papersDir, outDir, outputType, logFile)

  cli.processPapers(Some(threadLimit), withAssembly)

  def now = new Date()
}