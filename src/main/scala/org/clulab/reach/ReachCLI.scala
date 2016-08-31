package org.clulab.reach

import java.io.File
import java.util.Date
import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.{FileUtils, FilenameUtils}
import org.clulab.assembly._
import org.clulab.assembly.export.{AssemblyExporter, Row}
import org.clulab.odin._
import org.clulab.reach.darpa.OutputDegrader
import org.clulab.reach.extern.export.MentionManager
import org.clulab.reach.extern.export.fries._
import org.clulab.reach.extern.export.indexcards.IndexCardOutput
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
  val logFile: File,
  val verbose: Boolean = false
) extends LazyLogging {

  /** Process papers **/
  def processPapers(threadLimit: Option[Int], withAssembly: Boolean): Int = {
    if (verbose)
      println("Initializing Reach ...")

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

    if (verbose) {
      println(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: starting reading")
    }

    // entry must be kept around for outputter
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)


    if (verbose) {
      println(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: finished reading")
    }

    // generate output
    outputMentions(mentions, entry, paperId, startTime, outputDir, outputFormat, withAssembly)

    // time elapsed (processing + writing output)
    val endTime = ReachCLI.now
    val endNS = System.nanoTime

    if (verbose) {
      println(s"  ${nsToS(startNS, System.nanoTime)}s: $paperId: finished writing JSON to ${outputDir.getCanonicalPath}")
    }

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
        println(s"writing ${outFile.getName} ...")
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

      // Arizona's custom output for assembly
      case ("assembly-tsv", _) =>
        val assembler = doAssembly(mentions)
        val ae = new AssemblyExporter(assembler.am)
        val outFile = new File(outputDir, s"$paperId-assembly-out.tsv")
        val outFile2 = new File(outputDir, s"$paperId-assembly-out-unconstrained.tsv")
        // MITRE's requirements
        ae.writeTSV(outFile, AssemblyExporter.MITREfilter)
        // no filter
        ae.writeTSV(outFile2, (rows: Set[Row]) => rows.filter(_.seen > 0))

      case _ => throw new RuntimeException(s"Output format ${outputType.toLowerCase} not yet supported!")
    }
  }

  private def nsToS (startNS:Long, endNS:Long): Long = (endNS - startNS) / 1000000000L
}

object ReachCLI extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val verbose = config.getBoolean("verbose")
  val papersDir = new File(config.getString("papersDir"))
  val outDir = new File(config.getString("outDir"))
  // should assembly be performed?
  val withAssembly = config.getBoolean("withAssembly")
  val outputType = config.getString("outputType")
  val logFile = new File(config.getString("logFile"))

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
    if (verbose)
      println(s"Creating output directory: ${outDir.getCanonicalPath}")
    FileUtils.forceMkdir(outDir)
  } else if (!outDir.isDirectory) {
    sys.error(s"${outDir.getCanonicalPath} is not a directory")
  }

  val cli = new ReachCLI(papersDir, outDir, outputType, logFile, verbose)

  cli.processPapers(Some(threadLimit), withAssembly)

  def now = new Date()
}
