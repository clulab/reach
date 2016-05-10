package edu.arizona.sista.reach

import java.io.File
import java.util.Date
import scala.collection.parallel.ForkJoinTaskSupport
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.assembly._
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.extern.export.fries._
import edu.arizona.sista.reach.nxml._


/**
 * Class to run Reach reading and assembly then produce FRIES format output
 * from a group of input files.
 */
class SimpleAssemblyCLI(
  val papersDir: File,
  val outputDir: File,
  val logFile: File
) {

  /** Process papers **/
  def processPapers(threadLimit: Option[Int]): Unit = {
    println("initializing reach ...")
    val _ = PaperReader.rs.extractFrom("Blah", "", "")

    val files = papersDir.listFiles.par
    // limit parallelization
    if (threadLimit.nonEmpty) {
      files.tasksupport =
        new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit.get))
    }
    for {
      f <- files.seq
      paperID = FilenameUtils.removeExtension(f.getName)
      startTime = SimpleAssemblyCLI.now
      entries = PaperReader.getEntriesFromPaper(f)
      mentions = PaperReader.getMentionsFromFriesEntries(entries)
      _ = println(s"Finished reading $paperID")
      // do assembly
      assemblyAPI = new Assembler(mentions)
      _ = println(s"finished initializing Assembler for $paperID")
      endTime = SimpleAssemblyCLI.now
    } {
      println(s"Starting writeJSON for $paperID")
      outputMentions(mentions, entries, paperID, startTime, endTime, outputDir, assemblyAPI)
    }
  }

  // process papers in parallel

  def outputMentions(
    mentions: Seq[Mention],
    paperPassages: Seq[FriesEntry],
    paperId: String,
    startTime: Date,
    endTime: Date,
    outputDir: File,
    assemblyAPI: Assembler
  ) = {
    val outFile = s"${outputDir.getAbsolutePath}${File.separator}$paperId"
    val outputter:FriesOutput = new FriesOutput()
    outputter.writeJSON(paperId, mentions, paperPassages, startTime, endTime, outFile, assemblyAPI)
  }

}

object SimpleAssemblyCLI extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val papersDir = new File(config.getString("assembly.papers"))
  val friesDir = new File(config.getString("friesDir"))
  val logFile = new File(config.getString("logFile"))

  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")

  // lets start a new log file
  if (logFile.exists) {
    FileUtils.forceDelete(logFile)
  }
  FileUtils.writeStringToFile(logFile, s"$now\nstarting extraction ...\n")

  // if nxmlDir does not exist there is nothing to do
  if (!papersDir.exists) {
    sys.error(s"${papersDir.getCanonicalPath} does not exist")
  }

  // if friesDir does not exist create it
  if (!friesDir.exists) {
    println(s"creating ${friesDir.getCanonicalPath}")
    FileUtils.forceMkdir(friesDir)
  } else if (!friesDir.isDirectory) {
    sys.error(s"${friesDir.getCanonicalPath} is not a directory")
  }

  val cli = new SimpleAssemblyCLI(papersDir, friesDir, logFile)

  cli.processPapers(Some(threadLimit))

  def now = new Date()
}
