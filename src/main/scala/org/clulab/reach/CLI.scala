package org.clulab.reach

import ai.lum.common.FileUtils.LumAICommonFileWrapper
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FilenameUtils
import org.clulab.utils.ThreadUtils

import java.io.File
import java.nio.charset.Charset

abstract class CLI (
                 val papersDir: File,
                 val outputDir: File,
                 val statsKeeper: ProcessingStats,
                 val encoding: Charset,
                 val restartFile: Option[File]
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

  def processPaper(file: File, withAssembly: Boolean): Int

  /** Process papers **/
  def processPapers (threadLimit: Option[Int], withAssembly: Boolean): Int = {
    logger.info("Initializing Reach ...")

    val serFiles = papersDir.listFilesByRegex(pattern=ReachInputFilePattern, caseInsensitive = true, recursive = true).toVector
    // limit parallelization
    val parFiles = threadLimit.map(ThreadUtils.parallelize(serFiles, _)).getOrElse(serFiles.par)

    val errorCounts = for {
      file <- parFiles
      filename = file.getName
      if ! skipFiles.contains(filename)
    } yield {
      val error: Int = try {
        // Count the number of failed files, not failed formats.
        math.signum(processPaper(file, withAssembly))
      } catch {
        case e: Throwable =>
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


  /** Return the duration, in seconds, between the given nanosecond time values. */
  protected def durationToS (startNS:Long, endNS:Long): Long = (endNS - startNS) / 1000000000L
}
