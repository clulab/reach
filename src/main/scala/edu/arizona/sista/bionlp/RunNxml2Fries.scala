package edu.arizona.sista.bionlp

import java.io.File
import java.util.Date
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.{ Try,Success,Failure }
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.extern.export.JsonOutputter

import edu.arizona.sista.bionlp.nxml._

object RunNxml2Fries extends App{

  def now = new Date()

  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val nxmlDir = new File(config.getString("nxmlDir"))
  val txtDir = new File(config.getString("txtDir"))
  val logFile = new File(config.getString("logFile"))

  // lets start a new log file
  if (logFile.exists) {
    FileUtils.forceDelete(logFile)
  }
  FileUtils.writeStringToFile(logFile, s"${now}\nstarting extraction ...\n")

  // if nxmlDir does not exist there is nothing to do
  if (!nxmlDir.exists) {
    sys.error(s"${nxmlDir.getCanonicalPath} does not exist")
  }

  // if txtDir does not exist create it
  if (!txtDir.exists) {
    println(s"creating ${txtDir.getCanonicalPath}")
    FileUtils.forceMkdir(txtDir)
  } else if (!txtDir.isDirectory) {
    sys.error(s"${txtDir.getCanonicalPath} is not a directory")
  }

  println("initializing NxmlReader ...")
  val nxmlReader = new NxmlReader(
    config.getStringList("nxml2fries.ignoreSections").asScala)

  // process papers in parallel
  for (file <- nxmlDir.listFiles.par if file.getName.endsWith(".nxml")) {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startTime = now // start measuring time here
    val startNS = System.nanoTime

    // Process individual sections and collect all mentions
    val entries = Try(nxmlReader.readNxml(file)) match {
      case Success(v) => v
      case Failure(e) =>
        val report = s"""
          |==========
          |
          | ¡¡¡ NxmlReader error !!!
          |
          |paper: $paperId
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
        Nil
    }

    val outFile = new File(txtDir, s"$paperId.tsv")
    FileUtils.writeLines(outFile, (entries map (_.toString)).asJavaCollection)
  }

}
