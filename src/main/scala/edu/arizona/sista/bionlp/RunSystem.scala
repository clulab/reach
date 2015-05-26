package edu.arizona.sista.bionlp

import java.io.File
import java.util.Date
import scala.collection.JavaConverters._
import scala.collection.mutable
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin.extern.export.JsonOutputter
import edu.arizona.sista.odin.extern.export.hans._
import edu.arizona.sista.odin.extern.export.reach._

object RunSystem extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val nxmlDir = new File(config.getString("nxmlDir"))
  val friesDir = new File(config.getString("friesDir"))
  val encoding = config.getString("encoding")
  val outputType = config.getString("outputType")
  val logFile = new File(config.getString("logFile"))

  // lets start a new log file
  if (logFile.exists) {
    FileUtils.forceDelete(logFile)
    FileUtils.writeStringToFile(logFile, s"${now}\nstarting extraction ...\n")
  }

  // if nxmlDir does not exist there is nothing to do
  if (!nxmlDir.exists) {
    sys.error(s"${nxmlDir.getCanonicalPath} does not exist")
  }

  // if friesDir does not exist create it
  if (!friesDir.exists) {
    println(s"creating ${friesDir.getCanonicalPath}")
    FileUtils.forceMkdir(friesDir)
  } else if (!friesDir.isDirectory) {
    sys.error(s"${friesDir.getCanonicalPath} is not a directory")
  }

  println("initializing reach ...")
  val reach = new ReachSystem

  println("initializing nxml2fries ...")
  val nxml2fries = new Nxml2Fries(
    config.getString("nxml2fries.executable"),
    config.getBoolean("nxml2fries.removeCitations"),
    config.getStringList("nxml2fries.ignoreSections").asScala.toSet,
    encoding)

  // process papers in parallel
  for (file <- nxmlDir.listFiles.par if file.getName.endsWith(".nxml")) {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startTime = now // start measuring time here

    // process individual sections and collect all mentions
    val entries = try {
      nxml2fries.extractEntries(file)
    } catch {
      case e: Exception =>
        val report = s"""
          |==========
          |
          | ¡¡¡ nxml2fries error !!!
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

    val paperMentions = new mutable.ArrayBuffer[BioMention]
    for (entry <- entries) {
      try {
        paperMentions ++= reach.extractFrom(entry)
      } catch {
        case e: Exception =>
          val report = s"""
            |==========
            |
            | ¡¡¡ extraction error !!!
            |
            |paper: $paperId
            |chunk: ${entry.chunkId}
            |section: ${entry.sectionId}
            |section name: ${entry.sectionName}
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
      }
    }

    // done processing
    val endTime = now

    if (outputType != "text") {             // if reach will handle output
      outputMentions(paperMentions, entries, outputType, paperId, startTime, endTime, friesDir)
    }
    else {                                  // else dump all paper mentions to file
      try {
        val mentionMgr = new MentionManager()
        val lines = mentionMgr.sortMentionsToStrings(paperMentions)
        val outFile = new File(friesDir, s"$paperId.txt")
        println(s"writing ${outFile.getName} ...")
        FileUtils.writeLines(outFile, lines.asJavaCollection)
      } catch {
        case e: Exception =>
          val report = s"""
            |==========
            |
            | ¡¡¡ serialization error !!!
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
            """.stripMargin
          FileUtils.writeStringToFile(logFile, report, true)
      }
    }
  }

  def now = new Date()

  def outputMentions(mentions:Seq[Mention],
                     paperPassages:Seq[FriesEntry],
                     outputType:String,
                     paperId:String,
                     startTime:Date,
                     endTime:Date,
                     outputDir:File) = {
    val outFile = outputDir + File.separator + paperId

    val outputter:JsonOutputter = outputType.toLowerCase match {
      case "hans" => new HansOutput()
      case      _ => new ReachOutput()
    }

    outputter.toJSON(paperId, mentions, paperPassages, startTime, endTime, outFile)
  }

}
