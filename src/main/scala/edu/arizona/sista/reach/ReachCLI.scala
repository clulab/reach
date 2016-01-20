package edu.arizona.sista.reach

import java.io.File
import java.util.Date

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.{ Try,Success,Failure }
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.extern.export._
import edu.arizona.sista.reach.extern.export.fries._
import edu.arizona.sista.reach.extern.export.indexcards._
import edu.arizona.sista.reach.extern.export.context._
import edu.arizona.sista.reach.nxml._
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.context.ContextEngineFactory.Engine
import edu.arizona.sista.reach.context.ContextEngineFactory.Engine._

class ReachCLI(val nxmlDir:File,
               val outputDir:File,
               val encoding:String,
               val outputType:String,
               val ignoreSections:Seq[String],
               val contextEngineType: Engine,
               val contextEngineParams: Map[String, String],
               val logFile:File) {

  def processPapers(): Int = {
    println("initializing reach ...")
    val reach = new ReachSystem(contextEngineType=contextEngineType, contextParams=contextEngineParams)

    println("initializing NxmlReader ...")
    val nxmlReader = new NxmlReader(ignoreSections)

    var errorCount = 0

    // process papers in parallel
    for (file <- nxmlDir.listFiles.par if file.getName.endsWith(".nxml")) {
      val paperId = FilenameUtils.removeExtension(file.getName)
      val startTime = ReachCLI.now // start measuring time here
      val startNS = System.nanoTime

      // Process individual sections and collect all mentions
      val entries = Try(nxmlReader.readNxml(file)) match {
        case Success(v) => v
        case Failure(e) =>
          this.synchronized { errorCount += 1}
          val report =
            s"""
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

      val paperMentions = new mutable.ArrayBuffer[BioMention]
      val mentionsEntriesMap = new mutable.HashMap[BioMention, FriesEntry]()
      for (entry <- entries) {
        try {
          val mentions = reach.extractFrom(entry)
          mentions foreach { m => mentionsEntriesMap += (m -> entry) }
          paperMentions ++= mentions
        } catch {
          case e: Throwable =>
            this.synchronized { errorCount += 1}
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
      val endTime = ReachCLI.now
      val endNS = System.nanoTime

      try outputType match {
        case "text" =>
          val mentionMgr = new MentionManager()
          val lines = mentionMgr.sortMentionsToStrings(paperMentions)
          val outFile = new File(outputDir, s"$paperId.txt")
          println(s"writing ${outFile.getName} ...")
          FileUtils.writeLines(outFile, lines.asJavaCollection)
          FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)
        // Anything that is not text (including Fries-style output)
        case "pandas" =>
          println("Using pandas output ...")
          val outputter:PandasOutput = new PandasOutput()
          val (entities, events, relations, lines) = outputter.toCSV(paperId, paperMentions, mentionsEntriesMap.toMap)
          val outMentions = new File(outputDir, s"$paperId.entities")
          val outEvents = new File(outputDir, s"$paperId.events")
          val outRelations = new File(outputDir, s"$paperId.relations")
          val outLines = new File(outputDir, s"$paperId.lines")
          FileUtils.writeLines(outMentions, entities.asJavaCollection)
          FileUtils.writeLines(outEvents, events.asJavaCollection)
          FileUtils.writeLines(outRelations, relations.asJavaCollection)
          FileUtils.writeLines(outLines, lines.asJavaCollection)

          FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)
        case _ =>
          outputMentions(paperMentions, entries, outputType, paperId, startTime, endTime, outputDir)
          FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)
      } catch {
        case e: Throwable =>
          this.synchronized { errorCount += 1}
          val report =
            s"""
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

    errorCount // should be 0 :)
  }

  def outputMentions(mentions:Seq[Mention],
                     paperPassages:Seq[FriesEntry],
                     outputType:String,
                     paperId:String,
                     startTime:Date,
                     endTime:Date,
                     outputDir:File) = {
    val outFile = outputDir + File.separator + paperId
    // println(s"Outputting to $outFile using $outputType")

    val outputter:JsonOutputter = outputType.toLowerCase match {
      case "fries" => new FriesOutput()
      case "indexcard" => new IndexCardOutput()
      case _ => throw new RuntimeException(s"Output format ${outputType.toLowerCase()} not yet supported!")
    }
    outputter.writeJSON(paperId, mentions, paperPassages, startTime, endTime, outFile)
  }

}

object ReachCLI extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val nxmlDir = new File(config.getString("nxmlDir"))
  val friesDir = new File(config.getString("friesDir"))
  val encoding = config.getString("encoding")
  val outputType = config.getString("outputType")
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala
  val logFile = new File(config.getString("logFile"))

  val contextEngineType:Engine = Engine.withName(config.getString("contextEngine.type"))
  val contextConfig = config.getConfig("contextEngine.params").root
  // TODO: There must be a better way to do this!
  val contextEngineParams:Map[String, String] = contextConfig.keySet.asScala.map{
      key => key -> contextConfig.asScala.apply(key).unwrapped.toString
  }.toMap

  println(s"Context engine: $contextEngineType\tParams: $contextEngineParams")

  // lets start a new log file
  if (logFile.exists) {
    FileUtils.forceDelete(logFile)
  }
  FileUtils.writeStringToFile(logFile, s"${now}\nstarting extraction ...\n")

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

  val cli = new ReachCLI(nxmlDir, friesDir, encoding, outputType,
       ignoreSections, contextEngineType, contextEngineParams, logFile)

  cli.processPapers()

  def now = new Date()
}
