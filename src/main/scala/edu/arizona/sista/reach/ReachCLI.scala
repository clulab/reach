package edu.arizona.sista.reach

import java.io.File
import java.util.Date

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.{ Try,Success,Failure }
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.extern.export._
import edu.arizona.sista.reach.extern.export.fries._
import edu.arizona.sista.reach.extern.export.indexcards._
import edu.arizona.sista.reach.nxml._
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.context.rulebased._
import edu.arizona.sista.reach.extern.export.context._
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

  def processPapers(threadLimit: Option[Int]): Int = {
    println("initializing reach ...")
    val reach = new ReachSystem(contextEngineType=contextEngineType, contextParams=contextEngineParams)

    println("initializing NxmlReader ...")
    val nxmlReader = new NxmlReader(ignoreSections)

    var errorCount = 0

    // process papers in parallel
    val files = nxmlDir.listFiles.par
    // limit parallelization
    if (threadLimit.nonEmpty) {
      files.tasksupport =
        new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit.get))
    }
    for (file <- files if file.getName.endsWith(".nxml")) {
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

      // These documents are sorted
      val documents = new mutable.ArrayBuffer[Document]
      val paperMentions = new mutable.ArrayBuffer[BioMention]
      //val mentionsEntriesMap = new mutable.HashMap[BioMention, FriesEntry]()
      for (entry <- entries) {
        try {
          // Create a document instance per entry and add it to the cache
          documents += reach.mkDoc(entry.text, entry.sectionId, entry.chunkId)
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

      try{
        val mentions:Seq[BioMention] = reach.extractFrom(entries, documents)
        paperMentions ++= mentions
      } catch {
        case e: Exception =>
         val report = s"""
             |==========
             |
             | ¡¡¡ extraction error !!!
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
       }


      // done processing
      val endTime = ReachCLI.now
      val endNS = System.nanoTime

      try outputType match {
        case "context-output" =>

          // Create paper directory
          val paperDir = new File(outputDir, paperId)

          if(!paperDir.exists){
            paperDir.mkdir
          }

          // These are the intervals for generating HTML files
          val outputter = new IntervalOutput(documents, entries, paperMentions)
          // Write the context stuff
          val ctxSentencesFile = new File(paperDir, "sentences.txt")
          FileUtils.writeLines(ctxSentencesFile, outputter.sentences.asJavaCollection)

          val ctxEventsFile = new File(paperDir, "event_intervals.txt")
          FileUtils.writeLines(ctxEventsFile, outputter.evtIntervals.asJavaCollection)

          val ctxMentionsFile = new File(paperDir, "mention_intervals.txt")
          FileUtils.writeLines(ctxMentionsFile, outputter.ctxMentions.asJavaCollection)

          val ctxSectionsFile = new File(paperDir, "sections.txt")
          FileUtils.writeLines(ctxSectionsFile, outputter.sections.asJavaCollection)

          val ctxReachEventsFile = new File(paperDir, "reach_events.txt")
          FileUtils.writeLines(ctxReachEventsFile, outputter.eventLines.asJavaCollection)

          val ctxIsTitlesFile = new File(paperDir, "titles.txt")
          FileUtils.writeLines(ctsIsTitlesFile, outputer.titles.asJavaCollection)

          // These are the context plotfiles
            // Write obs.txt
          val contextEngine = reach.contextCache(paperId)

          contextEngine match {
            case ce:RuleBasedContextEngine =>
              val obs = ce.getObservationsMatrixStrings
              FileUtils.writeLines(new File(paperDir, "obs.txt"), obs.asJavaCollection)
              val states = ce.getStatesMatrixStrings
              FileUtils.writeLines(new File(paperDir, "states.txt"), states.asJavaCollection)
            case _ =>
              // So far, these only makes sense if we use a rule based context engine
              Unit
          }

          // Context_events.txt created by python!!!

          // Observation (features) vocabulary. These are descriptions
          val obsLabelsFile = new File(outputDir, "obs_labels.txt")
          if(!obsLabelsFile.exists){
            val obs_labels = ContextEngine.featureVocabulary.values.toList
            FileUtils.writeLines(obsLabelsFile, obs_labels.asJavaCollection)
          }

          // Context (states) vocabulary. These are descriptions
          val statesLabelsFile = new File(outputDir, "states_labels.txt")
          if(!statesLabelsFile.exists){
            val states_labels = ContextEngine.latentVocabulary.values.toList
            FileUtils.writeLines(statesLabelsFile, states_labels.asJavaCollection)
          }

          val statesIdsFile = new File(outputDir, "states_keys.txt")
          if(!statesIdsFile.exists){
            val statesIds = ContextEngine.latentVocabulary.keys.map( x => x._2)
            FileUtils.writeLines(statesIdsFile, statesIds.asJavaCollection)
          }
          FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)

        case "text" =>
          val mentionMgr = new MentionManager()
          val lines = mentionMgr.sortMentionsToStrings(paperMentions)
          val outFile = new File(outputDir, s"$paperId.txt")
          println(s"writing ${outFile.getName} ...")
          FileUtils.writeLines(outFile, lines.asJavaCollection)
          FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)
        // Anything that is not text (including Fries-style output)
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

  def outputMentions(
    mentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    outputType:String,
    paperId:String,
    startTime:Date,
    endTime:Date,
    outputDir:File
  ) = {
    val outFile = outputDir + File.separator + paperId
    // println(s"Outputting to $outFile using $outputType")

    val outputter:JsonOutputter = outputType.toLowerCase match {
      case "fries" => new FriesOutput()
      case "indexcard" => new IndexCardOutput()
      case _ => throw new RuntimeException(s"Output format ${outputType.toLowerCase} not yet supported!")
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

  // for context engine
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  val contextConfig = config.getConfig("contextEngine.params").root
  val contextEngineParams: Map[String, String] =
    context.createContextEngineParams(contextConfig)

  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")

  println(s"Context engine: $contextEngineType\tParams: $contextEngineParams")

  // lets start a new log file
  if (logFile.exists) {
    FileUtils.forceDelete(logFile)
  }
  FileUtils.writeStringToFile(logFile, s"$now\nstarting extraction ...\n")

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

  cli.processPapers(Some(threadLimit))

  def now = new Date()
}
