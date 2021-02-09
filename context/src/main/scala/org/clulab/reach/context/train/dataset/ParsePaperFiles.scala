package org.clulab.reach.context.train.dataset

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.context.utils.PaperExtraction
import org.clulab.struct.Interval
import org.clulab.utils.Serializer

import java.io.File
import scala.io.Source

case class ManuallyAnnotatedData(extractions: Seq[PaperExtraction], annotations: Map[PaperExtraction, Seq[String]])

/**
 * Parses the original paper files into data structures suitable for the rest of the pipeline
 */
object ParsePaperFiles extends App with LazyLogging {

  val config = ConfigFactory.load().getConfig("parsePaperFiles")

  val dir = config.getString("filesDirectory")
  val outputPath = config.getString("outputFile")

  def parsePaperFiles(dirPath: String): ManuallyAnnotatedData = {
    // First the event intervals
    val dir = new File(dirPath)
    val eventsFile = new File(dir, "event_intervals.txt")
    val annotatedEventsFile = new File(dir, "annotated_event_intervals.tsv")
    val mentionsFile = new File(dir, "mention_intervals.txt")
    val manualMentionsFile = new File(dir, "manual_context_mentions.tsv")
    val textFile = new File(dir, "sentences.txt")

    val docTokens = readText(textFile)

    val (annotatedEventsIntervals, eventAnnotations) = parsedAnnotatedEvents(annotatedEventsFile, docTokens)

    ManuallyAnnotatedData(
      (parseEvents(eventsFile, docTokens) ++
        annotatedEventsIntervals ++
        parseMentions(mentionsFile, docTokens) ++
        parseManualMentions(manualMentionsFile, docTokens)).distinct,
      eventAnnotations)
  }

  def parseEvents(file: File, docTokens: Array[Array[String]]): Seq[PaperExtraction] = {
    val src = Source.fromFile(file)
    val lines = src.getLines().toList
    src.close()

    lines flatMap {
      line =>
        val tokens = line.split(" ")
        val sent = tokens(0).toInt
        val intervals = tokens.tail map {
          tok =>
            val nums = tok.split("-")
            val (start, end) = (nums(0), nums(1))
            Interval(start.toInt, end.toInt + 1)
        }

        for (interval <- intervals) yield {
          val text = docTokens(sent).slice(interval.start, interval.end).mkString(" ")
          PaperExtraction(sent, interval, text, "Event")
        }
    }
  }

  def parsedAnnotatedEvents(file:File,docTokens:Array[Array[String]]):(Seq[PaperExtraction], Map[PaperExtraction, Seq[String]]) = {
    val src = Source.fromFile(file)
    val lines = src.getLines().toList
    src.close()


    val (extractions, rawAnnotations) =
      (lines map {
        line =>
          val tokens = line.split("\t")
          val sent = tokens(0).toInt

          val nums = tokens(1).split("-")
          val (start, end) = (nums(0), nums(1))
          val ids =
            if (tokens.length == 3)
              tokens(2).split(",").toSeq
            else
              Seq()
          val interval = Interval(start.toInt, end.toInt + 1)
          val text = docTokens(sent).slice(interval.start, interval.end).mkString(" ")


          (PaperExtraction(sent, interval, text, "Event"), ids)

      }).unzip

    val annotations = (extractions zip rawAnnotations).toMap

    (extractions, annotations)
  }

  def parseMentions(mentionsFile: File, docTokens:Array[Array[String]]) = {
    val src = Source.fromFile(mentionsFile)
    val lines = src.getLines().toList
    src.close()

    lines flatMap {
      line =>
        val tokens = line.split(" ")
        val sent = tokens(0).trim().toInt

        tokens.tail map {
          elem =>
            val tokens = elem.split("%")
            val (start, end) = (tokens(0), tokens(1))
            val id = tokens(3)
            val interval = Interval(start.toInt, end.toInt + 1)
            val text = docTokens(sent).slice(interval.start, interval.end).mkString(" ")


            PaperExtraction(sent, interval, text, id)
        }
    }
  }

  def readText(textFile: File): Array[Array[String]] = {
    val src = Source.fromFile(textFile)
    val lines = src.getLines().toArray
    src.close()

    lines map (_.split(" "))
  }

  def parseManualMentions(manualMentionsFile: File, docTokens: Array[Array[String]]) = {
    val src = Source.fromFile(manualMentionsFile)
    val lines = src.getLines().toList
    src.close()

    lines map {
      line =>
        val tokens = line.split("\t")
        val sent = tokens(0).toInt

        val nums = tokens(1).split("-")
        val (start, end) = (nums(0), nums(1))
        val id = tokens(2)
        val interval = Interval(start.toInt, end.toInt + 1)
        val text = docTokens(sent).slice(interval.start, interval.end).mkString(" ")


        PaperExtraction(sent, interval, text, id)

    }
  }

  logger.info(s"Reading data form $dir")
  val paperDirs = for (f <- new File(dir).listFiles(); if f.isDirectory) yield f.getAbsolutePath
  val extractions = paperDirs.map(p => new File(p).getName -> parsePaperFiles(p)).toMap

  logger.info(s"Saving output to $outputPath")
  Serializer.save(extractions, outputPath)

}
