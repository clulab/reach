package org.clulab.reach

import java.io.File
import java.util.Date

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.extern.export.indexcards.IndexCardOutput
import org.clulab.reach.utils.DSVParser


/**
 * Parses a CSV file with custom sentences and/or paragraphs
 * User: mihais
 * Date: 10/30/15
 */
object ReachCSV extends App with LazyLogging {
  //
  // Customize these for various CSV files
  //

  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  // arguments
  // NOTE: we could also read .tsv files
  val csvFile = new File(config.getString("csv"))
  val outputDir = config.getString("outDir")
  // NOTE: these aren't listed in the application.conf under src/main/resources
  val docIdColumn = config.getInt("docIdColumn")
  val chunkIdColumn = config.getInt("chunkIdColumn")
  val sectionIdColumn = -1
  val textColumn = config.getInt("textColumn")
  val hasHeader = config.getBoolean("hasHeader")

  val reach = new ReachSystem
  val csvParser = new DSVParser

  var count = 0
  for(entry <- csvParser.toFriesEntries(csvFile,
    docIdColumn = docIdColumn,
    chunkIdColumn = chunkIdColumn,
    sectionIdColumn = sectionIdColumn,
    textColumn = textColumn,
    hasHeader = hasHeader)) {
    //println(s"""Parsing line: ${line.mkString("; ")}""")
    logger.info(s"Processing ${entry.name}, ${entry.chunkId}...")

    try {
      val startTime = now
      val mentions = reach.extractFrom(entry)
      val endTime = now

      val outputtter = new IndexCardOutput
      outputtter.writeJSON(
        entry.name + "_" + entry.chunkId,
        mentions,
        List(entry),
        startTime, endTime,
        outputDir + File.separator + entry.name + "_" + entry.chunkId
      )

    } catch {
      case e: Throwable =>
        val report = s"""
                      |==========
                      |
                      | ¡¡¡ extraction error !!!
                      |
                      |paper: ${entry.name}
                      |chunk: ${entry.chunkId}
                      |section: ${entry.sectionId}
                      |
                      |error:
                      |${e.toString}
                      |
                      |stack trace:
                      |${e.getStackTrace.mkString("\n")}
                      |
                      |==========
                      |""".stripMargin
    }

    count += 1
    logger.info(s"Processed $count lines.")
  }

  def now = new Date()
}
