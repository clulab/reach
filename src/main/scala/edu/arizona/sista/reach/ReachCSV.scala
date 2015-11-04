package edu.arizona.sista.reach

import java.io.File
import java.util.Date

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.reach.extern.export.indexcards.IndexCardOutput
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.reach.utils.CSVParser

/**
 * Parses a CSV file with custom sentences and/or paragraphs
 * User: mihais
 * Date: 10/30/15
 */
object ReachCSV extends App {
  //
  // Customize these for various CSV files
  //

  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  // arguments
  val csvFile = new File(config.getString("csv"))
  val outputDir = config.getString("output")
  val docIdColumn = config.getInt("docIdColumn")
  val chunkIdColumn = config.getInt("chunkIdColumn")
  val textColumn = config.getInt("textColumn")
  val hasHeader = config.getBoolean("hasHeader")

  val reach = new ReachSystem
  val csvParser = new CSVParser

  var count = 0
  for(line <- csvParser.split(csvFile, removeHeader = hasHeader)) {
    //println(s"""Parsing line: ${line.mkString("; ")}""")
    val text = line(textColumn)
    val docId = line(docIdColumn)
    val chunkId = line(chunkIdColumn)
    val entry = FriesEntry(docId, chunkId, "", "", isTitle = false, text) // we have no section information for this CSV data

    try {
      val startTime = now
      val mentions = reach.extractFrom(entry)
      val endTime = now

      val outputtter = new IndexCardOutput
      outputtter.writeJSON(
        docId + "_" + chunkId,
        mentions,
        List(entry),
        startTime, endTime,
        outputDir + File.separator + docId + "_" + chunkId
      )

    } catch {
      case e: Exception =>
        val report = s"""
                      |==========
                      |
                      | ¡¡¡ extraction error !!!
                      |
                      |paper: $docId
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
    println(s"Processed $count lines.")
  }

  def now = new Date()
}
