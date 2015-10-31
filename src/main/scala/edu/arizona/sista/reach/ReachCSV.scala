package edu.arizona.sista.reach

import java.io.File

import com.typesafe.config.ConfigFactory
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
  val EXPECTED_COLUMN_COUNT = 4
  val DOCID_COLUMN = 0
  val CHUNKID_COLUMN = 1
  val TEXT_COLUMN = 2
  val HAS_HEADER = true

  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val csvFile = new File(config.getString("csv"))

  val reach = new ReachSystem
  val csvParser = new CSVParser

  var count = 0
  for(line <- csvParser.split(csvFile, removeHeader = HAS_HEADER)) {
    //println(s"""Parsing line: ${line.mkString("; ")}""")
    assert(line.length == EXPECTED_COLUMN_COUNT)
    val text = line(TEXT_COLUMN)
    val docId = line(DOCID_COLUMN)
    val chunkId = line(CHUNKID_COLUMN)

    val mentions = reach.extractFrom(text, docId, chunkId)

    count += 1
    println(s"Processed $count lines.")
  }


}
