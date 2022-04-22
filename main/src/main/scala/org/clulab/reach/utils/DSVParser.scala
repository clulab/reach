package org.clulab.reach.utils

import java.io.File

import org.apache.commons.io.FilenameUtils
import org.clulab.reach.FriesEntry

import scala.util.matching.Regex


class DSVParser {

  /**
    * Converts a DSV file into a sequence of FriesEntry blocks, similar to NxmlReader.readNxml
    *
    * @param file input file in DSV format
    * @param docIdColumn which column contains the doc id
    * @param chunkIdColumn which column contains the unique id for the segment of text reported in each line
    * @param sectionIdColumn which column contains the section id for this text; -1 if not present
    * @param textColumn which column contains the actual text
    * @param hasHeader true if this DSV file has a header row
    */
  def toFriesEntries(
    file: File,
    docIdColumn: Int = 0,
    chunkIdColumn: Int = 1,
    sectionIdColumn: Int = -1,
    textColumn: Int = 2,
    hasHeader: Boolean = true,
    sectionsToIgnore: Set[String] = Set.empty[String]
  ): Seq[FriesEntry] = {
    // Sniff out the delimiter based on the file's extension
    val delimiter: String = getDelimiter(file)
    val splitPattern = getSplitPattern(delimiter)

    val numCols = if (sectionIdColumn != -1) 4 else 3

    // remove header if present
    val lines = hasHeader match {
      case true => scala.io.Source.fromFile(file).getLines.drop(1)
      case false => scala.io.Source.fromFile(file).getLines
    }

    val entries = for (line <- lines.toSeq) yield {

      val columns: Seq[String] = line.split(splitPattern, numCols)
      val docID = columns(docIdColumn)
      val sectionID = if (sectionIdColumn != -1) columns(sectionIdColumn) else ""
      val chunkID = columns(chunkIdColumn)
      val text = columns(textColumn)

      FriesEntry(
        name = trim(docID),
        chunkId = trim(chunkID),
        sectionId = trim(sectionID),
        sectionName = "",
        isTitle = false,
        text = trim(text),
        None
      )
    }

    // remove sections that should be ignored
    entries.filterNot(entry => sectionsToIgnore contains entry.sectionId)
  }

  def toFriesEntry(
    file: File,
    docIdColumn: Int = 0,
    chunkIdColumn: Int = 1,
    sectionIdColumn: Int = -1,
    textColumn: Int = 2,
    hasHeader: Boolean = true,
    sectionsToIgnore: Set[String] = Set.empty[String]
  ): FriesEntry = {
    val entries = toFriesEntries(file, docIdColumn, chunkIdColumn, sectionIdColumn, textColumn, hasHeader, sectionsToIgnore)

    val docID = entries.head.name
    val allText = entries.map(_.text).mkString("\n")
    // Create a single FriesEntry
    FriesEntry(
      name = docID,
      // we'll use the docID as the chunk-id
      chunkId = docID,
      sectionId = "",
      sectionName = "",
      isTitle = false,
      text = allText,
      None
    )
  }

  /** Sniff out the based on the file's extension */
  def getDelimiter(file: File): String = FilenameUtils.getExtension(file.getName) match {
    case "tsv" => "\t"
    case "csv" => ","
    case other => throw new Exception(s"'$other' is not a supported extension")
  }

  /** Create a safe split pattern based on the delimiter */
  def getSplitPattern(delimiter: String): String = delimiter match {
    case "," => DSVParser.CSV
    case "\t" => DSVParser.TSV
    case other => other
  }

  /** Remove extra quotes surrounding a text string */
  def trim(text: String): String = text match {
    case DSVParser.Quoted(inside) => inside
    case _ => text
  }
}

object DSVParser {
  val CSV = """,(?=([^\"]*\"[^\"]*\")*[^\"]*$)"""
  val TSV = """\t"""
  val Quoted = new Regex("^\"(.*?)\"$")
}
