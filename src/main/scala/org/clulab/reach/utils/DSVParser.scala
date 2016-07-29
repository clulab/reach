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
  @deprecated(message = "Use .toFriesEntry to produce a single FriesEntry for the entire paper.", since = "Issue #210 (https://github.com/clulab/reach/issues/210)")
  def toFriesEntries(
    file: File,
    docIdColumn: Int = 0,
    chunkIdColumn: Int = 1,
    sectionIdColumn: Int = -1,
    textColumn: Int = 2,
    hasHeader: Boolean = true): Seq[FriesEntry] = {
    // Sniff out the delimiter based on the file's extension
    val delimiter: String = getDelimiter(file)

    val numCols = if (sectionIdColumn != -1) 4 else 3

    // remove header if present
    val lines = hasHeader match {
      case true => scala.io.Source.fromFile(file).getLines.drop(1)
      case false => scala.io.Source.fromFile(file).getLines
    }
    for (line <- lines.toSeq) yield {

      val columns: Seq[String] = line.split(delimiter, numCols)
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
        text = trim(text)
      )
    }
  }

  def toFriesEntry(
    file: File,
    docIdColumn: Int = 0,
    chunkIdColumn: Int = 1,
    sectionIdColumn: Int = -1,
    textColumn: Int = 2,
    hasHeader: Boolean = true
  ): FriesEntry = {
    // Sniff out the delimiter based on the file's extension
    val delimiter: String = getDelimiter(file)

    val numCols = if (sectionIdColumn != -1) 4 else 3

    // remove header if present
    val lines = hasHeader match {
      case true => scala.io.Source.fromFile(file).getLines.drop(1)
      case false => scala.io.Source.fromFile(file).getLines
    }

    val docID = lines.toSeq.head.split(delimiter, numCols)(docIdColumn)

    val text = for (line <- lines.toSeq) yield {
      val columns: Seq[String] = line.split(delimiter, numCols)
      trim(columns(textColumn))
    }

    // We now produce a single FriesEntry for the entire paper
    FriesEntry(
      name = trim(docID),
      // we'll use the docID for the chunk
      chunkId = trim(docID),
      sectionId = "",
      sectionName = "",
      isTitle = false,
      text = text.mkString("\n")
    )
  }

  /** Sniff out the delimiter based on the file's extension */
  def getDelimiter(file: File): String = FilenameUtils.getExtension(file.getName) match {
    case "tsv" => "\t"
    case _ => ","
  }

  /** Remove extra quotes surrounding a text string */
  def trim(text: String): String = text match {
    case DSVParser.Quoted(inside) => inside
    case _ => text
  }
}

object DSVParser {

  val Quoted = new Regex("^\"(.*?)\"$")
}
