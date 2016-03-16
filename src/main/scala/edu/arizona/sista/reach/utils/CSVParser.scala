package edu.arizona.sista.reach.utils

import java.io.File

import edu.arizona.sista.reach.nxml.FriesEntry

import scala.collection.mutable.{ListBuffer, ArrayBuffer}

import CSVParser._

/**
 * Simple utility to tokenize a CSV file
 * User: mihais
 * Date: 10/30/15
 */
class CSVParser {
  /**
    * Converts a CSV file into a sequence of FriesEntry blocks, similar to NxmlReader.readNxml
    *
    * @param file input file in CSV format
    * @param docIdColumn which column contains the doc id
    * @param chunkIdColumn which column contains the unique id for the segment of text reported in each line
    * @param sectionIdColumn which column contains the section id for this text; -1 if not present
    * @param textColumn which column contains the actual text
    * @param hasHeader true if this CSV file has a header row
    */
  def toFriesEntries(
    file:File,
    docIdColumn:Int = 0,
    chunkIdColumn:Int = 1,
    sectionIdColumn:Int = -1,
    textColumn:Int = 2,
    hasHeader:Boolean = true): Seq[FriesEntry] = {
    val entries = new ListBuffer[FriesEntry]
    for(line <- split(file, removeHeader = hasHeader)) {
      val text = line(textColumn)
      val docId = line(docIdColumn)
      val chunkId = line(chunkIdColumn)
      val sectionId = sectionIdColumn match {
        case -1 => ""
        case _ => line(sectionIdColumn)
      }
      val entry = FriesEntry(docId, chunkId, sectionId, "", isTitle = false, text)
      entries += entry
    }
    entries.toSeq
  }


  def split(file:File, removeHeader:Boolean):Array[Array[String]] = {
    val result = new ArrayBuffer[Array[String]]
    var lineCount = 0
    for(line <- FileReader.readFile(file)) {
      //println(s"LINE $line")
      if(lineCount > 0 || ! removeHeader) {
        val tokens = splitLine(line)
        result += tokens
      }
      lineCount += 1
    }
    result.toArray
  }

  def splitLine(line:String):Array[String] = {
    //println(s"SPLITTING LINE: $line")
    var offset = 0
    val tokens = new ArrayBuffer[String]
    while(offset < line.length) {
      val sep = findNextSep(line, offset)
      //println(s"Found token from $offset to $sep")
      val token = trim(line.substring(offset, sep))
      offset = sep + 1
      tokens += token
    }
    //println("TOKENS: " + tokens.mkString("; "))
    tokens.toArray
  }

  def findNextSep(line:String, offset:Int):Int = {
    var pos = skipWhiteSpaces(line, offset)
    pos = skipQuotedString(line, pos)
    pos = findNext(line, pos, SEP)
    pos
  }

  def skipQuotedString(line:String, offset:Int):Int = {
    if(offset < line.length && line.charAt(offset) == QUOTE) {
      //println(s"Found START quote at $offset")
      val end = findNext(line, offset + 1, QUOTE) + 1
      //println(s"Found END quote at $end")
      end
    } else {
      offset
    }
  }

  def findNext(line:String, offset:Int, sep:Char):Int = {
    var pos = offset
    while(pos < line.length && line.charAt(pos) != sep) // TODO: handle escaped separators here
      pos += 1
    pos
  }

  def skipWhiteSpaces(line:String, start:Int):Int = {
    var pos = start
    while(pos < line.length && Character.isWhitespace(line.charAt(pos)))
      pos += 1
    pos
  }

  def trim(token:String):String = {
    val trimmed = token.trim
    if(trimmed.startsWith("\"")) {
      trimmed.substring(1, trimmed.length - 1).trim
    } else {
      trimmed
    }
  }

  def split(fileName:String, removeHeader:Boolean):Array[Array[String]] = {
    val file = new File(fileName)
    split(file, removeHeader)
  }
}

object CSVParser {
  val SEP = ','
  val QUOTE = '\"'
}
