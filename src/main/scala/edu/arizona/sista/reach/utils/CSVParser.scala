package edu.arizona.sista.reach.utils

import java.io.File

import scala.collection.mutable.ArrayBuffer

import CSVParser._

/**
 * Simple utility to tokenize a CSV file
 * User: mihais
 * Date: 10/30/15
 */
class CSVParser {
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
