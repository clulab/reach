package org.clulab.reach.export.mentions

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.Date
import java.util.regex.Pattern

import com.typesafe.scalalogging.LazyLogging

import org.clulab.odin.Mention
import org.clulab.reach.FriesEntry
import org.clulab.reach.export.JsonOutputter
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json._

/**
  * Defines classes and methods used to output the Mentions-JSON output format.
  *   Written by: Tom Hicks. 2/9/2016.
  *   Last Modified: Initial creation.
  */
class MentionsJsonOutput (

  /** The character set encoding to use when writing out the JSON to a file. */
  encoding: String = "utf-8"

) extends JsonOutputter with LazyLogging {

  /** Returns the given mentions in the Mentions-JSON format, as one big string. */
  override def toJSON (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    outFilePrefix:String
  ): String = {
    val mentions = allMentions.map(_.toCorefMention)
    mentions.json(true)                     // true = pretty print
  }

  /**
   * Writes the given mentions to an output file in Mention-JSON format.
   * The output file is prefixed with the given prefix string.
   */
  override def writeJSON (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    outFilePrefix:String
  ): Unit = {
    val filename = outFilePrefix + ".json"
    val mentions = allMentions.map(_.toCorefMention)
    Files.write(Paths.get(filename), mentions.json(true).getBytes(encoding))
  }

}
