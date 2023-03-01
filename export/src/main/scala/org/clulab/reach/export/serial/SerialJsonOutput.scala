package org.clulab.reach.export.serial

import java.io.File
import java.util.Date
import java.util.regex.Pattern

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8

import ai.lum.common.FileUtils._

import com.typesafe.scalalogging.LazyLogging

import org.clulab.odin.Mention
import org.clulab.reach.FriesEntry
import org.clulab.reach.export.JsonOutputter
import org.clulab.reach.mentions.{MentionOps => ImplicitMentionOps}
import org.clulab.reach.mentions.serialization.json.MentionsOps

/**
  * Defines classes and methods used to output the serial-json output format.
  *   Written by: Tom Hicks. 2/9/2016.
  *   Last Modified: Rename output type and this class.
  */
class SerialJsonOutput (

  /** The character set encoding to use when writing out the JSON to a file. */
  encoding: Charset = UTF_8

) extends JsonOutputter with LazyLogging {

  /** Returns the given mentions in the serial-json format, as one big string. */
  override def toJSON (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    outFilePrefix:String
  ): String = {
    val mentions = allMentions.map(_.toCorefMention)
    MentionsOps(mentions).json(true)                     // true = pretty print
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
    val f: File = new File(outFilePrefix + ".json")
    val mentions = allMentions.map(_.toCorefMention)

    f.writeString(
      string = MentionsOps(mentions).json(true),
      charset = encoding, 
      append = false, 
      gzipSupport = false
    )
  }

}
