package org.clulab.reach.export.serial

import java.io.{File, PrintWriter}
import java.util.Date
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import ai.lum.common.FileUtils._
import com.fasterxml.jackson.databind.ObjectWriter
import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention
import org.clulab.reach.FriesEntry
import org.clulab.reach.export.JsonOutputter
import org.clulab.reach.mentions.{MentionOps => ImplicitMentionOps}
import org.clulab.reach.mentions.serialization.json.MentionsOps
import org.clulab.serialization.json.stringify
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sink
import org.json4s.JValue
import org.json4s.jackson.{JsonMethods, prettyJson, renderJValue}

/**
  * Defines classes and methods used to output the serial-json output format.
  *   Written by: Tom Hicks. 2/9/2016.
  *   Last Modified: Rename output type and this class.
  */
class SerialJsonOutput (

  /** The character set encoding to use when writing out the JSON to a file. */
  encoding: Charset = UTF_8

) extends JsonOutputter with LazyLogging {
  val objectWriter = JsonMethods.mapper.writerWithDefaultPrettyPrinter()

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
    MentionsOps(mentions).json(pretty = true)
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
    val mentions = allMentions.map(_.toCorefMention)
    val jsonAST = MentionsOps(mentions).jsonAST
    // Code here has been modified so that no json string is produced.
    // String lengths max out at 2GB, unlike files, and with large inputs
    // we were crashing when output could not be stuffed into a string.
    val renderedJsonAST = JsonMethods.render(jsonAST)
    val file = new File(outFilePrefix + ".json")
    val printWriter = new PrintWriter(new Sink(file, encoding.name, append = false))

    printWriter.autoClose { printWriter =>
      objectWriter.writeValue(printWriter, renderedJsonAST)
    }
  }
}
