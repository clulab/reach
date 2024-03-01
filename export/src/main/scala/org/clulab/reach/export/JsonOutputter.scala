package org.clulab.reach.export

import java.io._
import java.util.Date
import scala.collection.mutable
import ai.lum.nxmlreader.NxmlDocument
import com.typesafe.scalalogging.Logger
import org.clulab.odin.Mention
import org.clulab.reach.FriesEntry
import org.clulab.reach.ReachConstants._
import org.clulab.odin.serialization.json.MentionOps
import org.json4s.jackson.Serialization
import org.slf4j.LoggerFactory


/**
  * Trait for output formatters which output JSON formats.
  *   Written by Tom Hicks. 5/22/2015.
  *   Last Modified: Update for Amount event. Add missing method doc strings.
  */
trait JsonOutputter {

  /**
    * Returns the given mentions in some JSON-based format, as one big string.
    * Default method to be overridden by each JSON output formatter.
    * The processing start and stop date/times are given.
    * The output filename prefix is provided for use by the generator routines, as needed.
    */
  def toJSON (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    outFilePrefix:String): String

  /**
    * Returns the given mentions in some JSON-based format, as one big string.
    * Alternate interface: takes document and extracts passages to output mentions.
    * The processing start and stop date/times are given.
    * The output filename prefix is provided for use by the generator routines, as needed.
    */
  def toJSON (
      paperId: String,
      allMentions: Seq[Mention],
      nxmldoc: NxmlDocument,
      startTime: Date,
      endTime: Date,
      outFilePrefix: String
  ): String = {
    toJSON(paperId, allMentions, nxmlToEntries(nxmldoc), startTime, endTime, outFilePrefix)
  }


  /**
    * Outputs the given mentions to the given output file in some JSON-based format.
    * The processing start and stop date/times are given.
    * The output file is given as a prefix, in case outputters choose to generate
    * multiple output files (e.g., see FriesOutput).
    */
  def writeJSON (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    outFilePrefix:String
  ): Unit = writeJSON(paperId, allMentions, paperPassages, startTime, endTime, outFilePrefix)

  def writeJSON(
    paperId: String,
    allMentions: Seq[Mention],
    nxmldoc: NxmlDocument,
    startTime: Date,
    endTime: Date,
    outFilePrefix: String
  ): Unit = {
    writeJSON(paperId, allMentions, nxmlToEntries(nxmldoc), startTime, endTime, outFilePrefix)
  }


  /** Take NXML document and return a sequence of passages from it. */
  private def nxmlToEntries (nxmldoc: NxmlDocument): Seq[FriesEntry] = {
    Seq(new FriesEntry(nxmldoc))
  }

}


/**
  * Companion object containing types and formatting utilities used by all formatters.
  */
object JsonOutputter {
  // some useful global type definitions
  type PropMap = scala.collection.mutable.HashMap[String, Any]
  type FrameList = scala.collection.mutable.MutableList[PropMap]  // has O(c) append
  type StringList = scala.collection.mutable.MutableList[String]  // has O(c) append

  // required for json output serialization:
  implicit val formats = org.json4s.DefaultFormats

  protected lazy val logger: Logger =
    Logger(LoggerFactory.getLogger(getClass.getName))

  val RUN_ID = "r1"
  val COMPONENT = "Reach"
  val ORGANIZATION = "UAZ"
  val METADATA_SECTION_NAME = "meta-data"

  /** Returns a map of metadata names to values, extracted from the given paper passages. */
  def extractOtherMetaData (paperPassages: Seq[FriesEntry]): Map[String, String] = {
    val metaPassages = paperPassages.filter(_.sectionId == METADATA_SECTION_NAME)
    val metaData = new mutable.HashMap[String, String]()
    for(md <- metaPassages) metaData += md.sectionName -> md.text
    metaData.toMap
  }

  /** Select an argument-type output string for the given mention label. */
  def mkArgType (arg:Mention): String = {
    if (arg matches "Complex") "complex"
    else if (arg matches "Entity") "entity"
    else if (arg matches "Site") "entity"
    else if (arg matches "Cellular_component") "entity"
    else if (arg matches "CellLine") "entity"
    else if (arg matches "CellType") "entity"
    else if (arg matches "Organ") "entity"
    else if (arg matches "Species") "entity"
    else if (arg matches "TissueType") "entity"
    else if (arg matches "Event") "event"
    else throw new RuntimeException("ERROR: unknown event type: " + arg.labels)
  }

  /** Select an event-type output string for the given mention label. */
  def mkEventType(mention: Mention): String = {
    val label = mention.label
    if (MODIFICATION_EVENTS.contains(label))
      "protein-modification"
    else if (label == "Binding")
      "complex-assembly"
    else if (label == "Transcription")
      "transcription"
    else if (label == "Translocation")
      "translocation"
    else if (label == "Complex")
      "complex-assembly"
    else if (label == "Conversion")
      "conversion"
    else if (AMOUNT_EVENTS.contains(label))
      "amount"
    else if (REGULATION_EVENTS.contains(label))
      "regulation"
    else if (ACTIVATION_EVENTS.contains(label))
      "activation"
    else {
      // "Gene_or_gene_product" is another possibility.
      // Also "Family", "Disease", "Simple_chemical"
      val json = MentionOps(mention).json(pretty = true)
      val message = s"""Unknown event type "$label" in event:\n$json"""
      // throw new RuntimeException(message)
      logger.warn(message)
      "unknown"
    }
  }

  /** Canonicalize the given label string. */
  def prettifyLabel (label:String): String = label.toLowerCase.replaceAll("_", "-")

  /** Convert the entire output data structure to JSON and write it to the given file. */
  def writeJsonToFile (model:PropMap, outFile:File) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    Serialization.writePretty(model, out)
    out.println()                           // add final newline which serialization omits
    out.flush()
    out.close()
  }

  /** Convert the entire output data structure to JSON and return it as a string. */
  def writeJsonToString (model:PropMap): String = {
    val out:StringWriter = new StringWriter()
    Serialization.writePretty(model, out)
    out.flush()                             // closing a string writer has no effect
    out.toString
  }

}
