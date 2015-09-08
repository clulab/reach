package edu.arizona.sista.odin.extern.export

import java.io._
import java.util.Date

import edu.arizona.sista.reach.FriesEntry
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.odin.{TextBoundMention, RelationMention, EventMention, Mention}
import edu.arizona.sista.processors.Document
import org.json4s.native.Serialization

/**
  * Trait for output formatters which output JSON formats.
  *   Written by Tom Hicks. 5/22/2015.
  *   Last Modified: Rename file writing method. Add method to return one big string.
  */
trait JsonOutputter {

  /**
    * Returns the given mentions in some JSON-based format, as one big string.
    * The processing start and stop date/times are given.
    * The input filename prefix is provided for use by the generator routines, as needed.
    * Default method to be overridden by each JSON output formatter.
    */
  def toJSON (paperId:String,
              allMentions:Seq[Mention],
              paperPassages:Seq[FriesEntry],
              startTime:Date,
              endTime:Date,
              outFilePrefix:String): String

  /**
    * Outputs the given mentions to the given output file in some JSON-based format.
    * The processing start and stop date/times are given.
    * The output file is given as a prefix, in case outputters choose to generate
    * multiple output files (e.g., see FriesOutput)
    * Default method to be overridden by each JSON output formatter.
    */
  def writeJSON (paperId:String,
                 allMentions:Seq[Mention],
                 paperPassages:Seq[FriesEntry],
                 startTime:Date,
                 endTime:Date,
                 outFilePrefix:String)

}

/** Contain formatting utilities used by all formatters */
object JsonOutputter {
  type PropMap = scala.collection.mutable.HashMap[String, Any]
  type FrameList = scala.collection.mutable.MutableList[PropMap]  // has O(c) append
  type StringList = scala.collection.mutable.MutableList[String]  // has O(c) append

  // required for json output serialization:
  implicit val formats = org.json4s.DefaultFormats

  val RUN_ID = "r1"
  val COMPONENT = "REACH"
  val ORGANIZATION = "UAZ"

  val MODIFICATION_EVENTS = Set(
    "Phosphorylation",
    "Ubiquitination",
    "Hydroxylation",
    "Sumoylation",
    "Glycosylation",
    "Acetylation",
    "Farnesylation",
    "Ribosylation",
    "Methylation",
    "Hydrolysis"
  )

  val REGULATION_EVENTS = Set(
    "Positive_regulation",
    "Negative_regulation"
  )

  val ACTIVATION_EVENTS = Set(
    "Negative_activation",
    "Positive_activation"
  )

  def isEventMention(m:Mention):Boolean = {
    m.isInstanceOf[EventMention] || m.isInstanceOf[RelationMention]
  }

  def getSentenceStartCharacterOffset(doc:Document, sentOffset:Int):Int = {
    doc.sentences(sentOffset).startOffsets.head
  }

  def getSentenceEndCharacterOffset(doc:Document, sentOffset:Int):Int = {
    doc.sentences(sentOffset).endOffsets.last
  }

  def prettifyLabel(label:String):String = label.toLowerCase.replaceAll("_", "-")

  def mkEventType(label:String):String = {
    if(MODIFICATION_EVENTS.contains(label))
      return "protein-modification"

    if(label == "Binding")
      return "complex-assembly"

    if(label == "Transcription")
      return "transcription"

    if(label == "Translocation")
      return "translocation"

    if(label == "Complex")
      return "complex-assembly"

    if(REGULATION_EVENTS.contains(label))
      return "regulation"

    if(ACTIVATION_EVENTS.contains(label))
      return "activation"

    throw new RuntimeException("ERROR: unknown event type: " + label)
  }

  def mkArgType(arg:Mention):String = {
    if (arg matches "Complex") "complex"
    else if (arg matches "Entity") "entity"
    else if (arg matches "Event") "event"
    else throw new RuntimeException("ERROR: unknown event type: " + arg.labels)
  }

  def isNegated(mention:BioMention):Boolean =
    mention.modifications.exists(isNegation)

  def isNegation(m:Modification) = m.isInstanceOf[Negation]

  def isHypothesized(mention:BioMention):Boolean =
    mention.modifications.exists(isHypothesis)

  def isHypothesis(m:Modification) = m.isInstanceOf[Hypothesis]

  def hasFeatures(m:BioMention):Boolean = {
    m.modifications.foreach(f => {
      if(f.isInstanceOf[Mutant] || f.isInstanceOf[PTM])
        return true
    })
    false
  }

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
