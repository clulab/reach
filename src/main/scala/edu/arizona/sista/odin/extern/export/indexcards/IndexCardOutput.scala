package edu.arizona.sista.odin.extern.export.indexcards

import java.io.{FileWriter, BufferedWriter, PrintWriter, File}
import java.util.Date

import edu.arizona.sista.bionlp.FriesEntry
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin.{RelationMention, EventMention, Mention}
import edu.arizona.sista.odin.extern.export.JsonOutputter
import org.json4s.native.Serialization

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import edu.arizona.sista.odin.extern.export.indexcards.IndexCardOutput._

/**
 * Defines classes and methods used to build and output the index card format.
 *   Written by Mihai Surdeanu. 8/27/2015.
 */
class IndexCardOutput extends JsonOutputter {
  type PropMap = scala.collection.mutable.HashMap[String, Any]

  // required for json output serialization:
  implicit val formats = org.json4s.DefaultFormats

  /**
   * Writes the given mentions to output files in Fries JSON format.
   * Separate output files are written for sentences, entities, and events.
   * Each output file is prefixed with the given prefix string.
   */
  override def writeJSON (paperId:String,
                          allMentions:Seq[Mention],
                          paperPassages:Seq[FriesEntry],
                          startTime:Date,
                          endTime:Date,
                          outFilePrefix:String): Unit = {
    // we create a separate directory for each paper, and store each index card as a separate file
    val dir = new File(outFilePrefix)
    if(! dir.exists()) {
      if (!dir.mkdirs()) {
        throw new RuntimeException(s"ERROR: failed to create output directory $outFilePrefix!")
      }
    }

    // index cards are generated here
    val cards = mkCards(paperId, allMentions, startTime, endTime)

    // save one index card per file
    var count = 1
    for(card <- cards) {
      val outFile = new File(outFilePrefix + File.separator + mkIndexCardFileName(paperId, count))
      writeJsonToFile(card, outFile)
      count += 1
    }

  }

  def mkIndexCardFileName(paperId:String, count:Int):String = s"$paperId-$ORGANIZATION-$RUN_ID-$count"

  /** Convert the entire output data structure to JSON and write it to the given file. */
  private def writeJsonToFile (model:PropMap, outFile:File) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    Serialization.writePretty(model, out)
    out.println()                           // add final newline which serialization omits
    out.flush()
    out.close()
  }

  /** Creates index cards from all events read for this paper */
  def mkCards(paperId:String,
              allMentions:Seq[Mention],
              startTime:Date,
              endTime:Date):Iterable[PropMap] = {
    val cards = new ListBuffer[PropMap]

    //
    // this needs to be done in 2 passes:
    //   first, save recursive events
    //   then, save simple events that are not part of recursive events
    //

    // keeps just events
    val eventMentions = allMentions.filter(isEventMention)
    // println(s"Found ${eventMentions.size} events.")
    // keeps track of simple events that participate in regulations
    val simpleEventsInRegs = new mutable.HashSet[Mention]()

    // first, print all regulation events
    for(mention <- eventMentions) {
      if (REGULATION_EVENTS.contains(mention.label)) {
        val card = mkRegulationIndexCard(mention.toBioMention, simpleEventsInRegs)
        addMeta(card, paperId, startTime, endTime)
        cards += card
      }
    }

    // now, print everything else that wasn't printed already
    for(mention <- eventMentions) {
      if (! REGULATION_EVENTS.contains(mention.label) && ! simpleEventsInRegs.contains(mention)) {
        val card = mkIndexCard(mention.toBioMention)
        addMeta(card, paperId, startTime, endTime)
        cards += card
      }
    }

    cards.toList
  }

  def mkIndexCard(mention:BioMention):PropMap = {
    val f = new PropMap
    f("evidence") = mention.text

    if()
  }

  def mkRegulationIndexCard(mention:BioMention,
                            simpleEventsInRegs:mutable.HashSet[Mention]):PropMap = {
    val f = new PropMap
    f("evidence") = mention.text
    f
  }

  def addMeta(f:PropMap,
              paperId:String,
              startTime:Date,
              endTime:Date): Unit = {
    f("submitter") = COMPONENT
    f("score") = 0
    f("pmc_id") = if(paperId.startsWith("PMC")) paperId.substring(3) else paperId
    f("reader_type") = "machine"
    f("reading_started") = startTime
    f("reading_complete") = endTime
  }

  def startFrame(paperId:String, component:String):PropMap = {
    val f = new PropMap
    f("submitter") = component
    f("score") = 0
    f("pmc_id") = paperId.substring(3)
    f("reader_type") = "machine"

    f("object-type") = "frame"
    val meta = new PropMap
    meta("object-type") = "meta-info"
    meta("component") = component
    f("object-meta") = meta
    f
  }

  private def isEventMention(m:Mention):Boolean = {
    m.isInstanceOf[EventMention] || m.isInstanceOf[RelationMention]
  }

  /**
   * Returns the given mentions in the index-card JSON format, as one big string.
   * All index cards are concatenated into a single JSON string.
   */
  override def toJSON (paperId:String,
                       allMentions:Seq[Mention],
                       paperPassages:Seq[FriesEntry],
                       startTime:Date,
                       endTime:Date,
                       outFilePrefix:String): String = {
    throw new RuntimeException("Not yet supported!")
  }
}

object IndexCardOutput {
  val RUN_ID = "r1"
  val COMPONENT = "REACH"
  val ORGANIZATION = "UAZ"

  val REGULATION_EVENTS = Set(
    "Positive_regulation",
    "Negative_regulation"
  )
}
