package org.clulab.reach.export.apis

import java.util.{Date, Map => JMap}
import com.typesafe.config.ConfigFactory
import org.clulab.odin.Mention
import org.clulab.reach._
import org.clulab.reach.export.arizona.ArizonaOutputter
import org.clulab.reach.export.cmu.CMUExporter
import org.clulab.reach.export.fries.FriesOutput
import org.clulab.reach.export.indexcards.IndexCardOutput
import org.clulab.reach.export.serial.SerialJsonOutput
import org.clulab.reach.utils.IncrementingId
import ai.lum.nxmlreader._
import scala.collection.JavaConverters._


/**
  * External interface class to accept and process text strings and NXML documents,
  * returning REACH results in either FRIES or IndexCard JSON format.
  *   Last modified: Add CMU output format.
  */
object ApiRuler {
  // a response is a heterogeneous Java Map from String to either String or Boolean
  type Response = JMap[String, Any]

  // shared counter for request numbering
  val apiRequestCntr = new IncrementingId()

  // some internal defaults for parameters required in lower layers
  private val prefix = "api"
  private val suffix = "Reach"

  // read configuration to determine processing parameters
  val config = ConfigFactory.load()
  val ignoreSections = config.getStringList("ignoreSections").asScala.toList
  val encoding = config.getString("encoding")

  val reader = new NxmlReader(ignoreSections.toSet)

  val reach = new ReachSystem               // start reach system

  val friesOutputter = new FriesOutput         // converts results to JSON in FRIES format
  val indexCardOutputter = new IndexCardOutput // converts results to JSON in Index Card format
  val serialOutputter = new SerialJsonOutput("utf-8") // converts results to serialized JSON

  /** Extracts raw text from given nxml and returns a response with all the mentions. */
  def annotateNxml(nxml: String, outFormat: String): Response = {
    val nxmlDoc = reader.parse(nxml)
    mkResponse(nxmlDoc, reach.extractFrom(nxmlDoc), outFormat)
  }

  /** Annotates some text by converting it to a FriesEntry and calling annotateEntry().
      Uses fake document ID and chunk ID. */
  def annotateText(text: String, outFormat: String): Response = {
    annotateEntry(FriesEntry(prefix, suffix, "NoSection", "NoSection", isTitle = false, text), outFormat)
  }

  /** annotates some text by converting it to a FriesEntry and calling annotateEntry(). */
  def annotateText(text: String, docId: String=prefix, chunkId: String=suffix,
                   outFormat: String="fries"): Response =
  {
    annotateEntry(FriesEntry(docId, chunkId, "NoSection", "NoSection", isTitle = false, text), outFormat)
  }


  /** Annotates a single FriesEntry and returns a response. */
  def annotateEntry(entry: FriesEntry, outFormat: String): Response =
    mkResponse(Seq(entry), reach.extractFrom(entry), outFormat)

  /** Gets a sequence of FriesEntries and their extracted mentions (by name)
      and constructs a Response. */
  def mkResponse(nxmlDoc: NxmlDocument, lazyMentions: => Seq[Mention],
                 outFormat: String): Response =
  {
    try {
      val startTime = new Date()
      val mentions = lazyMentions
      val endTime = new Date()
      val requestId = s"${prefix}${apiRequestCntr.genNextId()}"
      val resultString = outFormat.toLowerCase match {
        case "arizona" | "csv" | "tsv" =>
          ArizonaOutputter.tabularOutput(mentions)
        case "cmu" | "CMU" =>
          CMUExporter.tabularOutput(mentions)
        case "indexcard" =>
          indexCardOutputter.toJSON(requestId, mentions, nxmlDoc, startTime, endTime, prefix)
        case "serial-json" =>
          serialOutputter.toJSON(requestId, mentions, nxmlDoc, startTime, endTime, prefix)
        case _ =>
          friesOutputter.toJSON(requestId, mentions, nxmlDoc, startTime, endTime, prefix)
      }
      Map("result" -> resultString, "hasError" -> false).asJava
    } catch {
      case e: Exception => Map("result" -> "", "hasError" -> true, "errorMessage" -> e.getMessage).asJava
    }
  }

  /** Gets a sequence of FriesEntries and their extracted mentions (by name)
      and constructs a Response. */
  def mkResponse(entries: Seq[FriesEntry], lazyMentions: => Seq[Mention],
                 outFormat: String): Response =
  {
    try {
      val startTime = new Date()
      val mentions = lazyMentions
      val endTime = new Date()
      val requestId = s"${prefix}${apiRequestCntr.genNextId()}"
      val resultString = outFormat.toLowerCase match {
        case "arizona" | "csv" | "tsv" =>
          ArizonaOutputter.tabularOutput(mentions)
        case "cmu" | "CMU" =>
          CMUExporter.tabularOutput(mentions)
        case "indexcard" =>
          indexCardOutputter.toJSON(requestId, mentions, entries, startTime, endTime, prefix)
        case "serial-json" =>
          serialOutputter.toJSON(requestId, mentions, entries, startTime, endTime, prefix)
        case _ =>
          friesOutputter.toJSON(requestId, mentions, entries, startTime, endTime, prefix)
      }
      Map("result" -> resultString, "hasError" -> false).asJava
    } catch {
      case e: Exception => Map("result" -> "", "hasError" -> true, "errorMessage" -> e.getMessage).asJava
    }
  }

}
