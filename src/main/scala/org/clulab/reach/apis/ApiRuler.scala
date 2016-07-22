package org.clulab.reach.apis

import java.util.{Date, Map => JMap}

import com.typesafe.config.ConfigFactory
import org.clulab.odin.Mention
import org.clulab.reach._
import org.clulab.reach.extern.export.fries.FriesOutput
import org.clulab.reach.extern.export.indexcards.IndexCardOutput
import org.clulab.reach.nxml.FriesEntry
import org.clulab.reach.extern.export.IncrementingId
import ai.lum.nxmlreader.{ NxmlReader, NxmlDocument }

import scala.collection.JavaConverters._

/**
  * External interface class to accept and process text strings and NXML documents,
  * returning REACH results in either FRIES or IndexCard JSON format.
  *   Last Modified: Add output format argument to API calls.
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
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala.toList
  val encoding = config.getString("encoding")

  val reader = new NxmlReader(ignoreSections.toSet)

  val reach = new ReachSystem               // start reach system

  val friesOutputter = new FriesOutput         // converts results to json in FRIES format
  val indexCardOutputter = new IndexCardOutput // converts results to json in Index Card format

  /** Extracts raw text from given nxml and returns a response with all the mentions. */
  def annotateNxml(nxml: String, outFormat: String): Response = {
    val nxmlDoc = reader.read(nxml)
    mkResponse(nxmlDoc, reach.extractFrom(nxmlDoc), outFormat)
  }

  /** Annotates some text by converting it to a FriesEntry and calling annotateEntry().
      Uses fake document ID and chunk ID. */
  def annotateText(text: String, outFormat: String): Response = {
    annotateEntry(FriesEntry(prefix, suffix, "NoSection", "NoSection", false, text), outFormat)
  }

  /** annotates some text by converting it to a FriesEntry and calling annotateEntry(). */
  def annotateText(text: String, docId: String=prefix, chunkId: String=suffix,
                   outFormat: String="fries"): Response =
  {
    annotateEntry(FriesEntry(docId, chunkId, "NoSection", "NoSection", false, text), outFormat)
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
      val json = if (outFormat == "indexcard")
        indexCardOutputter.toJSON(requestId, mentions, nxmlDoc.standoff, startTime, endTime, prefix)
      else
        friesOutputter.toJSON(requestId, mentions, nxmlDoc.standoff, startTime, endTime, prefix)
      Map("resultJson" -> json, "hasError" -> false).asJava
    } catch {
      case e: Exception => Map("resultJson" -> "", "hasError" -> true, "errorMessage" -> e.getMessage).asJava
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
      val json = if (outFormat == "indexcard")
        indexCardOutputter.toJSON(requestId, mentions, entries, startTime, endTime, prefix)
      else
        friesOutputter.toJSON(requestId, mentions, entries, startTime, endTime, prefix)
      Map("resultJson" -> json, "hasError" -> false).asJava
    } catch {
      case e: Exception => Map("resultJson" -> "", "hasError" -> true, "errorMessage" -> e.getMessage).asJava
    }
  }


}
