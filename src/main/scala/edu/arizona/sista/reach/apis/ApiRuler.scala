package edu.arizona.sista.reach.apis

import java.util.{Date, List => JList, Map => JMap}

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.extern.export.fries.FriesOutput
import edu.arizona.sista.reach.nxml.{FriesEntry, NxmlReader}

import scala.collection.JavaConverters._

class ApiRuler {

  // a response is a heterogeneous java Map from String to either String or Boolean
  type Response = JMap[String, Any]

  // read configuration
  val config = ConfigFactory.load()
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala.toList
  val encoding = config.getString("encoding")

  val reader = new NxmlReader(ignoreSections)

  // start reach system
  val reach = new ReachSystem

  // converts results to json
  val outputter = new FriesOutput

  val prefix = "webapi"
  val suffix = "reach"

  // extracts raw text from given nxml and returns a response with all the mentions
  def annotateNxml(nxml: String): Response = {
    val entries = reader.readNxml(nxml, prefix)
    mkResponse(entries, entries flatMap extractMentions)
  }

  // annotates a single FriesEntry and returns a response
  def annotateEntry(entry: FriesEntry): Response =
    mkResponse(Seq(entry), extractMentions(entry))

  // annotates some text by converting it to a FriesEntry and calling annotateEntry()
  def annotateText(text: String, docId: String = prefix, chunkId: String = suffix): Response = {
    annotateEntry(FriesEntry(docId, chunkId, "NoSection", "NoSection", false, text))
  }

  // extracts the mentions from a FriesEntry
  def extractMentions(entry: FriesEntry): Seq[Mention] =
    reach.extractFrom(entry).toList

  // gets a sequence of FriesEntries and their extracted mentions (by name) and constructs a Response
  def mkResponse(entries: Seq[FriesEntry], lazyMentions: => Seq[Mention]): Response = {
    try {
      val startTime = new Date()
      val mentions = lazyMentions
      val endTime = new Date()
      val json = outputter.toJSON("FakePaperID", mentions, entries, startTime, endTime, prefix)
      Map("resultJson" -> json, "hasError" -> false).asJava
    } catch {
      case e: Exception => Map("resultJson" -> "", "hasError" -> true, "errorMessage" -> e.getMessage).asJava
    }
  }

}
