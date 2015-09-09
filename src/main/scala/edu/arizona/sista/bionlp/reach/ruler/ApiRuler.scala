package edu.arizona.sista.bionlp.reach.ruler

import java.util.{ Date, List => JList, Map => JMap }
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.nxml.{FriesEntry, NxmlReader}
import edu.arizona.sista.reach.extern.export.fries.FriesOutput

class ApiRuler {

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

  // extracts raw text from given nxml and returns a list of results
  // (nxml is splitted into sections and each section is handled independently)
  def annotateNxml(nxml: String): JList[JMap[String, Any]] = {
    val maps = reader.readNxml(nxml, prefix) map annotateEntry
    maps.asJava
  }

  // annotates a single FriesEntry and returns the results in a Java Map
  def annotateEntry(entry: FriesEntry): JMap[String, Any] = {
    try {
      val startTime = new Date()
      val mentions = reach.extractFrom(entry).toList
      val endTime = new Date()
      val json = outputter.toJSON("FakePaperID", mentions, Seq(entry), startTime, endTime, prefix)
      Map("resultJson" -> json, "hasError" -> false).asJava
    } catch {
      case e: Exception => Map("resultJson" -> "", "hasError" -> true, "errorMessage" -> e.getMessage).asJava
    }
  }

  // annotates some text by converting it to a FriesEntry and calling annotateEntry()
  def annotateText(text: String, docId: String = prefix, chunkId: String = suffix): JMap[String, Any] = {
    annotateEntry(FriesEntry(docId, chunkId, "NoSection", "NoSection", false, text))
  }

}
