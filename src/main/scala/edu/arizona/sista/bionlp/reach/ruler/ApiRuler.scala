package edu.arizona.sista.bionlp.reach.ruler

import java.io.File
import java.util.{ Date, List => JList, Map => JMap }
import scala.collection.JavaConverters._
import scala.util.{ Success, Failure }
import org.apache.commons.io.FileUtils
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.bionlp._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin.extern.export.hans.HansOutput

class ApiRuler {

  // read configuration
  val config = ConfigFactory.load()
  val executable = config.getString("nxml2fries.executable")
  val txtDir = new File(config.getString("txtDir"))
  val removeCitations = config.getBoolean("nxml2fries.removeCitations")
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala.toSet
  val encoding = config.getString("encoding")

  // make nxml2fries instance
  val nxml2fries = new Nxml2Fries(executable, txtDir, removeCitations, ignoreSections, encoding)

  // start reach system
  val reach = new ReachSystem

  // temporary directory for nxml files
  val tempDir = FileUtils.getTempDirectory()

  // converts results to json
  val outputter = new HansOutput

  val prefix = "webapi"
  val suffix = "reach"

  // extracts raw text from given nxml and returns a list of results
  // (nxml is splitted into sections and each section is handled independently)
  def annotateNxml(nxml: String): JList[JMap[String, Any]] = {
    val tempFile = File.createTempFile(prefix, suffix, tempDir)
    FileUtils.writeStringToFile(tempFile, nxml, encoding)
    val maps = nxml2fries.extractEntries(tempFile) map {
      case Success(entry) => annotateEntry(entry)
      case Failure(e) => Map("resultJson" -> "", "hasError" -> true, "errorMessage" -> e.getMessage).asJava
    }
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
