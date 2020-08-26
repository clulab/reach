package org.clulab.reach.assembly

// import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
// import ai.lum.common.ConfigUtils._

import org.clulab.serialization.json.JSONSerializer
import org.json4s.jackson.JsonMethods._

import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.serialization.json._
import org.clulab.reach.{ PaperReader, ReachSystem }
import org.clulab.reach.mentions._


object TestUtils extends LazyLogging {

  logger.debug("loading ...")
  // to set a custom conf file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
  // val config = ConfigFactory.load()

  lazy val reachSystem: ReachSystem = PaperReader.reachSystem

  def getMentionsFromDocument(doc: Document): Seq[Mention] = reachSystem.extractFrom(doc)

  def jsonStringToDocument(jsonstr: String): Document = JSONSerializer.toDocument(parse(jsonstr))

  /**
    * Utility to produce org.clulab.Document JSON using the REACH pipeline.
    *
    * @param text
    * @param id
    * @return org.clulab.Document JSON as String
    */
  def mkDocJson(text: String, id: String = "text"): String = {
    val doc = reachSystem.mkDoc(text = text, docId = "text", chunkId = "")
    doc.json(pretty = false)
  }
}