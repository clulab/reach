package org.clulab.reach.export.apis.open

import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.processors.client.ProcessorClient

import scala.util.Try

/**
  * Create a new Open Domain system engine.
  *   Last Modified: Update for client/server package/class renames.
  */
class OpenSystem (pcc: Option[ProcessorClient] = None) {

  // Use processor client to connect to the processor server
  val client: ProcessorClient = if (pcc.nonEmpty) pcc.get else ProcessorClient.instance

  // For the demo, Ruler will provide us with our rules
  var cachedRules: String = ""
  val actions = new OpenActions
  var engine: ExtractorEngine = null

  def mkDoc (text: String): Document = {
    client.annotate(text)
  }

  def extractFrom (rules: String, doc: Document): Try[Seq[Mention]] = {
    Try {
      if (rules != cachedRules && rules.nonEmpty) {
        cachedRules = rules
        // We might encounter a rule syntax problem on compilation
        engine = ExtractorEngine(cachedRules, actions)
      }
      engine.extractFrom(doc)
    }
  }

}
