package org.clulab.reach.export.apis.open

import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.reach.coserver.ProcessorCoreClient

import scala.util.Try

/**
  * Create a new Open Domain system engine.
  *   Last Modified: Redo to use processor core client.
  */
class OpenSystem (pcc: Option[ProcessorCoreClient] = None) {

  // Use processor core client to connect to the processor core server
  val client: ProcessorCoreClient =
    if (pcc.nonEmpty) pcc.get
    else new ProcessorCoreClient

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
