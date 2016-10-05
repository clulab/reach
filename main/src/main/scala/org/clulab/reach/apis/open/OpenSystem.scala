package org.clulab.reach.apis.open

import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor

import scala.util.Try


class OpenSystem(p: Option[CoreNLPProcessor] = None) {

  // Avoid costly reloading of models
  val proc: CoreNLPProcessor =
    if (p.nonEmpty) p.get
    else new CoreNLPProcessor(withDiscourse = ShallowNLPProcessor.NO_DISCOURSE)

  // For the demo, Ruler will provide us with our rules
  var cachedRules: String = ""
  val actions = new OpenActions
  var engine: ExtractorEngine = null

  def mkDoc(text: String): Document = {
    proc.annotate(text)
  }

  def extractFrom(rules: String, doc: Document): Try[Seq[Mention]] = {

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
