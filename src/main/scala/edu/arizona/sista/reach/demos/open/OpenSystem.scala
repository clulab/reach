package edu.arizona.sista.reach.demos.open

import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

import scala.util.Try

class OpenSystem(p: Option[CoreNLPProcessor] = None) {

  // Avoid costly reloading of models
  val proc: CoreNLPProcessor =
    if (p.nonEmpty) p.get
    else new CoreNLPProcessor(withDiscourse = false)

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
