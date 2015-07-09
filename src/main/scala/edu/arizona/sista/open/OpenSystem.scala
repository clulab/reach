package edu.arizona.sista.open

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
  var demoRules: String = ""
  val actions = new OpenActions
  var engine = ExtractorEngine(allRules, actions)

  def mkDoc(text: String): Document = {
    proc.annotate(text)
  }

  /** returns string with all rules used by the system */
  def allRules: String =
    Seq(demoRules).mkString("\n\n")

  def extractFrom(rules: String, doc: Document): Try[Seq[Mention]] = {

    if (rules != demoRules && rules.nonEmpty)
      demoRules = rules

    Try {
      // We might encounter a rule syntax problem on compilation
      engine = ExtractorEngine(demoRules)
      engine.extractFrom(doc)
    }
  }

}
