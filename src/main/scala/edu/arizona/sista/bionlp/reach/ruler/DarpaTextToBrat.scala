package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.brat.Brat
import edu.arizona.sista.matcher.ExtractorEngine
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

/**
 * Runs the DARPA rules on some arbitrary text and saves the output in the Brat format
 * User: mihais
 * Date: 12/29/14
 */
object DarpaTextToBrat {
  def main(args:Array[String]): Unit = {
    val proc = new BioNLPProcessor()
    val doc = proc.annotate("The ABC protein phosphorylates the CDE protein.") // TODO: read from a given file
    val actions = new DarpaActions
    val entityRules = Ruler.readEntityRules
    val eventRules = Ruler.readEventRules
    val rules = entityRules + "\n\n" + eventRules
    val extractor = new ExtractorEngine[DarpaActions](rules, actions)
    val mentions = extractor.extractFrom(doc)
    val eventAnnotations = Brat.dumpStandoff(mentions, doc)

    println(eventAnnotations) // TODO: save to a file
  }
}
