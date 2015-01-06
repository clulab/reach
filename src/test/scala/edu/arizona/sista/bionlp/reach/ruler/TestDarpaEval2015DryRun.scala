package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.matcher.ExtractorEngine
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import DarpaEvalUtils._

/**
 * Unit test for rules tailored for the DARPA evaluation; using the dryrun corpus
 * User: mihais
 * Date: 1/5/15
 */
class TestDarpaEval2015DryRun {

}

object TestDarpaEval2015DryRun {
  val proc = new BioNLPProcessor

  val extractor = mkExtractor

  def mkExtractor = {
    val actions = new DarpaActions
    val entityRules = Ruler.readEntityRules
    val eventRules = Ruler.readEventRules // reads all entity/event rules for the DARPA eval
    val rules = entityRules + "\n\n" + eventRules
    new ExtractorEngine(rules, actions)
  }
}
