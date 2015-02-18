package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.processors.bionlp.BioNLPProcessor

/**
 * Makes sure resource are findable by BioNLPProcessor
 */
class TestResources {

}

object TestResources {
  val bioproc = {
    val proc = new BioNLPProcessor
    proc.annotate("Test sentence") // To trick BANNER into not failing...
    proc
  }

  val extractor = mkExtractor

  def mkExtractor = {
    val actions = new DarpaActions
    val rules = BasicRuler.readRules()
    new BasicRuler(rules, actions)
  }

  def summarizeError(sentence: String, label: String, assignedParty: String): String =
    s"Failed ${label} test for sentence:\n\tWe measured transcription activation in the presence of ASPP2, which is phosphorylated by Ras.\n\tResponsible: ${assignedParty}"
}
