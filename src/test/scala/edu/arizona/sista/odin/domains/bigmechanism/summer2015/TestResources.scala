package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.bionlp.ReachSystem

/**
 * Makes sure resource are findable by BioNLPProcessor
 */
class TestResources {

}

object TestResources {
  val reach = new ReachSystem

  def summarizeError(sentence: String, label: String, assignedParty: String): String =
    s"Failed ${label} test for sentence:\n\tWe measured transcription activation in the presence of ASPP2, which is phosphorylated by Ras.\n\tResponsible: ${assignedParty}"
}
