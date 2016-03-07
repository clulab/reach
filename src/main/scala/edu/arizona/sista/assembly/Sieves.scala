package edu.arizona.sista.assembly

import edu.arizona.sista.odin.Mention

class Sieves(mentions: Seq[Mention]) {

  val reachMentions = mentions

  /**
   * populates an AssemblyManager with mentions (default behavior of AssemblyManager)
   * @param mentions a sequence of Odin Mentions
   * @param manager an AssemblyManager
   * @return an AssemblyManager with representations for each relevant mention
   */
  def trackMentions(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {
    val am = AssemblyManager()
    am.trackMentions(mentions)
    am
  }
}
