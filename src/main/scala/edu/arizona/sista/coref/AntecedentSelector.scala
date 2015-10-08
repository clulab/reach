package edu.arizona.sista.coref

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.mentions.CorefMention

abstract class AntecedentSelector {
  def apply(anaphor: CorefMention, candidates: Seq[CorefMention], numToSelect: Int = 1): Seq[CorefMention]
}

/**
 * Ignoring syntactic structure, select the candidate mention(s) closest to the start of the anaphor's sentence if
 * enough exist; else select the rightmost candidate(s).
 */
class LinearSelector extends AntecedentSelector {
  def apply(anaphor: CorefMention, candidates: Seq[CorefMention], numToSelect: Int = 1): Seq[CorefMention] = {
    require(!(candidates contains anaphor))

    val sameSent = candidates.filter(x => x.sentence == anaphor.sentence).sorted[Mention].take(numToSelect)
    if (sameSent.length == numToSelect) sameSent
    else candidates.sorted[Mention].takeRight(numToSelect)
  }
}