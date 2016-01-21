package edu.arizona.sista.coref

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.mentions.CorefMention

abstract class AntecedentSelector {
  def apply(anaphor: CorefMention, candidates: Seq[CorefMention], numToSelect: Int = 1, sentenceLimit: Int = 1): Seq[CorefMention]
}

/**
 * Ignoring syntactic structure, select the candidate mention(s) closest to the start of the anaphor's sentence if
 * enough exist; else select the rightmost candidate(s).
 */
class LinearSelector extends AntecedentSelector {
  def apply(anaphor: CorefMention, candidates: Seq[CorefMention], numToSelect: Int = 1, sentenceLimit: Int = 1): Seq[CorefMention] = {
    require(!(candidates contains anaphor), s"""The anaphor \"${anaphor.text}\" is among the candidates!"""")

    var selected: Seq[CorefMention] = Nil
    val rightMost = anaphor.sentence
    var i = rightMost

    while (i >= 0 && rightMost - i <= sentenceLimit && selected.length < numToSelect) {
      selected ++= candidates.filter(x => x.sentence == i).sorted[Mention].take(math.max(numToSelect - selected.length,0))
      i -= 1
    }

    selected
  }
}