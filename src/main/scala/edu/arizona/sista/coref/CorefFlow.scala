package edu.arizona.sista.coref

import edu.arizona.sista.reach.mentions.{CorefMention, Link}

trait CorefFlow {
  /** Gets the candidate mentions and returns the final mentions.
    *
    * @param orderedMentions current iteration's candidate mentions
    * @return same mentions with updated antecedents
    */
  def apply(orderedMentions: Seq[CorefMention]): Seq[CorefMention]

  /** Composes two instances of CorefFlow into a single CorefFlow */
  def andThen(that: CorefFlow): CorefFlow = new ComposedCorefFlow(this, that)
}

object CorefFlow {
  def apply(link: Link): CorefFlow = new CorefFlow {
    def apply(orderedMentions: Seq[CorefMention]): Seq[CorefMention] =
      link(orderedMentions)
  }
}

class ComposedCorefFlow(step1: CorefFlow, step2: CorefFlow) extends CorefFlow {
  def apply (orderedMentions: Seq[CorefMention]): Seq[CorefMention] =
    step2(step1(orderedMentions))
}
