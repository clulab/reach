package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.DarpaFlow

class IdentityGrounder extends DarpaFlow {
  // A NOP: returns all mentions unchanged
  def apply(mentions: Seq[Mention], state: State): Seq[Mention] = mentions
}
