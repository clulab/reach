package org.clulab.context

import org.clulab.reach.mentions._


class DummyContextEngine extends ContextEngine {
  def infer(mentions: Seq[BioMention]): Unit = ()
  def update(mentions: Seq[BioMention]): Unit = ()
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = mentions
}
