package edu.arizona.sista.reach.context

import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.processors.Document

class DummyContextEngine extends ContextEngine {
  def infer(mentions: Seq[BioMention]): Unit = ()
  def update(mentions: Seq[BioMention]): Unit = ()
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = mentions
}
