package org.clulab.reach.context

import org.clulab.reach.mentions._
import org.clulab.processors.Document
import org.clulab.reach.nxml.FriesEntry

class DummyContextEngine extends ContextEngine {
  def infer(mentions: Seq[BioMention]): Unit = ()
  def update(mentions: Seq[BioMention]): Unit = ()
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = mentions
}
