package edu.arizona.sista.reach.context

import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.nxml.FriesEntry

class DummyContextEngine extends ContextEngine {
  def infer(
      entries: Seq[FriesEntry],
      documents: Seq[Document],
      mentionsPerEntry: Seq[Seq[BioMention]]
  ): Unit = ()
  def update(mentions: Seq[BioMention]): Unit = ()
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = mentions
}
