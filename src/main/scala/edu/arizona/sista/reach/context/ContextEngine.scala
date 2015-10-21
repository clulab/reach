package edu.arizona.sista.reach.context

import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.nxml.FriesEntry

trait ContextEngine {

  /** initializes any data structure that needs to be initialized */
  def infer(
      entries: Seq[FriesEntry],
      documents: Seq[Document],
      mentionsPerEntry: Seq[Seq[BioMention]]
  ): Unit

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]): Unit

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]): Seq[BioMention]

}
