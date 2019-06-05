package org.clulab.reach.context

import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}

class EventContextPairGenerator(contextMentions:Seq[BioTextBoundMention], sentenceWindow:Option[Int] = None) {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  // Collect the event mentions
  val evtMentions = contextMentions collect  {
    case evt:BioEventMention => evt
  }

  val pairs:Seq[Pair] = for(evt <- evtMentions; ctx <- contextMentions) yield (evt, ctx)
  def yieldContextEventPairs():Seq[Pair] = {
    val filteredPairs = sentenceWindow match {
      case Some(bound) =>
        pairs.filter {
          case (evt, ctx) =>
            Math.abs(evt.sentence - ctx.sentence) <= bound
        }
      case None =>
        pairs
    }
    filteredPairs
  }


}
