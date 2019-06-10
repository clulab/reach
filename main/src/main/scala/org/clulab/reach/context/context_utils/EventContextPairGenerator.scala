package org.clulab.reach.context.context_utils

import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}

class EventContextPairGenerator(mentions:Seq[BioMention], ctxMentions:Seq[BioTextBoundMention]) {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  // Collect the event mentions
  val evtMentions = mentions collect  {
    case evt:BioEventMention => evt
  }

  def yieldContextEventPairs():Seq[Pair] = {

    for(evt <- evtMentions; ctx <- ctxMentions) yield (evt, ctx)

  }


}
