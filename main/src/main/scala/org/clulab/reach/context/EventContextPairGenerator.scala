package org.clulab.reach.context

import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}

class EventContextPairGenerator(contextMentions:Seq[BioTextBoundMention]) {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  // Collect the event mentions
  val evtMentions = contextMentions collect  {
    case evt:BioEventMention => evt
  }
  def yieldContextEventPairs():Seq[Pair] = {
    for(evt <- evtMentions; ctx <- contextMentions) yield (evt, ctx)
  }


}
