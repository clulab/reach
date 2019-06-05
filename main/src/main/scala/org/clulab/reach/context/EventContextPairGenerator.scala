package org.clulab.reach.context

import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}

class EventContextPairGenerator(mentions:Seq[BioMention], ctxMentions:Seq[BioTextBoundMention],sentenceWindow:Option[Int] = None) {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  // Collect the event mentions
  val evtMentions = mentions collect  {
    case evt:BioEventMention => evt
  }

 /* val ctxMentions = mentions collect {
    case ctx: BioTextBoundMention => ctx
  }*/


  def yieldContextEventPairs():Seq[Pair] = {
    //val pairs = collection.mutable.ListBuffer[Pair]()

    for(evt <- evtMentions; ctx <- ctxMentions) yield (evt, ctx)

  }


}
