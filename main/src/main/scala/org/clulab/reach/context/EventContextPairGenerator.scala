package org.clulab.reach.context

import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}

class EventContextPairGenerator(mentions:Seq[BioMention], sentenceWindow:Option[Int] = None) {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  println(" Inside pair generator class, checking size of mentions " + mentions.size)
  // Collect the event mentions
  val evtMentions = mentions collect  {
    case evt:BioEventMention => evt
  }

  val ctxMentions = mentions collect {
    case ctx: BioTextBoundMention => ctx
  }

  val pairs = collection.mutable.ListBuffer[Pair]()

    for(evt <- evtMentions; ctx <- ctxMentions) {
      val currentPair = (evt, ctx)
      pairs += currentPair
    }
  def yieldContextEventPairs():Seq[Pair] = {
    println(pairs.size)
    /*val filteredPairs = sentenceWindow match {
      case Some(bound) =>
        pairs.filter {
          case (evt, ctx) =>
            Math.abs(evt.sentence - ctx.sentence) <= bound
        }
      case None =>
        pairs
    }
    filteredPairs*/
    pairs

  }


}
