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

  val ns = ctxMentions.map(c => c.nsId())
  val mut = collection.mutable.HashMap[String, Int]()
  for(s <- ns) {
    if(mut.contains(s)) {
      var fr = mut(s)
      fr += 1
      mut ++= Map(s -> fr)
    }
    else mut ++= Map(s -> 1)
  }

  for((s,f) <- mut) {
    println(s"The context label ${s} appears ${f} times")
  }

  // The constructor of this class is supplied with all the mentions and BioTextBoundMentions (i.e. context IDs)
  // Let the given Seq[BioTextBoundMention] be of size m
  // We will filter the event mentions from the mentions (let it be of size n)
  // A cross product is then generated for the context-event pairs. The resultant seq(pairs) will be of size m*n
  def yieldContextEventPairs():Seq[Pair] = {

    for(evt <- evtMentions; ctx <- ctxMentions) yield (evt, ctx)

  }


}
