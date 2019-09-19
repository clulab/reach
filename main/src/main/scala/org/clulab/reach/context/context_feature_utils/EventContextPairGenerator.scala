package org.clulab.reach.context.context_feature_utils

import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}

class EventContextPairGenerator(mentions:Seq[BioMention], ctxMentions:Seq[BioTextBoundMention]) {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  // Collect the event mentions
  val evtMentions = mentions collect  {
    case evt:BioEventMention => evt
  }
  val groundingIDs = ctxMentions.map(x => x.nsId())
  println(s"Inside pair generator for SVM. Checking if context mentions have rice in them: ${groundingIDs.contains("taxonomy:4530")}")
  println(s"Inside pair generator for SVM. Checking if context mentions have mice/mouse in them: ${groundingIDs.contains("taxonomy:10090")}")
  // The constructor of this class is supplied with all the mentions and BioTextBoundMentions (i.e. context IDs)
  // Let the given Seq[BioTextBoundMention] be of size m
  // We will filter the event mentions from the mentions (let it be of size n)
  // A cross product is then generated for the context-event pairs. The resultant seq(pairs) will be of size m*n
  def yieldContextEventPairs():Seq[Pair] = {

    for(evt <- evtMentions; ctx <- ctxMentions) yield (evt, ctx)

  }


}
