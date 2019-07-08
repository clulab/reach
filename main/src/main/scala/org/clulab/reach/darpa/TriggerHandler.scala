package org.clulab.reach.darpa

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.struct.Interval


object TriggerHandler {

  def detectTrigger(mentions: Seq[Mention], state:State): Seq[Mention] = {
    // do something very smart to handle triggers
    // and then return the mentions

    // Iterate over the BioEventMentions
    mentions foreach {
        case event:BioEventMention =>

          val dependencies = event.sentenceObj.dependencies

          /////////////////////////////////////////////////
          // Check the outgoing edges from the trigger looking
          // for a neg label
          val outgoing = dependencies match {
            case Some(deps) => deps.outgoingEdges
            case None => Array.empty
          }

          for{
            tok <- event.tokenInterval
            out <- outgoing.lift(tok)
            (ix, label) <- out
            if label == "neg"
          }
            event.modifications += KDtrigger(new BioTextBoundMention(
              Seq("KDtrigger_trigger"),
              Interval(ix),
              sentence = event.sentence,
              document = event.document,
              keep = event.keep,
              foundBy = event.foundBy
            ))
          ///////////////////////////////////////////////////

          ///////////////////////////////////////////////////
          // Check for the presence of some negative verbs
          // in all the sentence except the tokens

          // First, extract the trigger's range from the mention
          val interval = event.trigger.tokenInterval

          //val pairs = for (lemma <- event.lemmas) yield (1, lemma)
          val pairs = event.tokenInterval zip event.lemmas.get

          val pairsL = pairs takeWhile (_._1 < interval.start)
          val pairsR = pairs dropWhile (_._1 <= interval.end)

          // Get the evidence for the existing negations to avoid duplicates
          val evidence:Set[Int] = event.modifications flatMap {
                  case mod:KDtrigger => mod.evidence.tokenInterval
                  case _ => Nil
              }

          // Check for single-token triggers

          // knockdown triggers
          for{
            (ix, lemma) <- (pairsL ++ pairsR)
            if (Seq("sirna", "silencing", "si-", "sh-", "shrna") contains lemma) && !(evidence contains ix)
          }{
              event.modifications += KDtrigger(new BioTextBoundMention(
                Seq("KDtrigger_trigger"),
                Interval(ix),
                sentence = event.sentence,
                document = event.document,
                keep = event.keep,
                foundBy = event.foundBy
              ))
            }

          // knockout triggers
          for{
            (ix, lemma) <- (pairsL ++ pairsR)
            if (Seq("knockout", "ko", "-/-") contains lemma) && !(evidence contains ix)
          }{
            event.modifications += KOtrigger(new BioTextBoundMention(
              Seq("KOtrigger_trigger"),
              Interval(ix),
              sentence = event.sentence,
              document = event.document,
              keep = event.keep,
              foundBy = event.foundBy
            ))
          }

          // dominant negative triggers
          // not sure about 'dominant-negative' -> a trigram?
          for{
            (ix, lemma) <- (pairsL ++ pairsR)
            if (Seq("dn-", "dominant-negative") contains lemma) && !(evidence contains ix)
          }{
            event.modifications += DNtrigger(new BioTextBoundMention(
              Seq("DNtrigger_trigger"),
              Interval(ix),
              sentence = event.sentence,
              document = event.document,
              keep = event.keep,
              foundBy = event.foundBy
            ))
          }

          // overexpression triggers
          for{
            (ix, lemma) <- (pairsL ++ pairsR)
            if (Seq("overexpress", "overexpression", "oe") contains lemma) && !(evidence contains ix)
          }{
            event.modifications += OEtrigger(new BioTextBoundMention(
              Seq("OEtrigger_trigger"),
              Interval(ix),
              sentence = event.sentence,
              document = event.document,
              keep = event.keep,
              foundBy = event.foundBy
            ))
          }

          def flattenTuples(left:(Int, String), right:(Int, String)) = {
            (
              (left._1, right._1),
              (left._2, right._2)
            )
          }

          // bigram triggers

          // dominant negative bigrams
          val dnVerbs = Seq(("dominant", "negative"))
          // Introduce bigrams for two-token verbs in both sides of the trigger
          for(side <- Seq(pairsL, pairsR)){
            val bigrams = (side zip side.slice(1, side.length)) map (x =>
              flattenTuples(x._1, x._2)
            )

            for{
              (interval, bigram) <- bigrams
              if (dnVerbs contains bigram) && (evidence intersect (interval._1 to interval._2+1).toSet).isEmpty
            }
              {
                event.modifications += DNtrigger(new BioTextBoundMention(
                Seq("DNtrigger_trigger"),
                Interval(interval._1, interval._2 + 1),
                sentence = event.sentence,
                document = event.document,
                keep = event.keep,
                foundBy = event.foundBy
              ))}

          }

          // chemical inhibition bigrams
          // 'chemical inhibition of' trigram?
          val chemVerbs = Seq(("chemical", "inhibition", "of"), ("inhibitor", "of"))
          // Introduce bigrams for two-token verbs in both sides of the trigger
          for(side <- Seq(pairsL, pairsR)){
            val bigrams = (side zip side.slice(1, side.length)) map (x =>
              flattenTuples(x._1, x._2)
              )

            for{
              (interval, bigram) <- bigrams
              if (chemVerbs contains bigram) && (evidence intersect (interval._1 to interval._2+1).toSet).isEmpty
            }
            {
              event.modifications += CHEMtrigger(new BioTextBoundMention(
                Seq("CHEMtrigger_trigger"),
                Interval(interval._1, interval._2 + 1),
                sentence = event.sentence,
                document = event.document,
                keep = event.keep,
                foundBy = event.foundBy
              ))}

          }
          ///////////////////////////////////////////////////
        case _ => ()
    }

    mentions
  }

  // Alter Negation modifications in-place
  def handleTriggers(ms: Seq[BioMention]): Unit = {
    ms foreach { m =>
      val (negMods, other) = m.modifications.partition(_.isInstanceOf[Negation])
      val negationModifications = negMods.map(_.asInstanceOf[Negation])

      // count the negations
      negationModifications match {
        // 0 or 1 Neg modifications means no change...
        case noChange if noChange.size <= 1 => ()
        // if we have an even number of Negations, remove them all
        case pos if pos.size % 2 == 0 =>
          m.modifications = other
        // if we have an odd number, report only the first...
        case neg if neg.size % 2 != 0 =>
          val singleNeg =
            negationModifications
              .toSeq
              .sortBy(_.evidence.tokenInterval)
              .head
          m.modifications = other + singleNeg
      }
    }
  }

}
