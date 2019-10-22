package org.clulab.reach.darpa

import java.io

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.struct.Interval


object RegulationHandler {

  /** Keywords for each regulation type */
  val keywordKD: Seq[String] = Seq("sirna", "silencing", "si-", "sh-", "shrna", "knockdown", "knock-down")
  val keywordKO: Seq[String] = Seq("knockout", "ko", "-/-")
  val keywordDN: Seq[io.Serializable] = Seq("dn-", "dominant-negative", ("dominant", "negative")) // for dependencies
  val keywordDNuni: Seq[String] = Seq("dn-", "dominant-negative") // for unigram tokens
  val keywordDNmulti: Seq[(String, String)] = Seq(("dominant", "negative")) // for bigram tokens
  val keywordOE: Seq[String] = Seq("overexpress", "overexpression", "oe")
  val keywordCHEM: Seq[Product] = Seq(("chemical", "inhibition", "of"), ("inhibitor", "of"))

    def detectRegulations(mentions: Seq[Mention], state:State): Seq[Mention] = {
    // do something very smart to handle triggers
    // and then return the mentions

    // Iterate over the BioEventMentions
    mentions foreach {
        case event:BioEventMention =>

          val dependencies = event.sentenceObj.dependencies //all deps for the sentence
          val eventTokInterval = event.tokenInterval //token interval for the event
          //find the indices of words in the sentence that are in the event span
          for (wordIdx <- event.sentenceObj.words.indices if wordIdx >= eventTokInterval.start & wordIdx < eventTokInterval.end) {
            //based on the word index for the words inside the event span, get all outgoing deps for this word
            val allOutgoingFromWord = event.sentenceObj.dependencies.get.outgoingEdges(wordIdx)
            //for each outgoing relation
            for (outgoing <- allOutgoingFromWord) {
              //if the node at the end of the outgoing edge is one of the triggers
              if (keywordKD contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the KD modification
                event.modifications += KDtrigger(new BioTextBoundMention(
                  Seq("KDtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
              else if (keywordKO contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the KO modification
                event.modifications += KOtrigger(new BioTextBoundMention(
                  Seq("KOtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
              else if (keywordDN contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the DN modification
                event.modifications += DNtrigger(new BioTextBoundMention(
                  Seq("DNtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
              else if (keywordOE contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the OE modification
                event.modifications += OEtrigger(new BioTextBoundMention(
                  Seq("OEtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
              else if (keywordCHEM contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the CHEM modification
                event.modifications += CHEMtrigger(new BioTextBoundMention(
                  Seq("CHEMtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
            }
          }

          ///////////////////////////////////////////////////

          ///////////////////////////////////////////////////
          // Check for the presence of some regulation keywords
          // in all the sentence except the tokens

          // First, extract the trigger's range from the mention
          val interval = event.trigger.tokenInterval

          //val pairs = for (lemma <- event.lemmas) yield (1, lemma)
          val pairs = event.tokenInterval zip event.lemmas.get

          val pairsL = pairs takeWhile (_._1 < interval.start)
          val pairsR = pairs dropWhile (_._1 <= interval.end)

          // Get the evidence for the existing regulations to avoid duplicates
          val evidence:Set[Int] = event.modifications flatMap {
                  case mod:KDtrigger => mod.evidence.tokenInterval
                  case mod:KOtrigger => mod.evidence.tokenInterval
                  case mod:DNtrigger => mod.evidence.tokenInterval
                  case mod:OEtrigger => mod.evidence.tokenInterval
                  case mod:CHEMtrigger => mod.evidence.tokenInterval
                  case _ => Nil
              }


          /** Check for single-token triggers */

          // knockdown triggers
          for{
            (ix, lemma) <- pairsL ++ pairsR
            if (keywordKD contains lemma) && !(evidence contains ix)
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
            (ix, lemma) <- pairsL ++ pairsR
            if (keywordKO contains lemma) && !(evidence contains ix)
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
            (ix, lemma) <- pairsL ++ pairsR
            if (keywordDNuni contains lemma) && !(evidence contains ix)
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
            (ix, lemma) <- pairsL ++ pairsR
            if (keywordOE contains lemma) && !(evidence contains ix)
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

          /** bi/trigram triggers */

          // dominant negative bigrams
          val dnVerbs = keywordDNmulti
          // Introduce bigrams for two-token keywords in both sides of the trigger
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
          val chemVerbs = keywordCHEM
          // Introduce bigrams for two-token keywords in both sides of the trigger
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

  //
  // this is leftover from NegationHandler; not sure if we need anything here for Regulations
  //

  // Alter Regulation modifications in-place
//  def handleRegulations(ms: Seq[BioMention]): Unit = {
//    ms foreach { m =>
//      val (negMods, other) = m.modifications.partition(_.isInstanceOf[Negation])
//      val negationModifications = negMods.map(_.asInstanceOf[Negation])
//
//      // count the negations
//      negationModifications match {
//        // 0 or 1 Neg modifications means no change...
//        case noChange if noChange.size <= 1 => ()
//        // if we have an even number of Negations, remove them all
//        case pos if pos.size % 2 == 0 =>
//          m.modifications = other
//        // if we have an odd number, report only the first...
//        case neg if neg.size % 2 != 0 =>
//          val singleNeg =
//            negationModifications
//              .toSeq
//              .sortBy(_.evidence.tokenInterval)
//              .head
//          m.modifications = other + singleNeg
//      }
//    }
//  }

}
