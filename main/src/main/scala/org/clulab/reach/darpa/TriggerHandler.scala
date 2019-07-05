package org.clulab.reach.darpa

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.struct.Interval


object NegationHandler {

  def detectNegations(mentions: Seq[Mention], state:State): Seq[Mention] = {
    // do something very smart to handle negated events
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
            event.modifications += Negation(new BioTextBoundMention(
              Seq("Negation_trigger"),
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
                  case mod:Negation => mod.evidence.tokenInterval
                  case _ => Nil
              }

          // Check for single-token negative verbs
          for{
            (ix, lemma) <- (pairsL ++ pairsR)
            if (Seq("fail", "not") contains lemma) && !(evidence contains ix)
          }{
              event.modifications += Negation(new BioTextBoundMention(
                Seq("Negation_trigger"),
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

          val verbs = Seq(("play", "no"), ("play", "little"), ("is", "not"), ("be", "insufficient"))
          // Introduce bigrams for two-token verbs in both sides of the trigger
          for(side <- Seq(pairsL, pairsR)){
            val bigrams = (side zip side.slice(1, side.length)) map (x =>
              flattenTuples(x._1, x._2)
            )

            for{
              (interval, bigram) <- bigrams
              if (verbs contains bigram) && (evidence intersect (interval._1 to interval._2+1).toSet).isEmpty
            }
              {
                event.modifications += Negation(new BioTextBoundMention(
                Seq("Negation_trigger"),
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
  def handleNegations(ms: Seq[BioMention]): Unit = {
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
