package org.clulab.reach.context.utils

import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.Document
import org.clulab.struct.Interval

// Internal data structures used for ML classification

/**
  * Represents a text-bound mention in a light-weight fashion
  * @param sent Sentence index w.r.t. document object
  * @param interval Token interval of the mention
  * @param text Text representation of the extraction
  * @param grounding Grounding id of the extraction
  */
case class PaperExtraction(sent:Int, interval:Interval, text:String, grounding:String)

/**
  * Represents an event mention - context mention pair
  * @param event Pair that represents the event mention
  * @param contextMention Pair instance that represents the context mention
  * @param isContext Flag that indicates whether this is a positive example. <i>Only relevant at training time</i>
  */
case class Pair(event:PaperExtraction, contextMention:PaperExtraction, isContext:Boolean = false)

/**
  * Data structure that represents the features used by the context classifier
  * @param eventSentence
  * @param eventInterval
  * @param contextSentence
  * @param contextInterval
  * @param sentenceDistance
  * @param contextFrequency
  * @param contextGrounding
  * @param isClosestContextClass
  * @param contextSentencePresentTense
  * @param eventSentencePresentTense
  * @param contextSentencePastTense
  * @param eventSentencePastTense
  * @param contextSentenceFirstPerson
  * @param eventSentenceFirstPerson
  * @param dependencyDistance
  * @param contextHasNegation
  * @param eventHasNegation
  * @param isContext Flag that indicates whether this is a positive example. <i>Only relevant at training time</i>
  */
case class PairFeatures(eventSentence:Int,
                        eventInterval:Interval,
                        contextSentence:Int,
                        contextInterval:Interval,
                        sentenceDistance:Int,
                        contextFrequency:Int,
                        contextGrounding:String,
                        isClosestContextClass:Boolean,
                        contextSentencePresentTense:Boolean,
                        eventSentencePresentTense:Boolean,
                        contextSentencePastTense:Boolean,
                        eventSentencePastTense:Boolean,
                        contextSentenceFirstPerson:Boolean,
                        eventSentenceFirstPerson:Boolean,
                        dependencyDistance:Int,
                        contextHasNegation:Boolean,
                        eventHasNegation:Boolean,
                        isContext:Boolean) { // True for the positive case during training

  override def toString: String = {
    Seq(s"$isContext",
      s"E${eventSentence}_${eventInterval.start}_${eventInterval.end}",
      s"$contextGrounding",
      s"$isClosestContextClass",
      s"$contextFrequency",
      s"$contextHasNegation",
      s"$contextSentenceFirstPerson",
      s"$contextSentencePastTense",
      s"$contextSentencePresentTense",
      s"$dependencyDistance",
      s"$eventHasNegation",
      s"$eventSentenceFirstPerson",
      s"$eventSentencePastTense",
      s"$eventSentencePresentTense",
      s"$sentenceDistance"
    ).mkString("\t")
  }
}

//object PairFeatures {
//  val headerRow: String = {
//    Seq(
//      "label",
//      "EvtID",
//      "CtxID",
//      "closesCtxOfClass",
//      "context_frequency",
//      "ctxNegationIntTail",
//      "ctxSentenceFirstPerson",
//      "ctxSentencePastTense",
//      "ctxSentencePresentTense",
//      "dependencyDistance",
//      "evtNegationInTail",
//      "evtSentenceFirstPerson",
//      "evtSentencePastTense",
//      "evtSentencePresentTense",
//      "sentenceDistance").mkString("\t")
//  }
//
//  def mkTsv(rows: Iterable[PairFeatures]): String =
//    (headerRow :: rows.map(_.toString).toList).mkString("\n")
//}n


/**
  * Extracts features for ML-based context classification
  */
object FeatureExtractor extends App with LazyLogging {


  /**
    * Extracts features from a pair instance and it's accompaining doc, counts and locations data structures
    * @param pair Represents the event mention - context mention pair
    * @param doc Doc instance with the relevant NLP information of the current document
    * @param counts Counts of the frequencies of every context mention
    * @param locations Tracking information of the locations of context mentions throughout the document
    * @return <b>PairFeatures</b> instance with the extracted features of the input pair
    */
  def extractFeaturePairs(pair: Pair, doc: Document,
                          counts: Map[String, Int],
                          locations: Map[String, Seq[Int]]): PairFeatures = {
    val event = pair.event
    val context = pair.contextMention

    // Helper functions to extract features
    def isSentencePresentTense(sent: Int, doc: Document): Boolean = {
      val sentence = doc.sentences(sent)
      val deps = sentence.dependencies.get
      val rootTags = deps.roots map sentence.tags.get

      // Return true if a root is a verb conjugated in present tense
      (rootTags contains "VB") ||
        (rootTags contains "VBP") ||
        (rootTags contains "VBZ") ||
        (rootTags contains "VBG")

    }

    def isSentencePastTense(sent: Int, doc: Document): Boolean = {
      val sentence = doc.sentences(sent)
      val deps = sentence.dependencies.get
      val rootTags = deps.roots map sentence.tags.get

      // Return true if a root is a verb conjugated in present tense
      (rootTags contains "VBD") ||
        (rootTags contains "VBN")
    }

    def isSentenceFirstPerson(sent: Int, doc: Document): Boolean = {
      val sentence = doc.sentences(sent)
      val tags = sentence.tags.get

      // Get the indices noun phrases
      val chunks = sentence.chunks.get
      val npIndices =
        chunks.zipWithIndex.collect { case (tag, ix) if tag.endsWith("-NP") => ix }

      // Get the POS tag in the noun phrases
      val npTags = npIndices map (ix => (ix, tags(ix)))

      // Find the personal pronouns and fetch their words
      val prps =
        npTags collect {
          case (ix, tag) if tag == "PRP" =>
            sentence.words(ix).toLowerCase
        }

      // If the personal pronoun is first person, return true
      (prps contains "i") || (prps contains "we") || (prps contains "us") || (prps contains "our")
    }

    def calculateDependencyDistance(ctx: PaperExtraction, evt: PaperExtraction): Int = {
      // If they appear in the same sentence, then compute the distance in dependency hops between them
      if (ctx.sent == evt.sent) {
        val deps = doc.sentences(event.sent).dependencies.get
        val allDistances =
          (for {
            i <- event.interval
            j <- ctx.interval
          }
            yield deps.shortestPath(i, j, ignoreDirection = true).size).filter(_ > 0)

        if (allDistances.nonEmpty)
          allDistances.min
        else
          doc.sentences(event.sent).size // If there is no path (I can't see why not) then return the theoretical max
      }
      // Otherwise the number of "roots" traversed plus the distance from each root to the head of the entity
      else {
        val depsEvt = doc.sentences(event.sent).dependencies.get
        val rootToEvt =
          (for {
            i <- event.interval
            j <- depsEvt.roots
          }
            yield depsEvt.shortestPath(i, j, ignoreDirection = true).size).filter(_ > 0)

        val ctxEvt = doc.sentences(context.sent).dependencies.get
        val rootToCtx =
          (for {
            i <- context.interval
            j <- ctxEvt.roots
          }
            yield ctxEvt.shortestPath(i, j, ignoreDirection = true).size).filter(_ > 0)

        val sentDistance = Math.abs(event.sent - context.sent)

        sentDistance + rootToEvt.min + rootToCtx.min
      }
    }

    def findNegationInTails(mention: PaperExtraction): Boolean = {
      val deps = doc.sentences(mention.sent).dependencies.get
      val labels =
        for {
          ix <- mention.interval
          (target, label) <- deps.getOutgoingEdges(ix)
          if !(mention.interval contains target)
        }
          yield label

      labels contains "neg"
    }

    // Here I compute the features
    val sentenceDistance = Math.abs(event.sent - context.sent)
    val contextCount = counts(context.grounding)
    val closestContextOf = {
      val minimumDistances =
        locations mapValues (sentences => sentences.map(s => Math.abs(s - event.sent)).min)
      val contextsByDistance = minimumDistances.toSeq.groupBy(_._2).mapValues(_.map(_._1).toSet)
      val shortestDistance = contextsByDistance.keys.min
      contextsByDistance(shortestDistance) contains context.grounding
    }
    val contextSentencePresentTense = isSentencePresentTense(context.sent, doc)
    val eventSentencePresentTense = isSentencePresentTense(event.sent, doc)

    val contextSentencePastTense = isSentencePastTense(context.sent, doc)
    val eventSentencePastTense = isSentencePastTense(event.sent, doc)

    val contextSentenceFirstPerson = isSentenceFirstPerson(context.sent, doc)
    val eventSentenceFirstPerson = isSentenceFirstPerson(event.sent, doc)

    val dependencyDistance = calculateDependencyDistance(context, event)

    val contextHasNegation = findNegationInTails(context)
    val eventHasNegation = findNegationInTails(event)

    PairFeatures(event.sent, event.interval, context.sent, context.interval,
      sentenceDistance, contextCount, context.grounding, closestContextOf,
      contextSentencePresentTense, eventSentencePresentTense,
      contextSentencePastTense, eventSentencePastTense, contextSentenceFirstPerson, eventSentenceFirstPerson,
      dependencyDistance, contextHasNegation, eventHasNegation,
      pair.isContext)
  }
}
