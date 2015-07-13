package edu.arizona.sista.bionlp.reach.rulelearning

import scala.collection.mutable
import edu.arizona.sista.bionlp.reach.brat.{Brat, Event, TextBound, Annotation}
import edu.arizona.sista.odin.{EventMention, TextBoundMention, Mention}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Interval
import org.slf4j.LoggerFactory

/**
 * Utility functions for use with Odin
 * Created by gus on 3/27/15.
 */
object OdinUtils {

  type TokenInterval = Interval
  type CharInterval = Interval

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  // returns a sequence of tuples
  // (SentenceIndex, CharacterInterval)
  def getTokenCharOffsets(doc: Document): Seq[(Int, CharInterval)] = {
    val tokenIntervals = for (i <- doc.sentences.indices) yield {
      val sentence = doc.sentences(i)
      sentence.startOffsets zip sentence.endOffsets map {
        case (start, end) => (i, Interval(start, end))
      }
    }
    tokenIntervals.flatten
  }

  // collapses several intervals into a single one
  // that contains them all
  def totalSpan(intervals: Seq[Interval]): Interval =
    Interval(intervals.map(_.start).min, intervals.map(_.end).max)

  // turn a sequence of (SentenceIndex, CharacterInterval)
  // into a Map[SentenceIndex, Seq[CharacterInterval]]
  def getTokenCharOffsetsMap(
      tkIntervals: Seq[(Int, CharInterval)]
  ): Map[Int, Seq[CharInterval]] = {
    tkIntervals.groupBy(_._1).transform((k, v) => v.map(_._2))
  }

  /**
   * Translates Brat-style annotations to Odin-style Mentions.
   *
   * @param doc a processors-style Document
   * @param annotations sequence of brat annotations
   * @return A sequence of Mention
   */
  def getMentionsfromAnnotations(doc: Document, annotations: Seq[Annotation]): Seq[Mention] = {
    val tokenOffsetsMap = getTokenCharOffsetsMap(getTokenCharOffsets(doc))
    val sentenceOffsets = tokenOffsetsMap.map { case (i, ints) => (i, totalSpan(ints)) }
    // Should be exactly one annotation for an ID
    val annotationLUT: Map[String, Annotation] =
      annotations.groupBy(_.id).transform((k,v) => v.head)

    val mentionLUT = new mutable.HashMap[Annotation, Option[Mention]]

    // returns the sentence that contains the interval
    def getSentence(int: CharInterval): Int = {
      for ((i, m) <- sentenceOffsets if m.start <= int.start && m.end >= int.end) return i
      sys.error(s"sentence not found for $int")
    }

    def lookupMention(name: String): Option[Mention] = getMention(annotationLUT(name))

    def characterToTokenInterval(
        sentenceIndex: Int,
        charInterval: CharInterval
    ): Option[TokenInterval] = {
      // get start and end tokens
      val tokens = for {
        (int, i) <- tokenOffsetsMap(sentenceIndex).zipWithIndex
        if int overlaps charInterval
      } yield i
      try {
        val start = tokens.head
        val end = tokens.last
        Some(Interval(start, end + 1))
      } catch {
        case e: Throwable => {
          logger.debug(s"characterInterval:\t$charInterval")
          logger.debug(s"sentenceIndex:\t$sentenceIndex")
          logger.debug("sentence offsets:\n")
          logger.debug(s"${sentenceOffsets(sentenceIndex)}")
          None
        }
      }
    }

    def getMention(annotation: Annotation): Option[Mention] = mentionLUT.get(annotation) match {
      case Some(result) => result
      case None =>
        val result = annotation match {

          case tb: TextBound =>
            val parentSentence = getSentence(tb.spans.head)
            // Convert character Interval to token Interval...
            val tokInt = characterToTokenInterval(parentSentence, tb.spans.head)
            tokInt map { interval =>
              new TextBoundMention(
                label = tb.label,
                tokenInterval = interval,
                sentence = parentSentence,
                document = doc,
                keep = true,
                foundBy = tb.id
              )
            }

          case event: Event =>
            // use trigger for sentence
            val triggerId = event.trigger
            val triggerAnnotation = annotationLUT(triggerId).asInstanceOf[TextBound]
            val triggerMention = lookupMention(triggerId).get
            val parentSentence = getSentence(triggerAnnotation.spans.head)
            // get event arguments
            val eventArgs: Map[String, Seq[Mention]] = event.arguments.transform {
              (name, anns) => anns.flatMap(lookupMention)
            }

            Some(new EventMention(
              label = event.label,
              trigger = triggerMention.asInstanceOf[TextBoundMention],
              arguments = eventArgs,
              sentence = parentSentence,
              document = doc,
              keep = true,
              foundBy = event.id
            ))

          // ignore any other annotation type
          case _ => None
        }
        mentionLUT += (annotation -> result)
        result
    }

    // convert all annotations to mentions
    annotations.flatMap(getMention)
  }

  /**
   * Reads Brat standoff and output Odin-style Mentions.
   *
   * @param doc a processors-style Document
   * @param standoff a String of Brat standoff
   * @return A sequence of Odin-style Mentions
   */
  def getMentionsFromStandoff(doc: Document, standoff: String): Seq[Mention] =
    getMentionsfromAnnotations(doc, Brat.readStandOff(standoff))
}
