package org.clulab.reach.context.dataset

import org.clulab.processors.Document
import org.clulab.reach.mentions._
import ai.lum.common.Interval

object BinnedDistance extends Enumeration{
  val SAME, CLOSE, FAR = Value
}

case class PairFeatures(
  val sentenceDistance:BinnedDistance.Value,
  val contextPOSTag:String,
  val eventPOSTag:String,
  val dependencyDistance:Option[BinnedDistance.Value],
  //val discourseDistance:BinnedDistance.Value,
  val dependencyPath:Seq[String],
  val posPath:Seq[String],
  var contextDocFrequency:Option[Int] = None
)

object FeatureExtractor{

  def extractFeatures(doc:Document,
    events:Seq[BioEventMention],
    contextMentions:Seq[BioTextBoundMention]):Seq[PairFeatures] =
    for{
      event <- events;
      contextMention <- contextMentions
    } yield {
      extractFeatures(doc, event, contextMention)
    }


  def eventMention2Annotation(m:BioEventMention) = EventAnnotation(m.sentence,
     Interval.closed(m.tokenInterval.start, m.tokenInterval.end))

  def contextMention2Annotation(m:BioTextBoundMention) = ContextAnnotation(m.sentence,
     Interval.closed(m.tokenInterval.start, m.tokenInterval.end),
     ContextType.parse(m.nsId))

  def extractFeatures(doc:Document, event:BioEventMention,
     contextMention:BioTextBoundMention):PairFeatures = extractFeatures(doc,
        eventMention2Annotation(event),
        contextMention2Annotation(contextMention))

  def extractFeatures(doc:Document, event:EventAnnotation, contextMention:ContextAnnotation):PairFeatures = {
    // TODO: Actually replace this with something meaningful
    PairFeatures(BinnedDistance.SAME, "NN", "NN", Some(BinnedDistance.FAR), Seq(), Seq())
  }

  def extractFeatures(doc:Document, eventAnnotations: => Seq[EventAnnotation],
     contextAnnotations:Seq[ContextAnnotation]):Seq[PairFeatures] =
    for{
      event <- eventAnnotations;
      contextAnnotation <- contextAnnotations
    } yield {
      extractFeatures(doc, event, contextAnnotation)
    }

}
