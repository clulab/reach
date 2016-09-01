package org.clulab.reach.context.dataset

import org.clulab.processors.Document
import org.clulab.reach.mentions._
import ai.lum.common.Interval

object BinnedDistance extends Enumeration{
  val SAME, CLOSE, FAR = Value
}

// Identifies the event and context id
// of an instance of PairFeatures
case class PairID(
  // Sentence # and token interval
  val textBoundLocation:EventAnnotation,
  // Context's grounded id
  val context:ContextType
)
{
  override def equals(o:Any) = o match {
    case that:PairID =>
      this.textBoundLocation == that.textBoundLocation && this.context == that.context
    case _ => false
  }
}

case class PairFeatures(
  val id:PairID,
  val sentenceDistance:BinnedDistance.Value,
  val contextPOSTag:String,
  val eventPOSTag:String,
  val dependencyDistance:Option[BinnedDistance.Value],
  //val discourseDistance:BinnedDistance.Value,
  val dependencyPath:Seq[String],
  val posPath:Seq[String]
  //var contextDocFrequency:Option[Int] = None
){
  def toSeq(feda:Boolean=false):Seq[String] = {
    val features = Seq(
      s"sentenceDistance_${this.sentenceDistance}",
      s"contextPOSTag_${this.contextPOSTag}",
      s"contextPOSTag_${this.eventPOSTag}",
      s"dependencyDistance_${this.dependencyDistance}",
      //s"discourseDistance_${this.discourseDistance}",
      s"dependencyPath_${this.dependencyPath.mkString("_")}",
      s"posPath_${this.posPath.mkString("_")}"
    )

    if(feda){
      for(label <- Seq("general") ++ ContextLabel.values.toSeq; f <- features)
        yield {
          s"${label}_$f"
        }
    }
    else
      features

  }
}

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

  def extractFeaturesB(doc:Document,
    events:Seq[EventAnnotation],
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

  def extractFeatures(doc:Document, event:EventAnnotation,
     contextMention:BioTextBoundMention):PairFeatures = extractFeatures(doc,
        event, contextMention2Annotation(contextMention))

  def extractFeatures(doc:Document, event:EventAnnotation, contextMention:ContextAnnotation):PairFeatures = {

    val id = PairID(event, contextMention.contextType)
    // TODO: Actually replace this with something meaningful
    PairFeatures(id, BinnedDistance.SAME, "NN", "NN", Some(BinnedDistance.FAR), Seq(), Seq())
  }

  def extractFeaturesFromCorpus(doc:Document, eventAnnotations:Seq[EventAnnotation],
     contextAnnotations:Seq[ContextAnnotation]):Seq[PairFeatures] =
    for{
      event <- eventAnnotations;
      contextAnnotation <- contextAnnotations
    } yield {
      extractFeatures(doc, event, contextAnnotation)
    }

}
