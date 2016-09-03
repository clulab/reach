package org.clulab.reach.context.dataset

import org.clulab.processors.Document
import org.clulab.reach.mentions._
import ai.lum.common.Interval
import org.clulab.learning._
import scala.util.Try
import org.clulab.struct.Counter

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
      s"eventPOSTag_${this.eventPOSTag}",
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

  def mkRVFDatum(instances:Seq[PairFeatures], contextFrequency:Int, label:Boolean):RVFDatum[Boolean, String] = {
      // Iterate over the instances to build a Datum instance
      val c = new Counter[String]

      for(i <- instances; f <- i.toSeq()){
        // Concatenate the feature name and its value
        c.incrementCount(f)
      }

      // Add the context id counts
      val contextTypeFreq = Seq.fill(contextFrequency)("context_frequency")
      // Add the same feature multiple times according to the example
      contextTypeFreq foreach (c.incrementCount(_))

      new RVFDatum[Boolean, String](label, c)
  }

  def eventMention2Annotation(m:BioEventMention) = EventAnnotation(m.sentence,
     Interval.open(m.trigger.tokenInterval.start, m.trigger.tokenInterval.end))

  def contextMention2Annotation(m:BioTextBoundMention) = ContextAnnotation(m.sentence,
     Interval.open(m.tokenInterval.start, m.tokenInterval.end),
     ContextType.parse(m.nsId))

  def extractFeatures(doc:Document, event:BioEventMention,
     contextMention:BioTextBoundMention):PairFeatures = extractFeatures(doc,
        eventMention2Annotation(event),
        contextMention2Annotation(contextMention))

  def extractFeatures(doc:Document, event:EventAnnotation,
     contextMention:BioTextBoundMention):PairFeatures = extractFeatures(doc,
        event, contextMention2Annotation(contextMention))

  object FeatureProcessing{
    def binSentenceDistance(d:Int):BinnedDistance.Value = {
      if(d == 0)
        BinnedDistance.SAME
      else if(d <= 13)
        BinnedDistance.CLOSE
      else
        BinnedDistance.FAR
    }

    def binDependencyDistance(d:Int):BinnedDistance.Value = {
      if(d <= 0)
        BinnedDistance.CLOSE
      else
        BinnedDistance.FAR
    }

    // def binDiscourseDistance(d:Int):BinnedDistance.Value = {
    //   // TODO: Implement this
    //   BinnedDistance.SAME
    // }

    def clusterPOSTag(tag:String):String ={

      if(tag.startsWith("NN"))
        return "NN"
      else if(tag.startsWith("VB"))
        return "VB"
      else if(Set(",", "-RRB-", ".", ":", ";", "LRB").contains(tag))
        return "BOGUS"
      else
        return tag
    }

    def clusterDependency(d:String):String = {
      if(d.startsWith("prep"))
          "prep"
        else if(d.startsWith("conj"))
            "conj"
        else if(d.endsWith("obj"))
            "obj"
        else if(d.endsWith("mod"))
            "mod"
        else if(d.contains("subj"))
            "subj"
        else
            d
    }


  }

  def extractFeatures(doc:Document, event:EventAnnotation, contextMention:ContextAnnotation):PairFeatures = {

    val id = PairID(event, contextMention.contextType)

    val sentenceDistance:BinnedDistance.Value = FeatureProcessing.binSentenceDistance(Math.abs(event.sentenceId - contextMention.sentenceId))

    val dependencyPath:Option[Seq[String]] =
      if(event.sentenceId == contextMention.sentenceId){
        // Get the shortest path in the dependency graph
        val sentence = doc.sentences(event.sentenceId)
        val deps = sentence.dependencies.get

        // Ignore direction for the sake of simplicity
        val sequence = deps.shortestPath(event.interval.start, contextMention.interval.start, true)

        if(sequence.size == 0){
          println("DEBUG: Problem when extracting dependency path for features")
          None
        }
        else{
          val edges:Seq[String] = for(i <- 1 until sequence.size)
           yield {
            val (h, t) = (sequence(i-1), sequence(i))
            val e = deps.getEdges(h, t)
            // TODO: Check this
            if(e.size > 1){
                val edge = e(0)
                println(s"DEBUG: $edge")
                edge._3
            }
            else{
                println("DEBUG: Din't find an edge that should be here")
                ""
            }
          }
          Some(edges)
        }
      }
      else
        None

    val dependencyLength = dependencyPath match {
      case Some(path) => Some(FeatureProcessing.binDependencyDistance(path.size))
      case None => None
    }

    val clusteredDependencyPath = dependencyPath match {
      case Some(path) => path map FeatureProcessing.clusterDependency
      case None => Seq()
    }

    // TODO: Check the index out of bounds in the POS tags
    val clusteredPOSPath:Seq[String] =
      if(event.sentenceId == contextMention.sentenceId){
        val sentence = doc.sentences(event.sentenceId)
        val start = if(event.interval.start <= contextMention.interval.start) event.interval.start else contextMention.interval.start
        val end = if(event.interval.start <= contextMention.interval.start) contextMention.interval.start else event.interval.start

        val tags = sentence.tags.get

        (start to end).map{
            i => Try(tags(i)).getOrElse("")
        }.map(FeatureProcessing.clusterPOSTag)
      }
      else
        Seq()


    val eventPOS= FeatureProcessing.clusterPOSTag(Try(doc.sentences(event.sentenceId).tags.get.apply(event.interval.start)).getOrElse(""))
    val contextPOS = FeatureProcessing.clusterPOSTag(Try(doc.sentences(contextMention.sentenceId).tags.get.apply(contextMention.interval.start)).getOrElse(""))

    PairFeatures(id, sentenceDistance, contextPOS, eventPOS, dependencyLength, clusteredDependencyPath, clusteredPOSPath)
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
