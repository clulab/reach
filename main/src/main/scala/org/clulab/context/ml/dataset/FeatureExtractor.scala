package org.clulab.context.ml.dataset

import org.clulab.processors.Document
import org.clulab.reach.mentions._
import ai.lum.common.Interval
import org.clulab.learning._
import scala.util.Try
import org.clulab.struct.Counter
import org.clulab.context.ContextClass
import collection.mutable

object BinnedDistance extends Enumeration{
  val SAME, CLOSE, FAR = Value
}

// Identifies the event and context id
// of an instance of PairFeatures
case class PairID(
  // Sentence # and token interval
  textBoundLocation:EventAnnotation,
  // Context's grounded id
  context:ContextType
)
{
  override def equals(o:Any):Boolean = o match {
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
  val posPath:Seq[String],
  val evtSentenceFirstPerson:Boolean,
  val ctxSentenceFirstPerson:Boolean,
  val evtSentencePastTense:Boolean,
  val ctxSentencePastTense:Boolean,
  val evtSentencePresentTense:Boolean,
  val ctxSentencePresentTense:Boolean,
  val closesCtxOfClass:Boolean
  //var contextDocFrequency:Option[Int] = None
){
  def toSeq(feda:Boolean=false):Seq[String] = {
    val features = new mutable.ArrayBuffer[String]

    features ++= Seq(
      s"sentenceDistance_${this.sentenceDistance}",
      s"contextPOSTag_${this.contextPOSTag}",
      s"eventPOSTag_${this.eventPOSTag}",
      s"dependencyDistance_${this.dependencyDistance}"
      //s"discourseDistance_${this.discourseDistance}",
      //s"dependencyPath_${this.dependencyPath.mkString("_")}",
      //s"posPath_${this.posPath.mkString("_")}",
    ) ++ this.dependencyPath.map(s => s"depBigram_$s") ++ this.posPath.map(s => s"posBigram_$s")

    if(evtSentenceFirstPerson)
      features += "evtSentenceFirstPerson"

    if(ctxSentenceFirstPerson)
      features += "ctxSentenceFirstPerson"

    if(evtSentencePastTense)
      features += "evtSentencePastTense"

    if(ctxSentencePastTense)
      features += "ctxSentencePastTense"

    if(evtSentencePresentTense)
      features += "evtSentencePresentTense"

    if(ctxSentencePresentTense)
      features += "ctxSentencePresentTense"

    if(closesCtxOfClass)
      features += "closesCtxOfClass"




    if(feda){
      for(label <- Seq("general") ++ ContextClass.values.toSeq; f <- features)
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
    events:Seq[BioMention],
    contextMentions:Seq[BioTextBoundMention]):Seq[PairFeatures] =
    for{
      event <- events;
      contextMention <- contextMentions
    } yield {
      extractFeatures(doc, event, contextMention, contextMentions map contextMention2Annotation)
    }

  def mkRVFDatum(instances:Seq[PairFeatures], contextFrequency:Int, label:String):RVFDatum[String, String] = {
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

      new RVFDatum[String, String](label, c)
  }

  def bioMention2Annotation(mention:BioMention) = mention match {
      case m:BioEventMention =>
        EventAnnotation(m.sentence, Interval.open(m.trigger.tokenInterval.start, m.trigger.tokenInterval.end))
      case m =>
        EventAnnotation(m.sentence, Interval.open(m.tokenInterval.start, m.tokenInterval.end))
  }

  def contextMention2Annotation(m:BioTextBoundMention) = ContextAnnotation(m.sentence,
     Interval.open(m.tokenInterval.start, m.tokenInterval.end),
     ContextType.parse(m))

  def extractFeatures(doc:Document, event:BioMention,
     contextMention:BioTextBoundMention, contextMentions:Iterable[ContextAnnotation]):PairFeatures = extractFeatures(doc,
        bioMention2Annotation(event),
        contextMention2Annotation(contextMention), contextMentions)

  def extractFeatures(doc:Document, event:EventAnnotation,
     contextMention:BioTextBoundMention, contextMentions:Iterable[ContextAnnotation]):PairFeatures = extractFeatures(doc,
        event, contextMention2Annotation(contextMention), contextMentions)

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

  def extractFeatures(doc:Document, event:EventAnnotation, contextMention:ContextAnnotation
                      , contextMentions:Iterable[ContextAnnotation]):PairFeatures = {

    val id = PairID(event, contextMention.contextType)

    val sentenceDistance:BinnedDistance.Value = FeatureProcessing.binSentenceDistance(Math.abs(event.sentenceId - contextMention.sentenceId))

    val dependencyPath:Option[Seq[String]] =
      if(event.sentenceId == contextMention.sentenceId){
        // Get the shortest path in the dependency graph
        val sentence = doc.sentences(event.sentenceId)
        val deps = sentence.dependencies.get

        // Ignore direction for the sake of simplicity
        val sequence = deps.shortestPathEdges(event.interval.start, contextMention.interval.start, true)(0).map(_._3).map(FeatureProcessing.clusterDependency)
        //println(sequence.mkString(" "))


        if(sequence.size == 0){
          println("DEBUG: Problem when extracting dependency path for features")
          None
        }
        else{
          // make bigrams
          val bigrams:Seq[String] = {
            if(sequence.size == 1)
              sequence
            else{
              val shifted = sequence.drop(1)
              sequence.zip(shifted).map{ case (a, b) => s"${a}_${b}" }
            }
          }

          Some(bigrams)
        }
      }
      else
        None

    val dependencyLength = dependencyPath match {
      case Some(path) => Some(FeatureProcessing.binDependencyDistance(path.size))
      case None => None
    }

    // val clusteredDependencyPath = dependencyPath match {
    //   case Some(path) => path map FeatureProcessing.clusterDependency
    //   case None => Seq()
    // }

    // TODO: Check the index out of bounds in the POS tags
    val clusteredPOSPath:Seq[String] =
      if(event.sentenceId == contextMention.sentenceId){
        val sentence = doc.sentences(event.sentenceId)
        val start = if(event.interval.start <= contextMention.interval.start) event.interval.start else contextMention.interval.start
        val end = if(event.interval.start <= contextMention.interval.start) contextMention.interval.start else event.interval.start

        val tags = sentence.tags.get

        val pos_path = (start to end).map{
            i => Try(tags(i)).getOrElse("")
        }.map(FeatureProcessing.clusterPOSTag)

        // Make bigrams
        val pos_bigrams:Seq[String] = {
          val shifted = pos_path.drop(1)
          val bigrams = pos_path zip shifted
          bigrams.map{ case (a, b) => s"${a}_${b}"}
        }

        pos_bigrams
      }
      else
        Seq()


    val eventPOS= FeatureProcessing.clusterPOSTag(Try(doc.sentences(event.sentenceId).tags.get.apply(event.interval.start)).getOrElse(""))
    val contextPOS = FeatureProcessing.clusterPOSTag(Try(doc.sentences(contextMention.sentenceId).tags.get.apply(contextMention.interval.start)).getOrElse(""))

    // TODO: Here I call the phi features
    val evtSCPRP = eventSentenceContainsPRP(doc, event)
    val ctxSCPRP = contextSentenceContainsPRP(doc, contextMention)
    val evtSPastT = eventSentenceContainsPastTense(doc, event)
    val ctxSPastT = contextSentenceContainsPastTense(doc, contextMention)
    val evtSPresentT = eventSentenceContainsPresentTense(doc, event)
    val ctxSPresentT = contextSentenceContainsPresentTense(doc, contextMention)
    val closestOfCategory = isItClosestContextOfSameCategory(event, contextMention, contextMentions)


    PairFeatures(id, sentenceDistance, contextPOS, eventPOS, dependencyLength,
      dependencyPath.getOrElse(Seq()), clusteredPOSPath,
      evtSCPRP, ctxSCPRP, evtSPastT, ctxSPastT, evtSPresentT, ctxSPresentT, closestOfCategory
    )
  }

  def extractFeaturesFromCorpus(doc:Document, eventAnnotations:Seq[EventAnnotation],
     contextAnnotations:Seq[ContextAnnotation]):Seq[PairFeatures] =
    for{
      event <- eventAnnotations;
      contextAnnotation <- contextAnnotations
    } yield {
      extractFeatures(doc, event, contextAnnotation, contextAnnotations)
    }

  /// Phi Features
  def eventSentenceContainsPRP(doc:Document, event:EventAnnotation):Boolean = sentenceContainsPRP(doc, event.sentenceId)
  def contextSentenceContainsPRP(doc:Document, context:ContextAnnotation):Boolean = sentenceContainsPRP(doc, context.sentenceId)
  def eventSentenceContainsPastTense(doc:Document, event:EventAnnotation):Boolean = sentenceContainsSimplePastTense(doc, event.sentenceId)
  def contextSentenceContainsPastTense(doc:Document, context:ContextAnnotation):Boolean = sentenceContainsSimplePastTense(doc, context.sentenceId)
  def eventSentenceContainsPresentTense(doc:Document, event:EventAnnotation):Boolean = sentenceContainsSimplePresentTense(doc, event.sentenceId)
  def contextSentenceContainsPresentTense(doc:Document, context:ContextAnnotation):Boolean = sentenceContainsSimplePresentTense(doc, context.sentenceId)


  def isItClosestContextOfSameCategory(event:EventAnnotation,
                                       context:ContextAnnotation,
                                       otherContexts:Iterable[ContextAnnotation]):Boolean = {
    val bounds = Seq(event.sentenceId, context.sentenceId)
    val (start, end) = (bounds.min, bounds.max)


    val filteredContexts = otherContexts.filter{
      c =>
        if(c.sentenceId == context.sentenceId &&  c.interval == context.interval && c.contextType == context.contextType)
          false
        else
          true
    }
    assert(filteredContexts.size == otherContexts.size -1)
    val interval = Interval.closed(start, end)

    if(interval.length >= 3){
      val contextCategory = context.contextType.contextClass.toString

      val contextClasses = filteredContexts.collect{
        case c if interval.contains(c.sentenceId) =>
          c.contextType.contextClass.toString
      }.toList.toSet

      //println(s"$contextCategory || ${contextClasses.toSet}")
      val ret = !(contextClasses.toSet.contains(contextCategory))

      ret
    }
    else
      true



  }
  ////////////////

  def sentenceContainsSimplePRP(doc:Document, ix:Int) = sentenceContainsSimpleTags(doc, ix, Set("PRP"))

  def sentenceContainsSimplePastTense(doc:Document, ix:Int) = sentenceContainsSimpleTags(doc, ix, Set("VBD", "VBN"))

  def sentenceContainsSimplePresentTense(doc:Document, ix:Int) = sentenceContainsSimpleTags(doc, ix, Set("VBG", "VBP", "VBZ"))

  def sentenceContainsSimpleTags(doc:Document, ix:Int, tags:Set[String]):Boolean = {
    val sentence = doc.sentences(ix)
    val tags = sentence.tags.get.toSet
    val evidence:Iterable[Boolean] = tags map {
      tag =>
        if(tags contains tag)
          true
        else
          false
    }

    evidence.exists(identity)
  }

  def sentenceContainsPRP(doc:Document, ix:Int):Boolean = {
    val targetWords = Set("we", "us", "our", "ours", "ourselves", "i", "me", "my", "mine", "myself")
    val sentence = doc.sentences(ix)
    val tags = sentence.tags.get
    val lemmas = sentence.lemmas.get

    val x = (tags zip lemmas) filter {
      case (tag, lemma) =>
        if(tag == "PRP" && targetWords.contains(lemma))
          true
        else
          false
    }

    !x.isEmpty
  }
  ////////////////

}
