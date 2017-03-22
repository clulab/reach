package org.clulab.context.ml.dataset

import org.clulab.processors.Document
import org.clulab.reach.mentions._
import ai.lum.common.Interval
import org.clulab.learning._
import scala.util.{Try, Success, Failure}
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
  id:PairID,
  //val sentenceDistance:BinnedDistance.Value,
  sentenceDistance:Int,
  contextPOSTag:Seq[String],
  eventPOSTag:Seq[String],
  //val dependencyDistance:Option[BinnedDistance.Value],
  dependencyDistance:Option[Int],
  //val discourseDistance:BinnedDistance.Value,
  dependencyPath:Seq[String],
  posPath:Seq[String],
  evtSentenceFirstPerson:Boolean,
  ctxSentenceFirstPerson:Boolean,
  evtSentencePastTense:Boolean,
  ctxSentencePastTense:Boolean,
  evtSentencePresentTense:Boolean,
  ctxSentencePresentTense:Boolean,
  closesCtxOfClass:Boolean,
  evtDependencyTails:Seq[String],
  ctxDependencyTails:Seq[String],
  evtNegationInTails:Boolean,
  ctxNegationInTails:Boolean
  //var contextDocFrequency:Option[Int] = None
){
  def toSeq(feda:Boolean=false):Seq[String] = {
    val features = new mutable.ArrayBuffer[String]

    features ++= Seq(
      s"contextPOSTag_${this.contextPOSTag}",
      s"eventPOSTag_${this.eventPOSTag}"
      //s"discourseDistance_${this.discourseDistance}",
      //s"dependencyPath_${this.dependencyPath.mkString("_")}",
      //s"posPath_${this.posPath.mkString("_")}",
    ) ++ this.dependencyPath.map(s => s"depBigram_$s") ++ this.posPath.map(s => s"posBigram_$s")

    features ++= Seq.fill(this.dependencyDistance match {case Some(n) => n; case None => 0})("dependencyDistance")

    features ++= Seq.fill(this.sentenceDistance)("sentenceDistance")

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


    for(t <- evtDependencyTails){
      features += s"evtDepTail_$t"
    }

    for(t <- ctxDependencyTails){
      features += s"ctxDepTail_$t"
    }

    if(evtNegationInTails)
      features += "evtNegationInTail"

    if(ctxNegationInTails)
      features += "ctxNegationIntTail"



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


      // Aggregate by mean
      val numInstances = instances.size.toDouble
      val keys = c.keySet

      for(k <- keys){
        val count = c.getCount(k)
        c.setCount(k, count / numInstances)
      }
      // Add the context id counts
      val contextTypeFreq = Seq.fill(contextFrequency)("context_frequency")
      // Add the same feature multiple times according to the example
      contextTypeFreq foreach (c.incrementCount(_))


      // Aggregate by mean

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

  def intersentenceDepPath(event: EventAnnotation,
                           contextMention: ContextAnnotation,
                           doc: Document):Option[Seq[String]] = {

    def pathToRoot(nodeIx:Int, sentence:Int, deps:Document):Seq[String] = {
      val deps = doc.sentences(sentence).dependencies.get
      val roots = deps.roots.toSeq
      val paths = roots flatMap  {
        r =>
          val ps = deps.shortestPathEdges(r, nodeIx)
          ps map (sequence => sequence map (_._3))
      }

      // Take the shortest of all the possible paths from the root to the node
      paths.sortBy(p => p.size).head
    }

    // Select the shortest path from the event mention to the root of its sentence
    val evtSubPath = event.interval.map(ix => pathToRoot(ix, event.sentenceId, doc)).sortBy(_.size).head
    // Do the same for the context mention
    val ctxSubPath = contextMention.interval.map(ix => pathToRoot(ix, contextMention.sentenceId, doc)).sortBy(_.size).head
    // Get the intermediate jumps
    val jumps = Seq.fill(Math.abs(event.sentenceId - contextMention.sentenceId))("sentenceJump")

    // Put them together
    val first = if(event.sentenceId < contextMention.sentenceId) evtSubPath else ctxSubPath
    val second = if(event.sentenceId < contextMention.sentenceId) ctxSubPath else evtSubPath

    val selectedPath = (first.reverse ++ jumps ++ second).map(FeatureProcessing.clusterDependency)

    // make bigrams
    val bigrams:Seq[String] = {
      val shifted = selectedPath.drop(1)
      selectedPath.zip(shifted).map{ case (a, b) => s"${a}_${b}" }
    }

    Some(bigrams)
  }

  def extractFeatures(doc:Document, event:EventAnnotation, contextMention:ContextAnnotation
                      , contextMentions:Iterable[ContextAnnotation]):PairFeatures = {

    val id = PairID(event, contextMention.contextType)



    // FEATURES //////////////////////

    val sentenceDistance:Int = Math.abs(event.sentenceId - contextMention.sentenceId)//FeatureProcessing.binSentenceDistance(Math.abs(event.sentenceId - contextMention.sentenceId))

    val dependencyPath:Option[Seq[String]] =
      if(event.sentenceId == contextMention.sentenceId){
        // Get the shortest path in the dependency graph
        val sentence = doc.sentences(event.sentenceId)
        val deps = sentence.dependencies.get

        // Put together all the possible paths and select the shortest
        val (first, second) = if(event.interval.start <= contextMention.interval.start) (event.interval, contextMention.interval) else (contextMention.interval, event.interval)

        val paths = first flatMap {
          i:Int =>
            second flatMap  {
              j:Int =>
                val localPaths:Seq[Seq[String]] = deps.shortestPathEdges(i, j, ignoreDirection = true) map (s => s map (_._3))
                localPaths
            }
        }

        val sequence = Try(paths.filter(_.size > 0).sortBy(_.size).head.map(FeatureProcessing.clusterDependency))

        // Ignore direction for the sake of simplicity
        //val sequence = deps.shortestPathEdges(event.interval.start, contextMention.interval.start, true)(0).map(_._3).map(FeatureProcessing.clusterDependency)
        //println(sequence.mkString(" "))


        sequence match {
          case Success(s) =>
            // make bigrams
            val bigrams:Seq[String] = {
              if(s.size == 1)
                s
              else{
                val shifted = s.drop(1)
                s.zip(shifted).map{ case (a, b) => s"${a}_${b}" }
              }
            }

            Some(bigrams)
          case Failure(e) =>
            println("DEBUG: Problem when extracting dependency path for features")
            None
        }
      }
      else
        intersentenceDepPath(event, contextMention, doc)

    val dependencyLength = dependencyPath match {
      case Some(path) => Some(path.size)//Some(FeatureProcessing.binDependencyDistance(path.size))
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


    val eventPOS= {
      val tags = doc.sentences(event.sentenceId).tags.get
      val evtTags = event.interval.map(i => Try(tags(i))).collect{ case Success(t) => t}
      evtTags map FeatureProcessing.clusterPOSTag
      //FeatureProcessing.clusterPOSTag(Try(doc.sentences(event.sentenceId).tags.get.apply(event.interval.start)).getOrElse(""))
    }
    val contextPOS = {
      val tags = doc.sentences(contextMention.sentenceId).tags.get
      val ctxTags = event.interval.map(i => Try(tags(i))).collect{ case Success(t) => t}
      ctxTags map FeatureProcessing.clusterPOSTag
    }

    // TODO: Here I call the phi features
    val evtSCPRP = eventSentenceContainsPRP(doc, event)
    val ctxSCPRP = contextSentenceContainsPRP(doc, contextMention)
    val evtSPastT = eventSentenceContainsPastTense(doc, event)
    val ctxSPastT = contextSentenceContainsPastTense(doc, contextMention)
    val evtSPresentT = eventSentenceContainsPresentTense(doc, event)
    val ctxSPresentT = contextSentenceContainsPresentTense(doc, contextMention)
    val closestOfCategory = isItClosestContextOfSameCategory(event, contextMention, contextMentions)

    // Dependency tails
    val evtDependencyTails = dependencyTails(event.sentenceId,event.interval, doc)
    val ctxDependencyTails = dependencyTails(contextMention.sentenceId, contextMention.interval, doc)

    // Negation in context mention
    val ctxNegation = if(ctxDependencyTails.filter(tail => tail.contains("neg")).size > 0) true else false
    val evtNegation = if(evtDependencyTails.filter(tail => tail.contains("neg")).size > 0) true else false

    //


    PairFeatures(id, sentenceDistance, contextPOS, eventPOS, dependencyLength,
      dependencyPath.getOrElse(Seq()), clusteredPOSPath,
      evtSCPRP, ctxSCPRP, evtSPastT, ctxSPastT, evtSPresentT, ctxSPresentT, closestOfCategory,
      evtDependencyTails map (tail => tail.mkString("_")), ctxDependencyTails map (tail => tail.mkString("_")),
      evtNegation, ctxNegation
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
      val ret = !contextClasses.toSet.contains(contextCategory)

      ret
    }
    else
      true



  }
  ////////////////

  def sentenceContainsSimplePRP(doc:Document, ix:Int):Boolean = sentenceContainsSimpleTags(doc, ix, Set("PRP"))

  def sentenceContainsSimplePastTense(doc:Document, ix:Int):Boolean = sentenceContainsSimpleTags(doc, ix, Set("VBD", "VBN"))

  def sentenceContainsSimplePresentTense(doc:Document, ix:Int):Boolean = sentenceContainsSimpleTags(doc, ix, Set("VBG", "VBP", "VBZ"))

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

  def dependencyTails(sentence:Int, interval:Interval, doc:Document):Seq[Seq[String]] = {

    val deps = doc.sentences(sentence).dependencies.get

    def helper(nodeIx:Int, depth:Int, maxDepth:Int):List[List[String]] = {

      // Get all the edges connected to the current node as long as they don't incide into another token of the
      val incoming = Try(deps.getIncomingEdges(nodeIx)) match {
       case Success(edges) => edges.filter(e => !interval.contains(e._1)).toList
       case Failure(e) => Nil
      }

      val outgoing = Try(deps.getOutgoingEdges(nodeIx)) match {
        case Success(edges) => edges.filter(e => !interval.contains(e._1)).toList
        case Failure(e) => Nil
      }

      val edges = incoming ++ outgoing

      if(depth == maxDepth)
        Nil
      else{
        edges.toList flatMap {
          e =>
            val label = FeatureProcessing.clusterDependency(e._2)
            val further = helper(e._1, depth+1, maxDepth)

            further match {
              case Nil => List(List(label))
              case list:List[List[String]] => list map (l => label::l)
            }
        }
      }
    }

    helper(interval.start, 0, 2) ++ helper(interval.end, 0, 2)
  }

  // FEATURES //////////////////////

}
