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
  var sentenceDistance:Option[Int] = None,
  var contextPOSTag:Option[Seq[String]] = None,
  var eventPOSTag:Option[Seq[String]] = None,
  //val dependencyDistance:Option[BinnedDistance.Value],
  var dependencyDistance:Option[Int] = None,
  //val discourseDistance:BinnedDistance.Value,
  var dependencyPath:Option[Seq[String]] = None,
  var posPath:Option[Seq[String]] = None,
  var evtSentenceFirstPerson:Option[Boolean] = None,
  var ctxSentenceFirstPerson:Option[Boolean] = None,
  var evtSentencePastTense:Option[Boolean] = None,
  var ctxSentencePastTense:Option[Boolean] = None,
  var evtSentencePresentTense:Option[Boolean] = None,
  var ctxSentencePresentTense:Option[Boolean] = None,
  var closesCtxOfClass:Option[Boolean] = None,
  var evtDependencyTails:Option[Seq[String]] = None,
  var ctxDependencyTails:Option[Seq[String]] = None,
  var evtNegationInTails:Option[Boolean] = None,
  var ctxNegationInTails:Option[Boolean] = None
  //var contextDocFrequency:Option[Int] = None
){
//  def toSeq(feda:Boolean=false):Seq[String] = {
//    val features = new mutable.ArrayBuffer[String]
//
//    features ++= Seq(
//      //s"contextPOSTag_${this.contextPOSTag}",
//      //s"eventPOSTag_${this.eventPOSTag}"
//      //s"discourseDistance_${this.discourseDistance}",
//      //s"dependencyPath_${this.dependencyPath.mkString("_")}",
//      //s"posPath_${this.posPath.mkString("_")}",
//    ) ++ this.dependencyPath.map(s => s"depBigram_$s") ++ this.posPath.map(s => s"posBigram_$s")
//
//    features ++= contextPOSTag map (cp => s"contextPOSTag_$cp")
//    features ++= eventPOSTag map (ep => s"eventPOSTag_$ep")
//    features ++= Seq.fill(this.dependencyDistance match {case Some(n) => n; case None => 0})("dependencyDistance")
//
//    features ++= Seq.fill(this.sentenceDistance)("sentenceDistance")
//
//    if(evtSentenceFirstPerson)
//      features += "evtSentenceFirstPerson"
//
//    if(ctxSentenceFirstPerson)
//      features += "ctxSentenceFirstPerson"
//
//    if(evtSentencePastTense)
//      features += "evtSentencePastTense"
//
//    if(ctxSentencePastTense)
//      features += "ctxSentencePastTense"
//
//    if(evtSentencePresentTense)
//      features += "evtSentencePresentTense"
//
//    if(ctxSentencePresentTense)
//      features += "ctxSentencePresentTense"
//
//    if(closesCtxOfClass)
//      features += "closesCtxOfClass"
//
//
//    for(t <- evtDependencyTails){
//      features += s"evtDepTail_$t"
//    }
//
//    for(t <- ctxDependencyTails){
//      features += s"ctxDepTail_$t"
//    }
//
//    if(evtNegationInTails)
//      features += "evtNegationInTail"
//
//    if(ctxNegationInTails)
//      features += "ctxNegationIntTail"
//
//
//
//    if(feda){
//      for(label <- Seq("general") ++ ContextClass.values.toSeq; f <- features)
//        yield {
//          s"${label}_$f"
//        }
//    }
//    else
//      features
//
//  }

  def toMap():Map[String, Double] = {
    val featureMap = new mutable.HashMap[String, Double]()

    // Binary features (Categorical mapped to binary)
    evtSentenceFirstPerson match {
      case Some(v) =>
        if(v)
          featureMap += ("evtSentenceFirstPerson" -> 1)
      case None => Unit
    }


    ctxSentenceFirstPerson match {
      case Some(v) =>
        if(v)
          featureMap += ("ctxSentenceFirstPerson" -> 1)
      case None => Unit
    }


    evtSentencePastTense match {
      case Some(v) =>
        if(v)
          featureMap += ("evtSentencePastTense" -> 1)
      case None => Unit
    }


    ctxSentencePastTense match {
      case Some(v) =>
        if(v)
          featureMap += ("ctxSentencePastTense" -> 1)
      case None => Unit
    }

    evtSentencePresentTense match {
      case Some(v) =>
        if(v)
          featureMap += ("evtSentencePresentTense" -> 1)
      case None => Unit
    }

    ctxSentencePresentTense match {
      case Some(v) =>
        if(v)
          featureMap += ("ctxSentencePresentTense" -> 1)
      case None => Unit
    }


    closesCtxOfClass match {
      case Some(v) =>
        if(v)
          if(v)
            featureMap += ("closesCtxOfClass" -> 1)
      case None => Unit
    }

    evtNegationInTails match {
      case Some(v) =>
        if(v)
          featureMap += ("evtNegationInTail" -> 1)
      case None => Unit
    }

    ctxNegationInTails match {
      case Some(v) =>
        if(v)
          featureMap += ("ctxNegationIntTail" -> 1)
      case None => Unit
    }




    // Integer features
//    featureMap ++= this.dependencyPath.map(s => s"depBigram_$s").groupBy(identity).mapValues(_.length)
//    featureMap ++= this.posPath.map(s => s"posBigram_$s").groupBy(identity).mapValues(_.length)
//    featureMap ++= contextPOSTag.map(cp => s"contextPOSTag_$cp").groupBy(identity).mapValues(_.length)
//    featureMap ++= eventPOSTag.map(ep => s"eventPOSTag_$ep").groupBy(identity).mapValues(_.length)

    val dependencyDistance:Double = this.dependencyDistance match {case Some(n:Int) => n.toDouble; case None => 0.0}
    featureMap += ("dependencyDistance" ->dependencyDistance)

    val sentenceDistance:Double = this.sentenceDistance match {case Some(n) => n.toDouble; case None => 0.0}
    featureMap += ("sentenceDistance" -> sentenceDistance)

    evtDependencyTails match {
      case Some(tails) =>
        featureMap ++= tails.map(t => s"evtDepTail_$t").groupBy(identity).mapValues(_.length)
      case None => Unit
    }


    ctxDependencyTails match {
      case Some(tails) =>
        featureMap ++= tails.map(t => s"ctxDepTail_$t").groupBy(identity).mapValues(_.length)
      case None => Unit
    }


    // Real features


    featureMap.toMap
  }
}

object FeatureExtractor{

  def extractFeatures(doc:Document,
    events:Seq[BioMention],
    contextMentions:Seq[BioTextBoundMention],
    featureFamilies:Set[FeatureFamily]):Seq[PairFeatures] =

    for{
      event <- events;
      contextMention <- contextMentions
    } yield {
      extractFeatures(doc, event, contextMention, contextMentions map contextMention2Annotation, featureFamilies) //TODO: Add the feature families correctly
    }

//  def mkRVFDatum(instances:Seq[PairFeatures], contextFrequency:Int, label:String):RVFDatum[String, String] = {
//      // Iterate over the instances to build a Datum instance
//      val c = new Counter[String]
//
//      for(i <- instances; f <- i.toSeq()){
//        // Concatenate the feature name and its value
//        c.incrementCount(f)
//      }
//
//
//      // Aggregate by mean
//      val numInstances = instances.size.toDouble
//      val keys = c.keySet
//
//      for(k <- keys){
//        val count = c.getCount(k)
//        c.setCount(k, count / numInstances)
//      }
//      // Add the context id counts
//      val contextTypeFreq = Seq.fill(contextFrequency)("context_frequency")
//      // Add the same feature multiple times according to the example
//      contextTypeFreq foreach (c.incrementCount(_))
//
//
//      // Aggregate by mean
//
//      new RVFDatum[String, String](label, c)
//  }

  def computeQuantiles(key: String, values: Seq[Double], bins: Int):Seq[(String, Double)] = {
    val cutPoints = (1 to bins).map(i => values.max/bins * i)

    val quantiles = values.groupBy{
      v =>
        var bin = 0
        val range = (0 until bins).reverse
        for(i <- range){
          if(v <= cutPoints(i))
            bin = i
        }

        bin
    }.mapValues(_.length)

    quantiles.map{
      case (q, v) =>
        (s"quant$q-$key", v.toDouble)
    }.toSeq
  }

  def mkRVFDatum(instances:Seq[PairFeatures], contextFrequency:Int, label:String):RVFDatum[String, String] = {
    val c = new Counter[String]
    val maps = instances.map(_.toMap)
    val allKeys = maps.flatMap(_.keys).toSet

    val aggregationType = Map(
      "dependencyDistance" -> Seq("mean", "quantiles", "min", "max"),
      "sentenceDistance" -> Seq("mean", "quantiles", "min", "max")
    )

    for(key <- allKeys){
      val values = maps.map(_.lift(key) match {case Some(v) => v; case None => 0.0})

      // The distance features
      if(key == "sentenceDistance" || key == "dependencyDistance"){
        c.setCount(s"mean_$key", values.sum/values.length)
        c.setCount(s"min_$key", values.min)
        c.setCount(s"max_$key", values.max)
        val quantiles = computeQuantiles(key, values, 4)
        for((k, v) <- quantiles){
          c.setCount(k, v)
        }
      }
      else if(key.startsWith("depBigram") || key.startsWith("posBigram") || key.startsWith("evtDepTail") || key.startsWith("ctxDepTail")){
        c.setCount(s"mean_$key", values.sum/values.length)
        c.setCount(s"min_$key", values.min)
        c.setCount(s"max_$key", values.max)
      }
      // Default aggregation: mean
      else{
        c.setCount(s"mean_$key", values.sum/values.length)
      }
    }

    // Add the ctx frequency val
    c.setCount("context_frequency", contextFrequency)

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
     contextMention:BioTextBoundMention, contextMentions:Iterable[ContextAnnotation], featureFamilies:Set[FeatureFamily]):PairFeatures = extractFeatures(doc,
        bioMention2Annotation(event),
        contextMention2Annotation(contextMention), contextMentions, featureFamilies)

  def extractFeatures(doc:Document, event:EventAnnotation,
     contextMention:BioTextBoundMention, contextMentions:Iterable[ContextAnnotation], featureFamilies:Set[FeatureFamily]):PairFeatures = extractFeatures(doc,
        event, contextMention2Annotation(contextMention), contextMentions, featureFamilies)

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
      else if(Set(",", "-RRB-", ".", ":", ";", "-LRB-").contains(tag))
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
                      , contextMentions:Iterable[ContextAnnotation], featureFamilies:Set[FeatureFamily]):PairFeatures = {

    //id connects event and context mention and is used as the id for the returned PairFeatures object
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
        }.map(FeatureProcessing.clusterPOSTag).filter(_ != "BOGUS")

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
      evtTags map FeatureProcessing.clusterPOSTag filter (_ != "BOGUS")
      //FeatureProcessing.clusterPOSTag(Try(doc.sentences(event.sentenceId).tags.get.apply(event.interval.start)).getOrElse(""))
    }
    val contextPOS = {
      val tags = doc.sentences(contextMention.sentenceId).tags.get
      val ctxTags = event.interval.map(i => Try(tags(i))).collect{ case Success(t) => t}
      ctxTags map FeatureProcessing.clusterPOSTag filter (_ != "BOGUS")
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

    // Instantiate new instance of PairFeatures with id and then select features to be used in classifier:
    val features = new PairFeatures(id)

    // Select feature families to be used in classifier
    for{ family <- featureFamilies }{
      family match {
        case _:Positional => {
          features.sentenceDistance = Some(sentenceDistance)
          features.dependencyDistance = dependencyLength
          features.closesCtxOfClass = Some(closestOfCategory)
        }
        case _:Dependency => {
          features.dependencyPath = dependencyPath

        }
        case _:Phi => {
          features.evtSentenceFirstPerson = Some(evtSCPRP)
          features.ctxSentenceFirstPerson = Some(ctxSCPRP)
          features.evtSentencePastTense = Some(evtSPastT)
          features.ctxSentencePastTense = Some(ctxSPastT)
          features.evtSentencePresentTense = Some(evtSPresentT)
          features.ctxSentencePresentTense = Some(ctxSPresentT)
        }
        case _:NegationProperty => {
          features.evtNegationInTails = Some(evtNegation)
          features.ctxNegationInTails = Some(ctxNegation)
        }
        case _:Tails => {
          features.evtDependencyTails = Some(evtDependencyTails map (tail => tail.mkString("_")))
          features.ctxDependencyTails = Some(ctxDependencyTails map (tail => tail.mkString("_")))
        }
        case _:POS => {
          features.contextPOSTag = Some(contextPOS)
          features.eventPOSTag = Some(eventPOS)
          features.posPath = Some(clusteredPOSPath)
        }
      }
    }

    // return the selected features:
    features
    // or return all features:
    /*
    PairFeatures(id,
      sentenceDistance = Some(sentenceDistance),
      contextPOSTag = Some(contextPOS),
      eventPOSTag = Some(eventPOS),
      dependencyDistance = dependencyLength,
      dependencyPath = Some(dependencyPath.getOrElse(Seq())),
      posPath = Some(clusteredPOSPath),
      evtSentenceFirstPerson = Some(evtSCPRP),
      ctxSentenceFirstPerson = Some(ctxSCPRP),
      evtSentencePastTense = Some(evtSPastT),
      ctxSentencePastTense = Some(ctxSPastT),
      evtSentencePresentTense = Some(evtSPresentT),
      ctxSentencePresentTense = Some(ctxSPresentT),
      closesCtxOfClass = Some(closestOfCategory),
      evtDependencyTails = Some(evtDependencyTails map (tail => tail.mkString("_"))),
      ctxDependencyTails = Some(ctxDependencyTails map (tail => tail.mkString("_"))),
      evtNegationInTails = Some(evtNegation),
      ctxNegationInTails = Some(ctxNegation)
    )
    */
  }

  def extractFeaturesFromCorpus(doc:Document, eventAnnotations:Seq[EventAnnotation],
     contextAnnotations:Seq[ContextAnnotation], featureFamilies:Set[FeatureFamily]):Seq[PairFeatures] =
    for{
      event <- eventAnnotations;
      contextAnnotation <- contextAnnotations
    } yield {
      extractFeatures(doc, event, contextAnnotation, contextAnnotations, featureFamilies)
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

object FeatureUtils{
  def featureSelection(dataset:RVFDataset[String, String]):RVFDataset[String, String] = {
    //dataset.removeFeaturesByFrequency(20).asInstanceOf[RVFDataset[String, String]]
    //dataset.removeFeaturesByInformationGain(.01).asInstanceOf[RVFDataset[String, String]]
    dataset
  }
}
