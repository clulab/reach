package org.clulab.reach.context
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import org.ml4ai.data.classifiers.LinearSVMWrapper
import org.ml4ai.data.utils.correctDataPrep.Utils
import org.ml4ai.data.utils.correctDataPrep.AggregatedRowNew
import org.ml4ai.data.utils.oldDataPrep.InputRow

class SVMContextEngine extends ContextEngine {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String

  var paperMentions:Option[Seq[BioTextBoundMention]] = None


  val svmWrapper = new LinearSVMWrapper(null)
  val trainedSVMInstanceWrapper = svmWrapper.loadFrom("/Users/shraddha/datascience/ScalaContext/src/main/resources/svmTrainedModel.dat")
  val trainedSVMInstance = trainedSVMInstanceWrapper.classifier
  override def assign(mentions: Seq[BioMention]): Seq[BioMention] = {
    paperMentions match {
      // If we haven't run infer, don't modify the mentions
      case None => mentions
      // If we have already run infer
      case Some(ctxMentions) =>

        // Collect the event mentions
        val evtMentions = mentions collect  {
          case evt:BioEventMention => evt
        }

        // Generate all the event/ctx mention pairs
        val pairs:Seq[Pair] = for(evt <- evtMentions; ctx <- ctxMentions) yield (evt, ctx)

        // Extract features for each of the pairs
        val features:Seq[InputRow] = pairs map extractFeatures

        // Aggregate the features of all the instances of a pair
        val aggregatesFeatures:Map[EventID, (Pair, AggregatedRowNew)] =
          (pairs zip features).groupBy{
            case (pair, feats) => extractEvtId(pair._1) // Group by their EventMention
          }.mapValues(aggregateFeatures)


        // Run the classifier for each pair and store the predictions
        val predictions:Map[EventID, (Pair, Boolean)] =
          aggregatesFeatures map {
            // confirm the signature of this with Enrique.
            // should we take individual aggregatedRowNew and construct and RVFDatum out of each of them,
            // OR
            // should we take Seq[AggregatedRowNew] and then construct RVFDatum out of them?
            // the current code looks like it takes individual aggregatedRowNew, but it has the name aggregatedFeatures,
            // so I need a suggestion for which might be a cleverer approach. I think taking Seq[AggregatedRowNew] might be better,
            // in which case, the signature of the following: (evtId, (pair, aggregatedFeatures))
            // must be verified to match this.
            case (evtId, (pair, aggregatedFeatures)) =>
              // TODO Shraddha: Uncomment this when ready
              //val prediction = trainedSVMInstance.predict(...)
              val prediction = true
              evtId -> (pair, prediction)
          }

        // Loop over all the mentions to generate the context dictionary
        for(mention <- mentions) yield {
          mention match {
            // If is an event mention, it's subject to context
            case evt: BioEventMention =>
              // Get its ID
              val evtId = extractEvtId(evt)
              // fetch its predicted pairs
              val contexts = predictions(evtId)

              // TODO Enrique: Assigns the context dictionary to the event mention
              evt
            // If it's not an event mention, leave it as is
            case m: BioMention =>
              m
          }
        }
    }
  }

  // Pre-filter the context mentions
  override def infer(mentions: Seq[BioMention]): Unit = {
    paperMentions = Some(mentions collect {
      case tb:BioTextBoundMention if isContextMention(tb) => tb
    })
  }

  override def update(mentions: Seq[BioMention]): Unit = ()


  private def isContextMention(mention: BioTextBoundMention):Boolean =  ContextEngine.isContextMention(mention)

  // the following code examines the best performing set from the ml4ai package.
  // the basic logic is that if a feature exists, it should have value 1 else 0.
  // When we apply this logic to any Seq[InputRow] (refer to ml4ai.data.oldDataPrep for the code), we may get many rows having value 1 for the same feature.
  // Note that this will affect the _min, _mean and _max values for every feature for that Seq[InputRow].
  // Given that the dataset on which we will test the model here is not read from file unlike ml4ai,
  // we have to take a slight detour of using InputRow and then AggregatedRowNew, instead of using AggregatedRowNew directly, as ml4ai does.
  // please contact the authors of the ml4ai package if you experience a roadblock while using the utilities it provides.

  private def extractFeatures(datum:(BioEventMention, BioTextBoundMention)):InputRow =
  { val file="/Users/shraddha/datascience/ScalaContext/src/main/resources/allFeaturesFile.txt"
    val PMCID = datum._1.document.id match {
      case Some(c) => c
      case None => "Unknown"
    }
    val label = None
    val sentencePos = datum._1.sentence
    val evntId = extractEvtId(datum._1)
    val ctxId = ContextEngine.getContextKey(datum._2)
    val (allFeatures, bestFeatureSet) = Utils.featureConstructor(file)
    var closestContext, context_freq, evtNegTail, evtSentFirst, evtSentPast, evtSentPresent, sentDist, depDist = 0.0
    val hardCodedFeatures = Seq("closesCtxOfClass", "context_frequency",
      "evtNegationInTail", "evtSentenceFirstPerson", "evtSentencePastTense", "evtSentencePresentTense", "sentenceDistance", "dependencyDistance")
    val dependencyFeatures = allFeatures.toSet -- (hardCodedFeatures.toSet ++ Seq(""))
    closestContext = if(bestFeatureSet.contains("closesCtxOfClass")) 1.0 else 0.0
    context_freq = if(bestFeatureSet.contains("context_frequency")) 1.0 else 0.0
    evtNegTail = if(bestFeatureSet.contains("evtNegationInTail")) 1.0 else 0.0
    evtSentFirst = if(bestFeatureSet.contains("evtSentenceFirstPerson")) 1.0 else 0.0
    evtSentPast = if(bestFeatureSet.contains("evtSentencePastTense")) 1.0 else 0.0
    evtSentPresent = if(bestFeatureSet.contains("evtSentencePresentTense")) 1.0 else 0.0
    sentDist = if(bestFeatureSet.contains("sentenceDistance")) 1.0 else 0.0
    depDist = if(bestFeatureSet.contains("dependencyDistance")) 1.0 else 0.0
    val ctxDepFeatures = collection.mutable.ListBuffer[String]()
    val evtDepFeatures = collection.mutable.ListBuffer[String]()
    dependencyFeatures foreach {
      case evt:String if evt.startsWith("evtDepTail") && bestFeatureSet.contains(evt) => evtDepFeatures += evt
      case ctx:String if ctx.startsWith("ctxDepTail") && bestFeatureSet.contains(ctx)=> ctxDepFeatures += ctx
    }
    InputRow(sentencePos,
      PMCID,
      label,
      evntId,
      ctxId._2,
      closestContext,
      context_freq,
      evtNegTail,
      evtSentFirst,
      evtSentPast,
      evtSentPresent,
      sentDist,
      depDist,
      ctxDepFeatures.toSet,
      evtDepFeatures.toSet)
  }

  private def extractEvtId(evt:BioEventMention):EventID = {
    val sentIndex = evt.sentence
    val tokenIntervalStart = (evt.tokenInterval.start).toString()
    val tokenIntervalEnd = (evt.tokenInterval.end).toString()
    sentIndex+tokenIntervalStart+tokenIntervalEnd
  }

  private def aggregateFeatures(instances:Seq[(Pair, InputRow)]):(Pair, AggregatedRowNew) = {
    val idMakerPair = instances(0)._1
    val sentInd = idMakerPair._1.sentence
    val pmcid = idMakerPair._1.document.id match {
      case Some(c) => c
      case None => "Unknown"}
    val evntId = extractEvtId(idMakerPair._1)
    val ctxId = ContextEngine.getContextKey(idMakerPair._2)._2
    val label = None
    val featureSetNames = collection.mutable.ListBuffer[String]()
    val featureSetValues = collection.mutable.ListBuffer[Double]()

    instances.map(i => {
      val closesCtxOfClassSet = collection.mutable.ListBuffer[Double]()
      closesCtxOfClassSet+=i._2.closesCtxOfClass
      val closestCtxStats = Utils.createStats(closesCtxOfClassSet)
      val closestExtended = Utils.extendFeatureName("closesCtxOfClass")
      featureSetNames ++= List(closestExtended._1, closestExtended._2, closestExtended._3)
      featureSetValues ++= List(closestCtxStats._1, closestCtxStats._2, closestCtxStats._3)

      val context_frequencySet = collection.mutable.ListBuffer[Double]()
      context_frequencySet += i._2.context_frequency
      val context_frequencyStats = Utils.createStats(context_frequencySet)
      val context_frequencyended = Utils.extendFeatureName("context_frequency")
      featureSetNames ++= List(context_frequencyended._1, context_frequencyended._2, context_frequencyended._3)
      featureSetValues ++= List(context_frequencyStats._1, context_frequencyStats._2, context_frequencyStats._3)

      val evtNegationInTailSet = collection.mutable.ListBuffer[Double]()
      evtNegationInTailSet += i._2.evtNegationInTail
      val evtNegationInTailStats = Utils.createStats(evtNegationInTailSet)
      val evtNegationInTailended = Utils.extendFeatureName("evtNegationInTail")
      featureSetNames ++= List(evtNegationInTailended._1, evtNegationInTailended._2, evtNegationInTailended._3)
      featureSetValues ++= List(evtNegationInTailStats._1, evtNegationInTailStats._2, evtNegationInTailStats._3)

      val evtSentenceFirstPersonSet = collection.mutable.ListBuffer[Double]()
      evtSentenceFirstPersonSet += i._2.evtSentenceFirstPerson
      val evtSentenceFirstPersonStats = Utils.createStats(evtSentenceFirstPersonSet)
      val evtSentenceFirstPersonended = Utils.extendFeatureName("evtSentenceFirstPerson")
      featureSetNames ++= List(evtSentenceFirstPersonended._1, evtSentenceFirstPersonended._2, evtSentenceFirstPersonended._3)
      featureSetValues ++= List(evtSentenceFirstPersonStats._1, evtSentenceFirstPersonStats._2, evtSentenceFirstPersonStats._3)

      val evtSentencePastTenseSet = collection.mutable.ListBuffer[Double]()
      evtSentencePastTenseSet += i._2.evtSentencePastTense
      val evtSentencePastTenseStats = Utils.createStats(evtSentencePastTenseSet)
      val evtSentencePastTenseended = Utils.extendFeatureName("evtSentencePastTense")
      featureSetNames ++= List(evtSentencePastTenseended._1, evtSentencePastTenseended._2, evtSentencePastTenseended._3)
      featureSetValues ++= List(evtSentencePastTenseStats._1, evtSentencePastTenseStats._2, evtSentencePastTenseStats._3)


      val evtSentencePresentTenseSet = collection.mutable.ListBuffer[Double]()
      evtSentencePresentTenseSet += i._2.evtSentencePresentTense
      val evtSentencePresentTenseStats = Utils.createStats(evtSentencePresentTenseSet)
      val evtSentencePresentTenseended = Utils.extendFeatureName("evtSentencePresentTense")
      featureSetNames ++= List(evtSentencePresentTenseended._1, evtSentencePresentTenseended._2, evtSentencePresentTenseended._3)
      featureSetValues ++= List(evtSentencePresentTenseStats._1,evtSentencePresentTenseStats._2, evtSentencePresentTenseStats._3)


      val sentenceDistanceSet = collection.mutable.ListBuffer[Double]()
      sentenceDistanceSet += i._2.sentenceDistance
      val sentenceDistanceStats = Utils.createStats(sentenceDistanceSet)
      val sentenceDistanceended = Utils.extendFeatureName("sentenceDistance")
      featureSetNames ++= List(sentenceDistanceended._1, sentenceDistanceended._2, sentenceDistanceended._3)
      featureSetValues ++= List(sentenceDistanceStats._1, sentenceDistanceStats._2, sentenceDistanceStats._3)


      val dependencyDistanceSet = collection.mutable.ListBuffer[Double]()
      dependencyDistanceSet += i._2.dependencyDistance
      val dependencyDistanceStats = Utils.createStats(dependencyDistanceSet)
      val dependencyDistanceended = Utils.extendFeatureName("dependencyDistance")
      featureSetNames ++= List(dependencyDistanceended._1, dependencyDistanceended._2, dependencyDistanceended._3)
      featureSetValues ++= List(dependencyDistanceStats._1, dependencyDistanceStats._2, dependencyDistanceStats._3)
    })

    val inputRows = instances.map(i => i._2)
    for(in <- inputRows) {
      val ctxMappings = Utils.aggregateInputRowFeats(in.ctx_dependencyTails.toSeq)
      val evtMappings = Utils.aggregateInputRowFeats(in.evt_dependencyTails.toSeq)
      val finalCtxPairings = Utils.finalFeatValuePairing(ctxMappings)
      val finalEvtPairings = Utils.finalFeatValuePairing(evtMappings)

      def addToFeaturesArray(input: Seq[((String,String,String), (Double,Double,Double))]):Unit = {
        for((nameTup, valueTup) <- input) {
          val nameList = List(nameTup._1, nameTup._2, nameTup._3)
          val valueList = List(valueTup._1, valueTup._2, valueTup._3)
          featureSetNames ++= nameList
          featureSetValues ++= valueList
        }
      }
      addToFeaturesArray(finalCtxPairings)
      addToFeaturesArray(finalEvtPairings)

    }
    val newAggRow = AggregatedRowNew(sentInd, pmcid, evntId, ctxId, label, featureSetValues.toArray,featureSetNames.toArray)

    //check with Enrique to see how Pairs in a given Seq[(Pair, InputRow)] can be consolidated to a single Pair in the aggregated row
    // will take the first pair for now
    (idMakerPair, newAggRow)
  }


}
