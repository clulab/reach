package org.clulab.reach.context
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import org.ml4ai.data.classifiers.LinearSVMWrapper
import org.ml4ai.data.utils.correctDataPrep.Utils
import org.ml4ai.data.utils.correctDataPrep.AggregatedRowNew
import org.ml4ai.data.utils.oldDataPrep.InputRow

import scala.io.Source

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
    val (allFeatures, bestFeatureSet) = featureConstructor(file)
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
      case evt:String if evt.startsWith("evtDepTail") => evtDepFeatures += evt
      case ctx:String if ctx.startsWith("ctxDepTail") => ctxDepFeatures += ctx
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

  private def aggregateFeatures(instances:Seq[(Pair, InputRow)]):(Pair, AggregatedRowNew) = throw new UnsupportedOperationException()
  private def featureConstructor(file:String):(Seq[String], Seq[String]) = {
    val allFeatures = collection.mutable.ListBuffer[String]()
    for(l <- Source.fromFile(file).getLines) {
      val contents = l.split(",")
      contents.map(allFeatures+=_)
    }
    (allFeatures, createBestFeatureSet(allFeatures))
  }

  private def createBestFeatureSet(allFeatures:Seq[String]):Seq[String] = {
    val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
    val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
    val featureDict = Utils.createFeatureDictionary(numericFeatures.toSeq)
    val bestFeatureSet = featureDict("NonDep_Context")
    bestFeatureSet
  }
}
