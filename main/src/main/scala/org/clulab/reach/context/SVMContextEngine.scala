package org.clulab.reach.context

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, ContextPairInstance}
import org.clulab.reach.RuleReader
import org.clulab.reach.RuleReader.readResource
import org.clulab.reach.context.context_feature_utils.{ContextFeatureAggregator, ContextFeatureUtils, EventContextPairGenerator}
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}

import scala.collection.immutable
import java.io._

import scala.io.Source

class SVMContextEngine(sentenceWindow:Option[Int] = None) extends ContextEngine with LazyLogging {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)

  var paperMentions:Option[Seq[BioTextBoundMention]] = None
  var orderedContextMentions:Map[Int, Seq[BioTextBoundMention]] = _
  var defaultContexts:Option[Map[String, String]] = None

  // The LinearSVMContextClassifier class is the container of our Linear SVM model.
  // It contains the methods you will need for training your model and predicting on it.
  // USAGE: val wrapper = new LinearSVMContextClassifier(LinearSVMClassifier[Int,String], String)
  // Both parameters to the constructor are optional. You can either pass a linearSVMClassifier with customized hyper-parameters, or a string that is a path to a pre-saved model.
  // If you do pass a string, the .fit function will load the model from the path and then train it.
  // If you pass both, the customized LinearSVM model will be given higher precedence and will be used for training, rather than reading from file and training.
  // Since both parameters are optional, you need not pass either. In that case, you can load a classifier from file, using the loadFrom(path) function
  // The LinearSVMClassifier is in accordance with Scikit-learn's Liblinear classifier. Therefore, it uses .fit(dataset) to train and .predict(dataset) to test.
  // Please refer to the class LinearSVMContextClassifier for further clarifications.

  val svmWrapper = new LinearSVMContextClassifier()

  val config = ConfigFactory.load()
  val configPath = config.getString("contextEngine.params.pathToSVMModel")
  // VERY IMPORTANT TO KEEP THE STARTING / for jar file path to resource dir
  val resourcesPath = "/org/clulab/context/svmFeatures"
  val pathToSVMModel = s"${resourcesPath}/svm_model.dat"


  //val answer = getClass().getClassLoader().getResourceAsStream(pathToSVMModel)
  val answer = getClass.getResource(pathToSVMModel)
  println(answer)
  val truncatedPathToFile = answer.toString.slice(5,answer.toString.length)

  println("crossed risk of null pointer")
  val trainedSVMInstance = svmWrapper.loadFrom(truncatedPathToFile)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
    case None => {
      null
    }
  }

  if(classifierToUse == null) throw new NullPointerException("No classifier found on which I can predict. Please make sure the SVMContextEngine class receives a valid Linear SVM classifier.")


  logger.debug(s"The SVM model has been tuned to the following settings: C: ${classifierToUse.C}, Eps: ${classifierToUse.eps}, Bias: ${classifierToUse.bias}")

  override def assign(mentions: Seq[BioMention]): Seq[BioMention] = {

    paperMentions match {
      // If we haven't run infer, don't modify the mentions
      case None => mentions
      // If we have already run infer
      case Some(ctxMentions) =>

        // Generate all the event/ctx mention pairs
        val pairGenerator = new EventContextPairGenerator(mentions, ctxMentions)
        val pairs = pairGenerator.yieldContextEventPairs()
        val filteredPairs = sentenceWindow match {
          case Some(bound) =>
            pairs.filter {
              case (evt, ctx) =>
                Math.abs(evt.sentence - ctx.sentence) <= bound
            }
          case None =>
            pairs
        }

        // The filteredPairs, as the name suggests, contains the subset of the context-event pairs, filtered based on the sentence distance window.
        // A filteredPair is an instance of Pair as defined on line 15. Once we have the seq(filteredPair), we are ready to calculate the feature values.

        // To extract the feature values for a given pair, we will build an instance of ContextPairInstance, that has information about the event and context IDs, and feature values associated with that pair.
        // ContextPairInstance basically is an object that contains information about paperID, eventID, contextID, and the set of features for which we need values.
        // To associate each ContextPairInstance to its correct set of feature values, we will use a Map, where the map has ContextPairInstance as a key, and as value, we have a map of features with values
        // for each pair, our map looks like: ContextPairInstance -> (sentenceDistance -> 0.0, dependencyDistance -> 1.0), etc.
        // We can call this map the lookUpTable.

        // the line below internally calls the feature extractor on each pair and constructs the map as described.
        // For more information on feature extraction, please refer to the ContextFeatureExtractor class.
        val lookUpTable = ContextFeatureUtils.getFeatValMapPerInput(filteredPairs.toSet, ctxMentions)

        // In order to associate the correct ContextPairInstance to its values, we use the map from above to extract the keyset
        // With this set, we can simply look up the lookUpTable and extract the correct values.
        val contextPairInput:Seq[ContextPairInstance] = ContextFeatureUtils.getCtxPairInstances(lookUpTable)

        // It is now time to introduce the FeatureAggregator. The basic idea behind the FeatureAggregator is that,
        // for any given (eventID, contextID) pair, it is possible that reach detected many sentences that match the pair.
        // Multiple sentences for the same pair means multiple feature values for the same feature name.
        // In order to avoid bias of choosing one sentence over another, we will aggregate their feature values.
        // We will take the arithmetic mean, minimum, and maximum of the values.
        // We will then have an instance of AggregatedContextInstance, that has 3 times the number of features of the original ContextPairInstance,
        // since each feature has been aggregated to min, max and avg values.
        // The SVM model will then predict on this aggregated instance.
        val groupingsReadyToAggr = collection.mutable.ListBuffer[(Pair, ContextPairInstance)]()
        for((eventID, contextID) <- pairs) {
          val miniList = collection.mutable.ListBuffer[(Pair, ContextPairInstance)]()
          val contextInstancesSubSet = contextPairInput.filter(x => ContextFeatureUtils.extractEvtId(eventID) == x.EvtID)
          val contextFiltByCtxID = contextInstancesSubSet.filter(x => x.CtxID == contextID.nsId())
          for(i <- 0 until contextFiltByCtxID.size) {
            val currentPair = (eventID,contextID)
            val tupRow = contextFiltByCtxID(i)
            val tupEntry = (currentPair, tupRow)
            miniList += tupEntry
          }
          groupingsReadyToAggr ++= miniList
        }

        val aggregatedFeatures = groupingsReadyToAggr.groupBy{
          case (pair, _) => ContextFeatureUtils.extractEvtId(pair._1)
        }.mapValues{
          v =>
            v.groupBy(r => ContextEngine.getContextKey(r._1._2)).mapValues(s => {
              val seqOfInputRowsToPass = s map (_._2)
              val featureAggregatorInstance = new ContextFeatureAggregator(seqOfInputRowsToPass, lookUpTable)
              val aggRow = featureAggregatorInstance.aggregateContextFeatures()
              aggRow}).toSeq
        }


        val predictions:Map[EventID, Seq[(ContextID, Boolean)]] = {
          val map = collection.mutable.HashMap[EventID, Seq[(ContextID, Boolean)]]()
          for((k,a) <- aggregatedFeatures) {

            val x = a.map {
              // this loop finds the prediction of the SVM on a given AggregatedFeature row.
              // The prediction is based on  LinearSVMClassifier provided by Clulab/Processors,
              // which returns 1 for true and 0 for false predictions. We then convert this to the appropriate boolean values.
              case (ctxId, aggregatedFeature) =>
                val predArrayIntForm = trainedSVMInstance.predict(Seq(aggregatedFeature))


                // It may be that we may need the aggregated instances for further analyses, like testing or cross-validation.
                // Should such a need arise, you can write the aggregated instances to file by uncommenting the following line(s)
                // there are multiple signatures to this function, please refer to the definition of ContextFeatureUtils.writeAggrRowToFile for more details
                // Example usages:
                // ContextFeatureUtils.writeAggRowToFile(aggregatedFeature,k.toString, ctxId._2, parentDirToWriteAllRows) --> This signature writes the AggregatedInstance to file whose path is specified by parentDirToWriteAllRows
                // ContextFeatureUtils.writeAggRowToFile(aggregatedFeature, k.toString, ctxId._2,sentWind, whereToWriteRowBySentDist)
                // Please note that this function writes aggregated rows for each (eventID, contextID) pair. Therefore, you may have a large number of files written to your directory.
                val prediction = {
                  predArrayIntForm(0) match {
                    case 1 => true
                    case 0 => false
                    case _ => false
                  }
                }
                logger.debug(s"For the paper ${aggregatedFeature.PMCID}, event ID: ${k.toString} and context ID: ${ctxId._2}, we have prediction: ${predArrayIntForm(0)}")

                (ctxId, prediction)
            }

            val entry = Map(k -> x)
            map ++= entry

          }
          map.toMap
        }

        // Loop over all the mentions to generate the context dictionary
        for(mention <- mentions) yield {
          mention match {
            // If is an event mention, it's subject to context
            case evt: BioEventMention =>
              // Get its ID
              val evtId = ContextFeatureUtils.extractEvtId(evt)
              // fetch its predicted pairs
              val contexts = predictions.getOrElse(evtId, Seq.empty)

              // collect only those context labels that have been predicted to be true and
              // associate it to the global map of context mentions of the given paper.
              val contextMap =
                (contexts collect {
                  case (ctx, true) => ctx
                } groupBy (_._1)).mapValues(x => x.map(_._2))

              // Assign the context map to the mention
              evt.context = if(contextMap != Map.empty) Some(contextMap) else None



              // Return the modified event
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
    val contextMentions = mentions filter ContextEngine.isContextMention map (_.asInstanceOf[BioTextBoundMention])
    paperMentions = Some(contextMentions)

    // code from rule based engine
    val entries = contextMentions groupBy (m => m.sentence)
    orderedContextMentions = immutable.TreeMap(entries.toArray:_*)
    val contextCounts:Map[(String, String), Int] = contextMentions map ContextEngine.getContextKey groupBy identity mapValues (_.size)
    val defaultContexts:Map[String, String] = contextCounts.toSeq.groupBy(_._1._1)
      // Sort them in decreasing order by frequency
      .mapValues(_.map(t => (t._1._2, t._2)))
      // And pick the id with of the type with highest frequency
      .mapValues(l => l.maxBy(_._2)._1)
    this.defaultContexts = Some(defaultContexts)
  }

  override def update(mentions: Seq[BioMention]): Unit = ()




}
