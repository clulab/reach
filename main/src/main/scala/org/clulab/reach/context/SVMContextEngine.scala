package org.clulab.reach.context


import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import org.ml4ai.data.classifiers.LinearSVMWrapper
import org.ml4ai.data.utils.{AggregatedRow, InputRow, CodeUtils}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

// This script currently tests papers in the old data
import scala.collection.immutable
class SVMContextEngine extends ContextEngine with LazyLogging {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)

  var paperMentions:Option[Seq[BioTextBoundMention]] = None
  var orderedContextMentions:Map[Int, Seq[BioTextBoundMention]] = _
  var defaultContexts:Option[Map[String, String]] = None

  val svmWrapper = new LinearSVMWrapper(null)
  val config = ConfigFactory.load()
  val configPath = config.getString("contextEngine.params.svmPath")
  val untrainedConfigPath = config.getString("contextEngine.params.untrainedSVMPath")
  val configFeaturesFrequencyPath = config.getString("contextEngine.params.bestFeatureFrequency")
  val configAllFeaturesPath = config.getString("contextEngine.params.allFeatures")
  val groupedFeaturesPath = config.getString("contextEngine.params.groupedFeatures")

  val hardCodedFeaturesPath = config.getString("contextEngine.params.hardCodedFeatures")
  val (_,oldDataSet) = CodeUtils.loadAggregatedRowsFromFile(groupedFeaturesPath, hardCodedFeaturesPath)
  val hardCodedFeatures = CodeUtils.readHardcodedFeaturesFromFile(hardCodedFeaturesPath)
  val numericFeaturesInputRow = hardCodedFeatures.drop(4)
  val (allFeatures, bestFeatureDict) = CodeUtils.featureConstructor(configAllFeaturesPath)
  val featSeq = bestFeatureDict("NonDep_Context")
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)


  logger.info(s"The SVM model has been tuned to the following settings: C: ${trainedSVMInstance.classifier.C}, Eps: ${trainedSVMInstance.classifier.eps}, Bias: ${trainedSVMInstance.classifier.bias}")

  override def assign(mentions: Seq[BioMention]): Seq[BioMention] = {

    logger.info("assigning respective mentions in SVMContextEngine")
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
        val aggregatedFeatures:Map[EventID, Seq[(ContextID, AggregatedRow)]] =
          (pairs zip features).groupBy{
            case (pair, _) => extractEvtId(pair._1) // Group by their EventMention
          }.mapValues{
            v =>
              v.groupBy(r => ContextEngine.getContextKey(r._1._2)).mapValues(s =>  aggregateFeatures(s map (_._2))).toSeq
          }

        logger.info("printing aggregatedFeature details for debugging:")
        for((k,v) <- aggregatedFeatures) {
          logger.info(k + " event ID in aggregatedFeatures")
          logger.info(v(0).getClass.getSimpleName)

        }



        val oldDataIDPairs = collection.mutable.ListBuffer[(String, String, Int)]()
        oldDataSet.map(o => {
          val evt = o.EvtID
          val ctxId = o.CtxID
          val intId = o.label match{
            case Some(t) => if (t == true) 1 else 0
            case _ => 0
          }
          logger.info(evt + " : Evt ID from old data")
          logger.info(ctxId + " : Ctx ID from old data")
          val tup =(evt,ctxId, intId)
          oldDataIDPairs += tup
        })


        // Run the classifier for each pair and store the predictions
        val newPredTup = collection.mutable.ListBuffer[(String, String, Int)]()
        val predictions:Map[EventID, Seq[(ContextID, Boolean)]] = {
          val map = collection.mutable.HashMap[EventID, Seq[(ContextID, Boolean)]]()
          for((k,a) <- aggregatedFeatures) {
            logger.info(k.toString + ": Evt ID")
            val x = a.map {
              case (ctxId, aggregatedFeature) =>
                logger.info(ctxId._1 + " : "  +ctxId._2 + "  Is the context ID tuple in order of appearance of new data in predictions loop")
                val predArrayIntForm = trainedSVMInstance.predict(Seq(aggregatedFeature))

                logger.info(s"Prediction by svm: ${predArrayIntForm(0)}")
                val prediction = {
                  predArrayIntForm(0) match {
                    case 1 => true
                    case 0 => false
                    case _ => false
                  }
                }
                //val prediction = true
                val tup = (k.toString,ctxId._2,predArrayIntForm(0))
                newPredTup += tup
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
              val evtId = extractEvtId(evt)
              // fetch its predicted pairs
              val contexts = predictions(evtId)
              val result = compareCommonPairs(oldDataIDPairs.toArray, newPredTup.toArray)
              for((k,v) <- result) {
                logger.info(k + " : has scores in the following order: (train/test, Precision, recall, f1)" + v) }
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
    logger.info("inferring ctx-evt mention in SVMContextEngine")
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



  // the following code examines the best performing set from the ml4ai package.
  // the basic logic is that if a feature exists, it should have value 1 else 0.
  // When we apply this logic to any Seq[InputRow] (refer to ml4ai.data.oldDataPrep for the code), we may get many rows having value 1 for the same feature.
  // Note that this will affect the _min, _mean and _max values for every feature for that Seq[InputRow].
  // Given that the dataset on which we will test the model here is not read from file unlike ml4ai,
  // we have to take a slight detour of using InputRow and then AggregatedRowNew, instead of using AggregatedRowNew directly, as ml4ai does.
  // please contact the authors of the ml4ai package if you experience a roadblock while using the utilities it provides.

  private def extractFeatures(datum:(BioEventMention, BioTextBoundMention)):InputRow =
  {

    // val file
    val PMCID = datum._1.document.id match {
      case Some(c) => c
      case None => "Unknown"
    }
    val label = None
    val sentencePos = datum._1.sentence
    val evntId = extractEvtId(datum._1)
    val ctxId = ContextEngine.getContextKey(datum._2)

    val hardCodedFeatureNames = collection.mutable.ListBuffer[String]()
    val dependencyFeatures = allFeatures.toSet -- (hardCodedFeatures.toSet ++ Seq(""))
    numericFeaturesInputRow.map(h => {
      if(featSeq.contains(h))
        hardCodedFeatureNames += h
    })
    val ctxDepFeatures = collection.mutable.ListBuffer[String]()
    val evtDepFeatures = collection.mutable.ListBuffer[String]()
    dependencyFeatures foreach {
      case evt:String if evt.startsWith("evtDepTail") => {
        if(featSeq.contains(evt)) evtDepFeatures += evt
      }
      case ctx:String if ctx.startsWith("ctxDepTail")=> {
        if(featSeq.contains(ctx)) ctxDepFeatures += ctx
      }
    }

    InputRow(sentencePos,
      PMCID,
      label,
      evntId,
      ctxId._2,
      hardCodedFeatureNames.toArray,
      ctxDepFeatures.toSet,
      evtDepFeatures.toSet)


  }

  private def extractEvtId(evt:BioEventMention):EventID = {
    val sentIndex = evt.sentence
    val tokenIntervalStart = (evt.tokenInterval.start).toString()
    val tokenIntervalEnd = (evt.tokenInterval.end).toString()
    sentIndex+tokenIntervalStart+tokenIntervalEnd
  }

  private def aggregateFeatures(instances:Seq[InputRow]):AggregatedRow = {
    val label = None
    val featureSetNames = collection.mutable.ListBuffer[String]()
    val featureSetValues = collection.mutable.ListBuffer[Double]()
    val inputRows = instances
    for(in <- inputRows) {
      val ctxMappings = CodeUtils.aggregateInputRowFeats(in.ctx_dependencyTails.toSeq)
      val evtMappings = CodeUtils.aggregateInputRowFeats(in.evt_dependencyTails.toSeq)
      val specificMappings = CodeUtils.aggregateInputRowFeats(in.specificFeatureNames)
      val finalCtxPairings = CodeUtils.finalFeatValuePairing(ctxMappings)
      val finalEvtPairings = CodeUtils.finalFeatValuePairing(evtMappings)
      val finalSpecificPairings = CodeUtils.finalFeatValuePairing(specificMappings)

      def addToFeaturesArray(input: Seq[((String,String,String), (Double,Double,Double))]):Unit = {
        for((nameTup, valueTup) <- input) {
          val nameList = List(nameTup._1, nameTup._2, nameTup._3)
          val valueList = List(valueTup._1, valueTup._2, valueTup._3)
          featureSetNames ++= nameList
          featureSetValues ++= valueList
        }
      }
      addToFeaturesArray(finalSpecificPairings)
      addToFeaturesArray(finalCtxPairings)
      addToFeaturesArray(finalEvtPairings)

    }
    val newAggRow = AggregatedRow(0, "", "", "", label, featureSetValues.toArray,featureSetNames.toArray)
    newAggRow
  }


  private def compareCommonPairs(oldData: Array[(String,String,Int)], newData: Array[(String,String,Int)]): Map[String, (String, Double, Double, Double)] = {
    val oldKeys = oldData.map(s =>(s._1, s._2))
    val newKeys = newData.map(n => (n._1, n._2))
    val intersect = oldKeys.toSet.intersect(newKeys.toSet)
    val oldPrediction = collection.mutable.ListBuffer[Int]()
    val newPrediction = collection.mutable.ListBuffer[Int]()
    for((evt, ctx, label) <- oldData) {
      val tup = (evt,ctx)
      if(intersect.contains(tup))
        oldPrediction += label
    }

    for((evt, ctx, label) <- newData) {
      val tup = (evt,ctx)
      if(intersect.contains(tup))
        newPrediction += label
    }

    val name = "Comparing predictions of SVM on new data with old data"
    CodeUtils.scoreMaker(name, oldPrediction.toArray, newPrediction.toArray)
  }








}
