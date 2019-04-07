package org.clulab.reach.context


import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import org.ml4ai.data.classifiers.LinearSVMWrapper
import org.ml4ai.data.utils.{AggregatedRow, Balancer, CodeUtils, InputRow}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

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
  val untrainedSVMInstance = svmWrapper.loadFrom(untrainedConfigPath)


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

        val oldDataIDPairs = collection.mutable.ListBuffer[(String, String, String, Int)]()
        oldDataSet.map(o => {
          val evt = o.EvtID
          val ctxId = o.CtxID
          val intLabel = o.label match{
            case Some(t) => if (t == true) 1 else 0
            case _ => 0
          }
          val numArray = evt.split("_")
          val sentIndStr = numArray(0)
          val sentIndS = sentIndStr.slice(3, sentIndStr.length)
          val sentInt = Integer.parseInt(sentIndS)
          val tokenIntervalStart = Integer.parseInt(numArray(1))
          val tokenIntervalEnd =Integer.parseInt(numArray(2).take(numArray(2).length-1))
          val numericalId = sentInt+""+tokenIntervalStart+""+tokenIntervalEnd
          val tup =(o.PMCID, numericalId,ctxId, intLabel)
          oldDataIDPairs += tup
        })


        // Run the classifier for each pair and store the predictions
        val newDataIdPairs = collection.mutable.ListBuffer[(String, String, String, Int)]()
        val dataToPassForCrossVal = collection.mutable.ListBuffer[AggregatedRow]()
        val predictions:Map[EventID, Seq[(ContextID, Boolean)]] = {
          val map = collection.mutable.HashMap[EventID, Seq[(ContextID, Boolean)]]()
          for((k,a) <- aggregatedFeatures) {

            val x = a.map {
              case (ctxId, aggregatedFeature) =>
                val predArrayIntForm = trainedSVMInstance.predict(Seq(aggregatedFeature))
                dataToPassForCrossVal += aggregatedFeature
                logger.info(s"Prediction by svm: ${predArrayIntForm(0)}")
                val prediction = {
                  predArrayIntForm(0) match {
                    case 1 => true
                    case 0 => false
                    case _ => false
                  }
                }
                //val prediction = true
                val tup = (aggregatedFeature.PMCID, k.toString,ctxId._2,predArrayIntForm(0))
                newDataIdPairs += tup
                (ctxId, prediction)
            }

            val entry = Map(k -> x)
            map ++= entry

          }
          map.toMap
        }

        val resultsFromCrossVal = crossValOnNewPairs(dataToPassForCrossVal.toArray)
        for((k,v) <- resultsFromCrossVal) {
          logger.info(k + " : " + v + " result of performing 5 fold cross val on new annotation")
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
              //val result = compareCommonPairs(oldDataIDPairs.toArray, newDataIdPairs.toArray)
//              for((k,v) <- result) {
//                logger.info(k + " : has scores in the following order: (train/test, Precision, recall, f1)" + v) }
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
  // When we apply this logic to any Seq[InputRow] (refer to ml4ai.data.utils for the code), we may get many rows having value 1 for the same feature.
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
    val newAggRow = AggregatedRow(0, instances(0).PMCID, "", "", label, featureSetValues.toArray,featureSetNames.toArray)
    newAggRow
  }


  private def compareCommonPairs(oldData: Array[(String, String,String,Int)], newData: Array[(String, String,String,Int)]): Map[String, (String, Double, Double, Double)] = {
    val oldKeys = oldData.map(s =>(s._1, s._2, s._3))
    val newKeys = newData.map(n => (n._1, n._2, n._3))
    val oldGroupedByPMCID = oldKeys.groupBy(_._1).mapValues(x => x.map(y => (y._2,y._3)))
    val newGroupedByPMCID = newKeys.groupBy(_._1)
    var modifiedNewKeys = collection.mutable.HashMap[String, Array[(String,String)]]()
    for((pmcid,seq) <- newGroupedByPMCID) {
      val split = pmcid.split("_")
      val adjust = s"b'PMC${split(0)}'"
      val tempo = seq.map(s => (s._2, s._3))
      val entry = Map(adjust -> tempo)
      modifiedNewKeys ++= entry
    }

    for((pmcid, arr) <- modifiedNewKeys) {
      val oldCounterPart = oldGroupedByPMCID(pmcid)
      logger.info(oldCounterPart.size + " : Size of old evt-ctx pairs by paper")
      logger.info(arr.size + " : Size of new evt-ctx pairs by paper")
      val zip = oldCounterPart zip arr
      for((oldK, newK) <- zip) {
        logger.info(s"Evt ID of old data: ${oldK._1} and Ctx ID of old data: ${oldK._2}")
        logger.info(s"Evt ID of new data: ${newK._1} and Ctx ID of new data: ${newK._2}")
      }
      //val intersect = arr.toSet.intersect(oldCounterPart.toSet)

    }


    val oldPrediction = collection.mutable.ListBuffer[Int]()
    val newPrediction = collection.mutable.ListBuffer[Int]()


    val name = "Comparing predictions of SVM on new data with old data"
    CodeUtils.scoreMaker(name, oldPrediction.toArray, newPrediction.toArray)
  }


  private def crossValOnNewPairs(dataSet: Array[AggregatedRow]): Map[String, (String, Double, Double, Double)] = {
    val giantTruthTestLabel = new mutable.ArrayBuffer[Int]()
    val giantPredTestLabel = new mutable.ArrayBuffer[Int]()
    val folds = prepareFolds(dataSet)
    for((trainIndices, testIndices) <- folds) {
      val trainingData = trainIndices.collect{case x => dataSet(x)}
      val balancedTrainingData = Balancer.balanceByPaperAgg(trainingData, 1)
      val (trainDataSet, _) = untrainedSVMInstance.dataConverter(balancedTrainingData)
      untrainedSVMInstance.fit(trainDataSet)


      val testingData = testIndices.collect{case trex => dataSet(trex)}
      val testLabelsTruth = untrainedSVMInstance.createLabels(testingData)
      giantTruthTestLabel ++= testLabelsTruth
      val testLabelsPred = untrainedSVMInstance.predict(testingData)
      giantPredTestLabel ++= testLabelsPred


    }
    val name = "Testing f1 of svm trained on new evt-ctx pairs"
    CodeUtils.scoreMaker(name, giantTruthTestLabel.toArray, giantPredTestLabel.toArray)
  }

  // creates folds for data for 5 fold cross validation
  // we will generate the folds by the following algorithm:
  // start = 0
  // for i in range(1,5): (5 is included in this range)
  //   stop = (list.size/5) * i
  //   testSlice = list.slice(start,stop)
  //   start = stop + 1
  //   trainSlice = list.tosSet -- testSlice.toSet

  private def prepareFolds(data: Array[AggregatedRow]):Array[(Array[Int], Array[Int])] = {
    val list = collection.mutable.ListBuffer[(Array[Int], Array[Int]]()
    val trainIndices = collection.mutable.ListBuffer[Int]()
    val testIndices = collection.mutable.ListBuffer[Int]()
    var start = 0
    for(i<- 1 to 5) {
      val stop = (data.size/5)*i
      val testSlice = data.slice(start,stop)
      start = stop + 1
      val testSliceIndex = testSlice.map(data.indexOf(_))
      val trainSlice = data.toSet -- testSlice.toSet
      val trainSliceIndex = trainSlice.map(data.indexOf(_))
      trainIndices ++= trainSliceIndex.toArray
      testIndices ++= testSliceIndex.toArray
      val tup = (trainIndices.toArray, testIndices.toArray)
      list += tup
    }
    list.toArray
  }








}
