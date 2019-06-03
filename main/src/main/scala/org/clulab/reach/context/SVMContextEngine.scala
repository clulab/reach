package org.clulab.reach.context

import java.io._
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, CodeUtils, ContextPairInstance}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.collection.immutable

class SVMContextEngine(sentenceWindow:Option[Int] = None) extends ContextEngine with LazyLogging {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)

  var paperMentions:Option[Seq[BioTextBoundMention]] = None
  var orderedContextMentions:Map[Int, Seq[BioTextBoundMention]] = _
  var defaultContexts:Option[Map[String, String]] = None

  val svmWrapper = new LinearSVMContextClassifier(null)

  val config = ConfigFactory.load()
  val configPath = config.getString("contextEngine.params.svmPath")
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)


  logger.info(s"The SVM model has been tuned to the following settings: C: ${trainedSVMInstance.classifier.C}, Eps: ${trainedSVMInstance.classifier.eps}, Bias: ${trainedSVMInstance.classifier.bias}")

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

        val filteredPairs = sentenceWindow match {
          case Some(bound) =>
            pairs.filter {
              case (evt, ctx) =>
                Math.abs(evt.sentence - ctx.sentence) <= bound
            }
          case None =>
            pairs
        }



        val listForFolds = collection.mutable.ListBuffer[AggregatedContextInstance]()

        // here, we will use a Seq(Map), where each map has ContextPairInstance as a key, and as value, we have a tuple of feature values
        // so for a given ContextPairInstance, I can look up the table and return the values of the features present in the ContextPairInstance.
        val tempo = filteredPairs.map{p =>
          val featureExtractor = new FeatureExtractor(p, ctxMentions)
          featureExtractor.extractFeaturesToCalcByBestFeatSet()
        }
        val flattenedMap = tempo.flatMap(t=>t).toMap
        val features:Seq[ContextPairInstance] = tempo.flatMap(t => t.keySet)
        // Aggregate the features of all the instances of a pair
        val aggregatedFeatures:Map[EventID, Seq[(ContextID, AggregatedContextInstance)]] =
          (pairs zip features).groupBy{
            case (pair, _) => extractEvtId(pair._1) // Group by their EventMention
          }.mapValues{
            v =>
              v.groupBy(r => ContextEngine.getContextKey(r._1._2)).mapValues(s =>  {
                val seqOfInputRowsToPass = s map (_._2)
                val aggRow = aggregateFeatures(seqOfInputRowsToPass, flattenedMap)
              aggRow}).toSeq
          }

        val predictions:Map[EventID, Seq[(ContextID, Boolean)]] = {
          val map = collection.mutable.HashMap[EventID, Seq[(ContextID, Boolean)]]()
          for((k,a) <- aggregatedFeatures) {

            val x = a.map {
              case (ctxId, aggregatedFeature) =>
                val predArrayIntForm = trainedSVMInstance.predict(Seq(aggregatedFeature))
                listForFolds += aggregatedFeature
                // comment row to file function before testing
                //writeRowToFile(aggregatedFeature, k.toString, ctxId._2)
                val prediction = {
                  predArrayIntForm(0) match {
                    case 1 => true
                    case 0 => false
                    case _ => false
                  }
                }
                logger.info(s"For the paper ${aggregatedFeature.PMCID}, event ID: ${k.toString} and context ID: ${ctxId._2}, we have prediction: ${predArrayIntForm(0)}")

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
              val contexts = predictions.getOrElse(evtId, Seq.empty)

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



  // the following code examines the best performing set from the ml4ai package.
  // the basic logic is that if a feature exists, it should have value 1 else 0.
  // When we apply this logic to any Seq[ContextPairInstance] (refer to ml4ai.data.utils for the code), we may get many rows having value 1 for the same feature.
  // Note that this will affect the _min, _mean and _max values for every feature for that Seq[ContextPairInstance].
  // Given that the dataset on which we will test the model here is not read from file unlike ml4ai,
  // we have to take a slight detour of using ContextPairInstance and then AggregatedRowNew, instead of using AggregatedRowNew directly, as ml4ai does.
  // please contact the authors of the ml4ai package if you experience a roadblock while using the utilities it provides.


  def extractEvtId(evt:BioEventMention):EventID = {
    val sentIndex = evt.sentence
    val tokenIntervalStart = (evt.tokenInterval.start).toString()
    val tokenIntervalEnd = (evt.tokenInterval.end).toString()
    sentIndex+tokenIntervalStart+tokenIntervalEnd
  }

  def aggregateFeatures(instances:Seq[ContextPairInstance], featValLookUp:Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])]):AggregatedContextInstance = {

    val label = None
    val featureSetNames = collection.mutable.ListBuffer[String]()
    val featureSetValues = collection.mutable.ListBuffer[Double]()
    val inputRows = instances
    val featNameToVals = collection.mutable.Map[String,mutable.ListBuffer[Double]]()
    val specfeatureNamesToUse = instances(0).specificFeatureNames
    val ctxFeatureNamesToUse = instances(0).ctx_dependencyTails
    val evtFeatureNamesToUse = instances(0).evt_dependencyTails
    def featureValuePairing(aggr:Map[String,(Double,Double, Double, Int)]): Seq[(String,Double)] = {
      val pairings = collection.mutable.ListBuffer[(String,Double)]()
      for((key,value) <- aggr) {
        val extendedName = CodeUtils.extendFeatureName(key)
        val minTup = (extendedName._1, value._1)
        val maxTup = (extendedName._2, value._2)
        val avgTup = (extendedName._3, value._3/value._4)

        val list = ListBuffer(minTup, maxTup, avgTup)
        pairings ++= list
      }
      pairings
    }

    def addAggregatedOnce(input: Seq[(String, Double)]):Unit = {
      for((name,value) <- input) {
        featureSetNames += name
        featureSetValues += value
      }
    }


    // we read through the input row values and add them to a name -> list of features map.
    // So for a given feature name as key, we will have a list of double as values, where the doubles are the values to the feature in a given input row.

    for(in <- inputRows) {
      val (specificVals, evtVals, ctxVals) = featValLookUp(in)
      for((spec,value)<-specificVals) {
        if(featNameToVals.contains(spec)) {
          val currentList = featNameToVals(spec)
          currentList += value
        }
        else {
          val toAddVal = collection.mutable.ListBuffer[Double]()
          toAddVal += value
          featNameToVals ++= Map(spec -> toAddVal)
        }
      }

      for((spec,value)<-evtVals) {
        if(featNameToVals.contains(spec)) {
          val currentList = featNameToVals(spec)
          currentList += value
        }
        else {
          val toAddVal = collection.mutable.ListBuffer[Double]()
          toAddVal += value
          featNameToVals ++= Map(spec -> toAddVal)
        }
      }

      for((spec,value)<-ctxVals) {
        if(featNameToVals.contains(spec)) {
          val currentList = featNameToVals(spec)
          currentList += value
        }
        else {
          val toAddVal = collection.mutable.ListBuffer[Double]()
          toAddVal += value
          featNameToVals ++= Map(spec -> toAddVal)
        }
      }
    }
    val aggregatedSpecVals = aggregateInputRowFeatValues(specfeatureNamesToUse, featNameToVals.toMap)
    val aggregatedctxDepVals = aggregateInputRowFeatValues(ctxFeatureNamesToUse.toSeq, featNameToVals.toMap)
    val aggregatedevtDepVals = aggregateInputRowFeatValues(evtFeatureNamesToUse.toSeq, featNameToVals.toMap)

    val specFeatVal = featureValuePairing(aggregatedSpecVals)
    val ctxFeatVal = featureValuePairing(aggregatedctxDepVals)
    val evtFeatVal = featureValuePairing(aggregatedevtDepVals)

    addAggregatedOnce(specFeatVal)
    addAggregatedOnce(ctxFeatVal)
    addAggregatedOnce(evtFeatVal)
    val newAggRow = AggregatedContextInstance(0, instances(0).PMCID, "", "", label, featureSetValues.toArray,featureSetNames.toArray)
    newAggRow
  }

  private def writeRowToFile(row:AggregatedContextInstance, evtID: String, ctxID: String):Unit = {
    val typeOfPaper = config.getString("svmContext.paperType")
    val dirForType = if(typeOfPaper.length != 0) config.getString("papersDir").concat(s"/${typeOfPaper}") else config.getString("papersDir")
    val fileListUnfiltered = new File(dirForType)
    val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
    val currentPMCID = s"PMC${row.PMCID.split("_")(0)}"
    for(file <- fileList) {
      val fileNamePMCID = file.getName.slice(0,file.getName.length-5)
      val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${fileNamePMCID}")
      // creating output directory if it doesn't already exist
      val outputPaperDir = new File(outPaperDirPath)
      if(!outputPaperDir.exists()) {
        outputPaperDir.mkdirs()
      }



        if(currentPMCID == fileNamePMCID) {
          val pathForRow = outPaperDirPath.concat(s"/AggregatedRow_${currentPMCID}_${evtID}_${ctxID}.txt")
          val sentenceFile = new File(pathForRow)
          if (!sentenceFile.exists()) {
            sentenceFile.createNewFile()
          }
          val os = new ObjectOutputStream(new FileOutputStream(pathForRow))

          os.writeObject(row)
          os.close()
        }



    }
  }

  def aggregateInputRowFeatValues(features:Seq[String], lookUpTable: Map[String,mutable.ListBuffer[Double]]):Map[String,(Double,Double, Double, Int)] = {
    val resultingMap = collection.mutable.Map[String,(Double,Double, Double, Int)]()
    for(r <- features) {
      if(lookUpTable.contains(r)) {
        val valueList = lookUpTable(r)
        val min = valueList.foldLeft(Double.MaxValue)(Math.min(_,_))
        val max = valueList.foldLeft(Double.MinValue)(Math.max(_,_))
        val sum = valueList.foldLeft(0.0)(_ + _)
        val tup = (min,max,sum,valueList.size)
        resultingMap ++= Map(r -> tup)
      }

      else {
        val tup = (0.0, 0.0, 0.0, 1)
        resultingMap ++= Map(r -> tup)
      }
    }
    resultingMap.toMap
  }



}
