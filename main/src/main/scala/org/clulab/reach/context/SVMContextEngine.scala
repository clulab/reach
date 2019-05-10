package org.clulab.reach.context

import java.io._
import java.io.PrintWriter
import org.clulab.processors.Document
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import org.ml4ai.data.classifiers.LinearSVMWrapper
import org.ml4ai.data.utils.{AggregatedRow, Balancer, CodeUtils, InputRow}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
//changed ram to 62G
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
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val configAllFeaturesPath = config.getString("contextEngine.params.allFeatures")
  val hardCodedFeaturesPath = config.getString("contextEngine.params.hardCodedFeatures")
  val hardCodedFeatures = CodeUtils.readHardcodedFeaturesFromFile(hardCodedFeaturesPath)
  val numericFeaturesInputRow = hardCodedFeatures.drop(4)
  val (allFeatures, bestFeatureDict) = CodeUtils.featureConstructor(configAllFeaturesPath)
  val featSeq = bestFeatureDict("NonDep_Context")
  var specFeatValPair = Map[String,Double]()
  var ctxDepFeatValPair = Map[String,Double]()
  var evtDepFeatValPair = Map[String,Double]()



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

        val filteredPairs = pairs filter {
          case (evt, ctx) =>
            Math.abs(evt.sentence - ctx.sentence) <= 50
        }

        // Extract features for each of the pairs
        // change to filtered pairs when you know the best value of sentence distance.
        //val features:Seq[InputRow] = filteredPairs map extractFeaturesToCalcByBestFeatSet
        val features:Seq[InputRow] = pairs map extractFeaturesToCalcByBestFeatSet
        val freqOfSentDist = countSentDistValueFreq(features.toArray)
        writeSentFreqToFile(freqOfSentDist)
        // Aggregate the features of all the instances of a pair
        val aggregatedFeatures:Map[EventID, Seq[(ContextID, AggregatedRow)]] =
          (pairs zip features).groupBy{
            // change to filtered pairs when you know the best value of sentence distance.
          //(filteredPairs zip features).groupBy{
            case (pair, _) => extractEvtId(pair._1) // Group by their EventMention
          }.mapValues{
            v =>
              v.groupBy(r => ContextEngine.getContextKey(r._1._2)).mapValues(s =>  aggregateFeatures(s map (_._2))).toSeq
          }

        val predictions:Map[EventID, Seq[(ContextID, Boolean)]] = {
          val map = collection.mutable.HashMap[EventID, Seq[(ContextID, Boolean)]]()
          for((k,a) <- aggregatedFeatures) {

            val x = a.map {
              case (ctxId, aggregatedFeature) =>
                val predArrayIntForm = trainedSVMInstance.predict(Seq(aggregatedFeature))
                val prediction = {
                  predArrayIntForm(0) match {
                    case 1 => true
                    case 0 => false
                    case _ => false
                  }
                }

                val sentenceDistance_max = "sentenceDistance_max"
                val index = aggregatedFeature.featureGroupNames.indexOf(sentenceDistance_max)
                val value = aggregatedFeature.featureGroups(index)
                logger.info(s"For the paper ${aggregatedFeature.PMCID}, event ID: ${k.toString} and context ID: ${ctxId._2}, we have prediction: ${predArrayIntForm(0)} and max sent distance: ${value}")
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
  // When we apply this logic to any Seq[InputRow] (refer to ml4ai.data.utils for the code), we may get many rows having value 1 for the same feature.
  // Note that this will affect the _min, _mean and _max values for every feature for that Seq[InputRow].
  // Given that the dataset on which we will test the model here is not read from file unlike ml4ai,
  // we have to take a slight detour of using InputRow and then AggregatedRowNew, instead of using AggregatedRowNew directly, as ml4ai does.
  // please contact the authors of the ml4ai package if you experience a roadblock while using the utilities it provides.

  private def extractFeaturesToCalcByBestFeatSet(datum:(BioEventMention, BioTextBoundMention)):InputRow =
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
    val ctxDepFeatures = collection.mutable.ListBuffer[String]()
    val evtDepFeatures = collection.mutable.ListBuffer[String]()


    def unAggregateFeatureName(features: Seq[String]): Array[String] = {
      val fixedFeatureNames = collection.mutable.ListBuffer[String]()
      for(f <- features) {
        if(f.contains("_min") || f.contains("_max") || f.contains("_avg")) {
          val subst = f.slice(0,f.length-4)
          fixedFeatureNames += subst
        }
        else fixedFeatureNames += f
      }
      fixedFeatureNames.toArray
    }


    val dependencyFeatures = unAggregateFeatureName(allFeatures).toSet -- (unAggregateFeatureName(hardCodedFeatures).toSet ++ Seq(""))
    unAggregateFeatureName(numericFeaturesInputRow).map(h => {
      if(unAggregateFeatureName(featSeq).contains(h))
        hardCodedFeatureNames += h
    })

    dependencyFeatures foreach {
      case evt:String if evt.startsWith("evtDepTail") => {
        if(unAggregateFeatureName(featSeq).contains(evt)) evtDepFeatures += evt
      }
      case ctx:String if ctx.startsWith("ctxDepTail")=> {
        if(unAggregateFeatureName(featSeq).contains(ctx)) ctxDepFeatures += ctx
      }
    }




    // call feature value extractor here

    specFeatValPair = calculateSpecificFeatValues(datum)
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
      val ctxMappings = aggregateInputRowFeatValues(in.ctx_dependencyTails.toSeq, ctxDepFeatValPair)
      val evtMappings = aggregateInputRowFeatValues(in.evt_dependencyTails.toSeq, evtDepFeatValPair)
      val specificMappings = aggregateInputRowFeatValues(in.specificFeatureNames, specFeatValPair)


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

      val altPairingCtx = featureValuePairing(ctxMappings)
      val altPairingEvt = featureValuePairing(evtMappings)
      val altPairingSpec = featureValuePairing(specificMappings)


      // look at the previous code again and make sure the values are the same
      def addAggregatedOnce(input: Seq[(String, Double)]):Unit = {
        for((name,value) <- input) {
          featureSetNames += name
          featureSetValues += value
        }
      }



      addAggregatedOnce(altPairingSpec)
      addAggregatedOnce(altPairingCtx)
      addAggregatedOnce(altPairingEvt)

    }
    val newAggRow = AggregatedRow(0, instances(0).PMCID, "", "", label, featureSetValues.toArray,featureSetNames.toArray)
    newAggRow
  }



  private def countSentDistValueFreq(seq: Array[InputRow]): Array[(Int,Int)] = {
    val map = collection.mutable.HashMap[Int, Int]()
    val result = collection.mutable.ListBuffer[(Int, Int)]()
    seq.map(s => {
      val currentIndex = s.sentenceIndex
      if(map.contains(currentIndex)) {
        var cur = map(currentIndex)
        cur += 1
        val entry = Map(currentIndex -> cur)
        map ++= entry
      }

      else {
        val tempMap = Map(currentIndex -> 1)
        map ++= tempMap
      }
    })

    for((k,v) <- map) {
        val tup = (k,v)
        result += tup
    }
    result.toArray
  }

  private def writeSentFreqToFile(frequencyList: Array[(Int,Int)]):Unit = {
    val typeOfPaper = "activation"
    val dirForType = config.getString("papersDir").concat(s"/${typeOfPaper}")
    val fileListUnfiltered = new File(dirForType)
    val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
    for(file <- fileList) {
      val pmcid = file.getName.slice(0,file.getName.length-5)
      val outPaperDirPath = config.getString("contextEngine.params.contextOutputDir").concat(s"${pmcid}")
      val outputPaperDir = new File(outPaperDirPath)
      if(!outputPaperDir.exists()) {
        outputPaperDir.mkdirs()
      }

      val pathForSentences = outPaperDirPath.concat("/sentenceDistFreq.txt")

      val sentenceDistFile = new File(pathForSentences)

      if (!sentenceDistFile.exists()) {
        sentenceDistFile.createNewFile()
      }

      val pw = new PrintWriter(sentenceDistFile)
      for ((dist,freq) <- frequencyList) {
        pw.write(s"${dist}:${freq}")
        pw.write("\n")
      }
      pw.close()
    }
  }


  private def intersentenceDependencyPath(datum:(BioEventMention, BioTextBoundMention)): Option[Seq[String]] = {
    def pathToRoot(currentNodeIndx:Int, currentSentInd:Int, currentDoc:Document): Seq[String] = {
      val dependencies = currentDoc.sentences(currentSentInd).dependencies.get
      val allRoots = dependencies.roots.toSeq
      val paths = allRoots flatMap  {
        r =>
          val ps = dependencies.shortestPathEdges(r, currentNodeIndx)
          ps map (sequence => sequence map (_._3))
      }

      paths.sortBy(p => p.size).head
    }
    val evtShortestPath = pathToRoot(datum._1.tokenInterval.start, datum._1.sentence, datum._1.document)
    val ctxShortestPath = pathToRoot(datum._2.tokenInterval.start, datum._2.sentence, datum._2.document)
    val numOfJumps = Seq.fill(Math.abs(datum._1.sentence - datum._2.sentence))("sentenceJump")

    val first = if(datum._1.sentence < datum._2.sentence) evtShortestPath else ctxShortestPath
    val second = if(datum._2.sentence < datum._1.sentence) ctxShortestPath else evtShortestPath
    val selectedPath = (first.reverse ++ numOfJumps ++ second).map(FeatureProcessing.clusterDependency)

    val bigrams = (selectedPath zip selectedPath.drop(1)).map{ case (a, b) => s"${a}_${b}" }

    Some(bigrams)
  }


  private def constructDependencyPath(datum:(BioEventMention, BioTextBoundMention)): Option[Seq[String]] = {
    if(datum._1.sentence == datum._2.sentence) {
      val currentSentContents = datum._1.document.sentences(datum._1.sentence)
      val dependencies = currentSentContents.dependencies.get
      val (first, second) = if(datum._1.tokenInterval.start <= datum._2.tokenInterval.start) (datum._1.tokenInterval, datum._2.tokenInterval) else (datum._2.tokenInterval, datum._1.tokenInterval)

      val paths = first flatMap {
        i:Int =>
          second flatMap  {
            j:Int =>
              val localPaths:Seq[Seq[String]] = dependencies.shortestPathEdges(i, j, ignoreDirection = true) map (s => s map (_._3))
              localPaths
          }
      }
      val sequence = Try(paths.filter(_.size > 0).sortBy(_.size).head.map(FeatureProcessing.clusterDependency))
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
    else intersentenceDependencyPath(datum)
  }





  private def calculateSpecificFeatValues(datum:(BioEventMention, BioTextBoundMention)):Map[String,Double] = {
    val result = collection.mutable.Map[String,Double]()


    val sentenceDistance = Math.abs(datum._1.sentence - datum._2.sentence)
    val sentDistEntry = Map("sentenceDistance" -> sentenceDistance.toDouble)
    result ++= sentDistEntry

    val dependencyPath = constructDependencyPath(datum)
    val dependencyDistance = dependencyPath match {
      case Some(path) => path.size.toDouble
      case None => 0.0
    }

    val dependencyDistEntry = Map("dependencyDistance" -> dependencyDistance)
    result ++= dependencyDistEntry


    result.toMap
  }

  def aggregateInputRowFeatValues(features:Seq[String], lookUpTable: Map[String,Double]):Map[String,(Double,Double, Double, Int)] = {
    val resultingMap = collection.mutable.Map[String,(Double,Double, Double, Int)]()
    for(r <- features) {
      if(resultingMap.contains(r)) {

        val valueToBeAdded = if(lookUpTable.contains(r)) lookUpTable(r) else 1.0
        val currentFeatDetails = resultingMap(r)
        val tupReplace = (Math.min(currentFeatDetails._1, valueToBeAdded),
          Math.max(currentFeatDetails._2, valueToBeAdded),
          currentFeatDetails._3 + valueToBeAdded,
          currentFeatDetails._4+1)
        resultingMap(r) = tupReplace

      }
      else {
        val valForNewEntry = if(lookUpTable.contains(r)) lookUpTable(r) else 1.0
        val entry = (r -> (valForNewEntry,valForNewEntry,valForNewEntry,1))
        resultingMap += entry
      }
    }
    resultingMap.toMap
  }

}
