package org.clulab.reach.context.feature_utils

import com.typesafe.config.ConfigFactory
import org.clulab.context.utils.ContextPairInstance
import org.clulab.processors.Document
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.context.utils.svm_training_utils.IOUtilsForFeatureName
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}
import org.clulab.struct.Interval

import scala.util.{Failure, Success, Try}


// This class calculates the values of pre-set linguistic features for (context-event) pairs detected by reach in previously unseen papers. The names of features are read from file, and the given pair i.e. (BioEventMention, BioTextBoundMention) is used to calculate the values of the features
// Please contact Dr. Clayton Morrison's team for further information on the selection of the feature names.
class ContextFeatureExtractor(datum:(BioEventMention, BioTextBoundMention), contextMentions:Seq[BioTextBoundMention]){
  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)
  def extractFeaturesToCalcByBestFeatSet():Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])] = {
    val config = ConfigFactory.load()
    // we need the contextSpecificDependencyFeatures file to get the names of the dependency features for which we need to calculate values.
    // The same holds for specificNonDependencyFeatureNames. These are specific and much smaller in number.
    // It takes a different procedure to calculate the values of these features, the details of which can be obtained below.

    val resourcesPath = "/org/clulab/context/svmFeatures"


    val pathToSpecificNonDepFeatures = s"${resourcesPath}/specific_nondependency_featurenames.txt"
    val urlToSpecificNonDep = getClass.getResource(pathToSpecificNonDepFeatures)
    // this function call to getResource returns to us a URL that is the path to the file svm_model.dat
    // the variable urlToSpecificNonDep holds the value file:/home/....
    // so we need to take the shorter version of it that starts from /home/...
    val truncatedPathToSpecificNonDep = urlToSpecificNonDep.toString.replace("file:","")
    val specificNonDepFeatureList = IOUtilsForFeatureName.readSpecificNonDependencyFeatureNames(truncatedPathToSpecificNonDep)

    val pathToAllFeatures = s"${resourcesPath}/all_feature_names_file.txt"
    val urlToAllFeatures = getClass.getResource(pathToAllFeatures)
    val truncatedPathToAllFeatures = urlToAllFeatures.toString.replace("file:","")


    val numericFeaturesInputRow = specificNonDepFeatureList.drop(4)
    val bestFeatureDict = ContextFeatureUtils.featureConstructor(truncatedPathToAllFeatures)

    // Over all the feature names that were used, an exhaustive ablation study was performed to study the best performing subset of features,
    // and this was found to be the union of non-dependency features and context-dependency features.
    // We will only calculate the values of this subset of features.
    val featSeq = bestFeatureDict("NonDep_Context")
    val allFeatures = bestFeatureDict("All_features")
    // val file
    val contextFrequencyMap = calculateContextFreq(contextMentions)
    val PMCID = datum._1.document.id match {
      case Some(c) => c
      case None => "Unknown"
    }
    val label = None
    val sentencePos = datum._1.sentence
    val evntId = ContextFeatureUtils.extractEvtId(datum._1)
    val ctxId = ContextEngine.getContextKey(datum._2)

    val hardCodedFeatureNames = collection.mutable.ListBuffer[String]()
    val ctxDepFeatures = collection.mutable.ListBuffer[String]()
    val evtDepFeatures = collection.mutable.ListBuffer[String]()


    // the names of the features read from file already contained _min, _max, etc, implying that feature values have been pre-aggregated when the Linear SVM model was trained.
    // In order to maintain parity, we will "unaggregate" the feature names from file, so that feature names will be the same for the pre-trained SVM model and the fresh test dataset

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




    // we add to the list of features the specific, non-dependency features like sentenceDistance, dependencyDistance, etc.
    val dependencyFeatures = unAggregateFeatureName(allFeatures).toSet -- (unAggregateFeatureName(specificNonDepFeatureList).toSet ++ Seq(""))
    unAggregateFeatureName(numericFeaturesInputRow).map(h => {
      if(unAggregateFeatureName(featSeq).contains(h))
        hardCodedFeatureNames += h
    })


    // here we classify the feature names into two separate lists, one that has event dependency feature values,
    // and one that has context dependency values.
    dependencyFeatures foreach {
      case evt:String if evt.startsWith("evtDepTail") => {
        if(unAggregateFeatureName(featSeq).contains(evt)) evtDepFeatures += evt
      }
      case ctx:String if ctx.startsWith("ctxDepTail")=> {
        if(unAggregateFeatureName(featSeq).contains(ctx)) ctxDepFeatures += ctx
      }
    }



    // call feature value extractor here
    // we will filter the feature names into "specific features", i.e. non-dependency features, context-dependency features, and event dependency features
    val specFeatVal = calculateSpecificFeatValues(datum, contextMentions, contextFrequencyMap.toMap)
    val evtDepFeatVal = calculateEvtDepFeatureVals(datum)
    val ctxDepFeatVal = calculateCtxDepFeatureVals(datum)

    val row = ContextPairInstance(sentencePos,
      PMCID,
      label,
      evntId,
      ctxId._2,
      hardCodedFeatureNames.toSet.toArray,
      ctxDepFeatures.toSet,
      evtDepFeatures.toSet)

    val entry = Map(row -> (specFeatVal, evtDepFeatVal, ctxDepFeatVal))

    entry
  }




 // this function extracts the values of the specific, non-dependency features mentioned above.
 // It takes as input:
 // a) tuple of event and context mention,
 // b) all the mentions detected by Reach for the given paper,
 // c) frequency of occurrence of each feature, which will prove necessary for feature names such as context_frequency.
  // It returns to us a map of strings that are the feature names, with their corresponding values.
  private def calculateSpecificFeatValues(datum:(BioEventMention, BioTextBoundMention), contextMentions:Seq[BioTextBoundMention], ctxTypeFreq:Map[String,Double]):Map[String,Double] = {
    val event = datum._1
    val context = datum._2
    val doc = event.document
    val result = collection.mutable.Map[String,Double]()
    // ****************INTEGER VALUE FEATURES BEGIN****************
    val evntId = ContextFeatureUtils.extractEvtId(datum._1)
   val ctxId = ContextEngine.getContextKey(datum._2)
    println(s"The current pair is: ${evntId},${ctxId}")
    val sentenceDistance = Math.abs(datum._1.sentence - datum._2.sentence)
    val sentDistEntry = Map("sentenceDistance" -> sentenceDistance.toDouble)
    result ++= sentDistEntry
    println(s"The current pair has sentence distance: ${sentenceDistance}")

    val dependencyPath = constructDependencyPath(datum)
    val dependencyDistance = dependencyPath match {
      case Some(path) => {
        path.size.toDouble}
      case None => 0.0
    }


   println(s"The current dependency path is : ")
   println(dependencyPath)

    val dependencyDistEntry = Map("dependencyDistance" -> dependencyDistance)
    result ++= dependencyDistEntry

    val context_frequency = ctxTypeFreq(context.nsId())
    result ++= Map("context_frequency" -> context_frequency)
    println(s"The current context frequency is : ${context_frequency}")



    // Dependency tails
    val evtDependencyTails = dependencyTails(event.sentence,event.tokenInterval, doc)
    val ctxDependencyTails = dependencyTails(context.sentence, context.tokenInterval, doc)
    // ****************INTEGER VALUE FEATURES END****************



    // ****************BOOLEAN VALUE FEATURES BEGIN****************
    val evtSentenceFirstPerson = if(eventSentenceContainsPRP(doc, event)) 1.0 else 0.0
    val evtSentenceFirstPersonEntry = Map("evtSentenceFirstPerson" -> evtSentenceFirstPerson)
    result ++= evtSentenceFirstPersonEntry

    val ctxSentenceFirstPerson = if(contextSentenceContainsPRP(doc, context)) 1.0 else 0.0
    val ctxSentenceFirstPersonEntry = Map("ctxSentenceFirstPerson" -> ctxSentenceFirstPerson)
    result ++= ctxSentenceFirstPersonEntry


    val evtSentencePastTense = if(eventSentenceContainsPastTense(doc, event)) 1.0 else 0.0
    result ++= Map("evtSentencePastTense" -> evtSentencePastTense)


    val ctxSentencePastTense = if(contextSentenceContainsPastTense(doc, context)) 1.0 else 0.0
    result ++= Map("ctxSentencePastTense" -> ctxSentencePastTense)


    val evtSentencePresentTense = if(eventSentenceContainsPresentTense(doc, event)) 1.0 else 0.0
    result ++= Map("evtSentencePresentTense" -> evtSentencePresentTense)

    val ctxSentencePresentTense = if(contextSentenceContainsPresentTense(doc, context)) 1.0 else 0.0
    result ++= Map("ctxSentencePresentTense" -> ctxSentencePresentTense)

    val closesCtxOfClass = if(isItClosestContextOfSameCategory(event, context, contextMentions)) 1.0 else 0.0
    result ++= Map("closesCtxOfClass" -> closesCtxOfClass)
    println(s"Closest context of class value for this pair : ${closesCtxOfClass}")


    // Negation in context mention
    val ctxNegationInTail = if(ctxDependencyTails.filter(tail => tail.contains("neg")).size > 0) 1.0 else 0.0
    result ++= Map("ctxNegationIntTail" -> ctxNegationInTail)


    val evtNegationInTail = if(evtDependencyTails.filter(tail => tail.contains("neg")).size > 0) 1.0 else 0.0
    result ++= Map("evtNegationInTail" -> evtNegationInTail)
    // ****************BOOLEAN VALUE FEATURES END****************


    result.toMap
  }


  // this function calculates the value of all the event dependency features.
  // it takes as input the tuple of (eventID, contextID),
  // and returns a map of feature_name -> feature_value, similar to the function(s) above

  private def calculateEvtDepFeatureVals(datum:(BioEventMention, BioTextBoundMention)):Map[String,Double] = {
    val event = datum._1
    val doc = event.document
    val evtDependencyTails = dependencyTails(event.sentence,event.tokenInterval, doc)
    val evtDepStrings = evtDependencyTails.map(e => e.mkString("_"))
    evtDepStrings.map(t => s"evtDepTail_$t").groupBy(identity).mapValues(_.length)
  }


  // this function calculates the value of all the context dependency features.
  // it takes as input the tuple of (eventID, contextID),
  // and returns a map of feature_name -> feature_value, similar to the function(s) above
  private def calculateCtxDepFeatureVals(datum:(BioEventMention, BioTextBoundMention)):Map[String,Double] = {
    val context = datum._2
    val doc = context.document

    val ctxDependencyTails = dependencyTails(context.sentence, context.tokenInterval, doc)
    val ctxDepStrings = ctxDependencyTails.map(c => c.mkString("_"))

    ctxDepStrings.map(t => s"ctxDepTail_$t").groupBy(identity).mapValues(_.length).mapValues(_.toDouble)
  }





  // ****** starting utility functions to calculate values of dependency features **********
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
    val evtShortestPath = datum._1.tokenInterval.map(ix => pathToRoot(ix, datum._1.sentence, datum._1.document)).sortBy(_.size).head
    val ctxShortestPath = datum._2.tokenInterval.map(ix => pathToRoot(ix, datum._2.sentence, datum._2.document)).sortBy(_.size).head
    //val evtShortestPath = pathToRoot(datum._1.tokenInterval.start, datum._1.sentence, datum._1.document)
    //val ctxShortestPath = pathToRoot(datum._2.tokenInterval.start, datum._2.sentence, datum._2.document)
    val numOfJumps = Seq.fill(Math.abs(datum._1.sentence - datum._2.sentence))("sentenceJump")

    val first = if(datum._1.sentence < datum._2.sentence) evtShortestPath else ctxShortestPath
    val second = if(datum._2.sentence < datum._1.sentence) ctxShortestPath else evtShortestPath
    val selectedPath = (first.reverse ++ numOfJumps ++ second).map(POSMaker.clusterDependency)

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
      val sequence = Try(paths.filter(_.size > 0).sortBy(_.size).head.map(POSMaker.clusterDependency))
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

  def sentenceContainsPRP(doc:Document, ix:Int):Boolean = {
    val targetWords = Set("we", "us", "our", "ours", "ourselves", "i", "me", "my", "mine", "myself")
    val sentence = doc.sentences(ix)
    val tags = sentence.tags.get
    val lemmas = sentence.lemmas.get

    val x = (tags zip lemmas) filter {
      case (tag, lemma) =>
        tag == "PRP" && targetWords.contains(lemma)

    }

    !x.isEmpty
  }

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
        edges flatMap {
          e =>
            val label = POSMaker.clusterDependency(e._2)
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

  def sentenceContainsSimplePRP(doc:Document, ix:Int):Boolean = sentenceContainsSimpleTags(doc, ix, Set("PRP"))

  def sentenceContainsSimplePastTense(doc:Document, ix:Int):Boolean = sentenceContainsSimpleTags(doc, ix, Set("VBD", "VBN"))

  def sentenceContainsSimplePresentTense(doc:Document, ix:Int):Boolean = sentenceContainsSimpleTags(doc, ix, Set("VBG", "VBP", "VBZ"))

  def sentenceContainsSimpleTags(doc:Document, ix:Int, tags:Set[String]):Boolean = {
    val sentence = doc.sentences(ix)
    val tags = sentence.tags.get.toSet
    val evidence:Iterable[Boolean] = tags map {
      tag =>
        tags contains tag
    }

    evidence.exists(identity)
  }

  def eventSentenceContainsPRP(doc:Document, event:BioEventMention):Boolean = sentenceContainsPRP(doc, event.sentence)
  def contextSentenceContainsPRP(doc:Document, context:BioTextBoundMention):Boolean = sentenceContainsPRP(doc, context.sentence)
  def eventSentenceContainsPastTense(doc:Document, event:BioEventMention):Boolean = sentenceContainsSimplePastTense(doc, event.sentence)
  def contextSentenceContainsPastTense(doc:Document, context:BioTextBoundMention):Boolean = sentenceContainsSimplePastTense(doc, context.sentence)
  def eventSentenceContainsPresentTense(doc:Document, event:BioEventMention):Boolean = sentenceContainsSimplePresentTense(doc, event.sentence)
  def contextSentenceContainsPresentTense(doc:Document, context:BioTextBoundMention):Boolean = sentenceContainsSimplePresentTense(doc, context.sentence)
  def isItClosestContextOfSameCategory(event:BioEventMention,
                                       context:BioTextBoundMention,
                                       otherContexts:Iterable[BioTextBoundMention]):Boolean = {
    val bounds = Seq(event.sentence, context.sentence)
    val (start, end) = (bounds.min, bounds.max)


    val filteredContexts = otherContexts.filter{
      c =>
        (!(c.sentence == context.sentence &&  c.tokenInterval == context.tokenInterval && c.nsId() == context.nsId()))

    }
    assert(filteredContexts.size == otherContexts.size -1)
    val interval = start to end

    if(interval.length >= 3){
      val contextCategory = context.nsId().split(":")(0)

      val contextClasses = filteredContexts.collect{
        case c if interval.contains(c.sentence) =>
          c.nsId().split(":")(0)
      }.toList.toSet

      val ret = !contextClasses.contains(contextCategory)

      ret
    }
    else
      true

  }

  // ****** ending utility functions to calculate values of dependency features **********


  // this function calculates the value of the context_frequency feature.
  // it takes as input the context mentions in the paper and counts the freequency of occurrence of each context label.
  // this is then returned as a map of(context_label -> frequnecy)
  //
  def calculateContextFreq(ctxMentions: Seq[BioTextBoundMention]):collection.mutable.Map[String, Double] = {
    val contextFrequencyMap = collection.mutable.Map[String, Double]()
    ctxMentions.map(f => {
      val id = f.nsId()
      if(contextFrequencyMap.contains(id)) {
        val get = contextFrequencyMap(id)
        contextFrequencyMap(id) = get + 1
      }

      else {
        val newEntry = Map(id -> 1.0)
        contextFrequencyMap ++= newEntry
      }
    })
    contextFrequencyMap
  }
}

