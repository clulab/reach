package org.clulab.reach.assembly

import org.apache.commons.io.FilenameUtils
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}
import org.clulab.odin.Mention
import org.clulab.reach.PaperReader
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json._
import org.clulab.utils.Serializer

import scala.collection.parallel.ForkJoinTaskSupport
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import java.io.{File, PrintWriter}

import org.clulab.reach.assembly.relations.SieveEvaluator
import org.clulab.reach.assembly.relations.SieveEvaluator.Performance
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import org.json4s._
import ai.lum.common.FileUtils._
import java.nio.charset.StandardCharsets.UTF_8

import org.clulab.reach.{ReachSystem, context}
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.assembly.relations.classifier.AssemblyRelationClassifier
import org.clulab.reach.assembly.relations.classifier.CrossValidateAssemblyRelationClassifier.logger
import org.clulab.reach.assembly.sieves.SieveUtils.precedenceRelations

import scala.collection.mutable.ArrayBuffer
import org.clulab.learning.{Classifier, RVFDatum}
import org.clulab.reach.assembly.EvalFeatureClassifierOnLabeledData.allPreds


/**
  * Run the sieves on the event pairs and get the predictions of each sieves.
  * Currently it has only rule-based sieves, but feature-based classifiers can also be included.
  * Contribution by Gus
  */
object RunAnnotationEval extends App with LazyLogging {

  val config = ConfigFactory.load()
//  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDir")).instances

  // gather precedence relations corpus
  val evalGoldPath = config.getString("assembly.evalGold")
  val evalMentionsPath = config.getString("assembly.evalMentions")

  val (posGold, testMentions) = {

    if(new File(evalGoldPath).exists & new File(evalMentionsPath).exists) {
      logger.info("Serialized files exist")
      val pg = Serializer.load[Seq[PrecedenceRelation]](evalGoldPath)
      val tm = Serializer.load[Seq[Mention]](evalMentionsPath)
      (pg, tm)
    } else {
      logger.info("Serialized files not found")
      val epsOld: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirOldEval")).instances
      val newMentions = Corpus.loadMentions(config.getString("assembly.corpus.corpusDirNewEval"))
      val eps = Corpus.softAlign(epsOld, newMentions)
      // gather precedence relations corpus
//      val precedenceAnnotations = CorpusReader.filterRelations(eps, precedenceRelations)
//      val noneAnnotations = CorpusReader.filterRelations(eps, noRelations ++ subsumptionRelations ++ equivalenceRelations)

      val (posGoldNested, testMentionsNested) = (for {
        ep <- eps
        e1 = ep.e1
        e2 = ep.e2
      } yield {
        // short-term assembly manager to get at mentions easier
        val am = AssemblyManager()
        am.trackMentions(Seq(e1, e2))
        val goldRel = ep.relation match {
          case "E1 precedes E2" =>
            Seq(PrecedenceRelation(am.getEER(e1), am.getEER(e2), Set.empty[Mention], "gold"))
          case "E2 precedes E1" =>
            Seq(PrecedenceRelation(am.getEER(e2), am.getEER(e1), Set.empty[Mention], "gold"))
          case _ => Nil
        }
        (goldRel, Seq(e1, e2))
      }).unzip

      val pg = posGoldNested.flatten.seq
      val tm = testMentionsNested.flatten.distinct.seq

      (pg, tm)
    }
  }

  println("="*20)
  println(s"num of labels:${posGold.length}")
  println("="*20)

  println("sieve\trule\tp\tr\tf1\ttp\tfp\tfn")

  for {
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(testMentions)
  } {
    val predicted = sieveResult.getPrecedenceRelations
    val smoothing = 0.00001

    // TODO: check with Gus. The original expression was "posGold exists(...)". Is this equivalent to "posGold.exists(...)"?
    val tp = predicted.count(p => posGold.exists(g => g.isEquivalentTo(p, ignoreMods = false)))
    val fp = predicted.count(p => ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = false)))
    val fn = posGold.count(g => ! predicted.exists(p => p.isEquivalentTo(g, ignoreMods = false)))

    // micro performance
    val p = tp / (tp + fp + smoothing)
    val r = tp / (tp + fn + smoothing)
    val f1 = (2 * p * r) / (p + r + smoothing)

    // for the whole sieve
    val sievePerformance = Performance(lbl, "**ALL**", p, r, f1, tp, fp, fn)

    val rulePerformance: Seq[Performance] = {
      val rulePs = predicted.groupBy(pr => (pr.foundBy, pr.evidence.head.foundBy))
      val allRtp = rulePs.mapValues(_.count(p => posGold exists(g => g.isEquivalentTo(p, ignoreMods = false))))
      val allRfp = rulePs.mapValues{_.count{p =>
        val isFP = ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = false))
        //if(isFP) displayMention(p.evidence.head)
        isFP
      }
      }
      val allRfn = {
        val res = for {
          (foundBy, group) <- rulePs
          gold = posGold.count(g => ! group.exists(p => p.isEquivalentTo(g, ignoreMods = false)))
        } yield (foundBy, gold)
        res
      }

      val rp = for {
        foundBy <- rulePs.keys
      } yield {
        val tp = allRtp.getOrElse(foundBy, 0)
        val fp = allRfp.getOrElse(foundBy, 0)
        val fn = allRfn.getOrElse(foundBy, 0)

        // micro performance
        val p = tp / (tp + fp + smoothing)
        val r = tp / (tp + fn + smoothing)
        val f1 = (2 * p * r) / (p + r + smoothing)

        // for the rule
        Performance (foundBy._1, foundBy._2, p, r, f1, tp, fp, fn)
      }
      rp.toSeq
    }

    (rulePerformance :+ sievePerformance).sortBy(_.p).foreach(perf => println(perf.mkRow))
  }
}

/**
  * This function is used to match the predictions of the rule/feature based classifier to the existing mentions.
  * Because the predictions returned by the classifiers are a bunch of precedence relationships. We need to map each
  * predicted precedence relationship with the original event pair in the event pair list.
  *
  * This is only a test function to verify whether the matching works. This is only a debugging function, not intended
  * to be used in actual matching.
  *
  * Contribution by Zhengzhong
 */
object TestMentionMatch extends App with LazyLogging {
  val evalMentionsPath = "/work/zhengzhongliang/2020_ASKE/20210117/"

  val testCorpus = Corpus(evalMentionsPath)

  println(s"number of event pairs: ${testCorpus.instances.length}")

  val eventPairHashIdxMap = scala.collection.mutable.Map[String, Int]() // get unique event pairs by hash
  val eventPairFeatureIdxMap = scala.collection.mutable.Map[String, Int]() // get unique event pairs by features
  val mentionHashIdxMap = scala.collection.mutable.Map[String, Int]() // get unique mentions by hash
  val mentionFeatureIdxMap = scala.collection.mutable.Map[String, Int]() // get unique mentions by features

  for (idx <- testCorpus.instances.indices){
    val ep = testCorpus.instances(idx)

    val e1Hash = ep.e1.hashCode().toString
    val e2Hash = ep.e2.hashCode().toString

    val e1Features = ep.e1.document.id.getOrElse("") + "," + ep.e1.sentence.toString + "," + ep.e1.start.toString + "," + ep.e1.end.toString + "," + ep.e1.label
    val e2Features = ep.e2.document.id.getOrElse("") + "," + ep.e2.sentence.toString + "," + ep.e2.start.toString + "," + ep.e2.end.toString + "," + ep.e2.label

    if (eventPairHashIdxMap.contains(e1Hash+ "," + e2Hash)) {eventPairHashIdxMap(e1Hash+ "," + e2Hash) += 1}
    else {eventPairHashIdxMap(e1Hash+ "," + e2Hash) = 1}

    if (eventPairFeatureIdxMap.contains(e1Features+";"+e2Features)) {eventPairFeatureIdxMap(e1Features+";"+e2Features) += 1}
    else {eventPairFeatureIdxMap(e1Features+";"+e2Features) = 1}

    if (mentionHashIdxMap.contains(e1Hash)) {mentionHashIdxMap(e1Hash) +=1 } else {mentionHashIdxMap(e1Hash) = 1}
    if (mentionHashIdxMap.contains(e2Hash)) {mentionHashIdxMap(e2Hash) +=1 } else {mentionHashIdxMap(e2Hash) = 1}

    if (mentionFeatureIdxMap.contains(e1Features)) {mentionFeatureIdxMap(e1Features) += 1} else {mentionFeatureIdxMap(e1Features) = 1}
    if (mentionFeatureIdxMap.contains(e2Features)) {mentionFeatureIdxMap(e2Features) += 1} else {mentionFeatureIdxMap(e2Features) = 1}

  }

  // check if there are repeated event pairs, identified by mention hash, proved to have no repeated event pairs.
  println(eventPairHashIdxMap.filter{x => x._2>1})

  // check if there are repeated event pairs, identified by mention features, proved to have some repeated event pairs.
  println(eventPairFeatureIdxMap.filter{x => x._2>1})

  for {
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(testCorpus.mentions)
  } {
    val predicted = sieveResult.getPrecedenceRelations
    val fullPredLabelsListToSave = ArrayBuffer[(Int, Int)]()

    var invalidMentionHashCount  = 0
    var invalidMentionFeatureCount = 0

    for (precedRel <- predicted){
      // The event in the prediction can be accessed by: precedRel.before.sourceMention.get.text
      // The event hash can be accessed by: precedRel.before.sourceMention.get.hashCode().toString
      val e1 = precedRel.before.sourceMention.get
      val e2 = precedRel.after.sourceMention.get
      val e1Features = e1.document.id.getOrElse("") + "," + e1.sentence.toString + "," + e1.start.toString + "," + e1.end.toString + "," + e1.label
      val e2Features = e2.document.id.getOrElse("") + "," + e2.sentence.toString + "," + e2.start.toString + "," + e2.end.toString + "," + e2.label

      val e1Hash = e1.hashCode().toString
      val e2Hash = e2.hashCode().toString

      if (!mentionHashIdxMap.contains(e1Hash)) {invalidMentionHashCount += 1}
      if (!mentionHashIdxMap.contains(e2Hash)) {invalidMentionHashCount += 1}
      if (!mentionFeatureIdxMap.contains(e1Features)) {invalidMentionFeatureCount += 1}
      if (!mentionFeatureIdxMap.contains(e2Features)) {invalidMentionFeatureCount += 1}

    }
    println(fullPredLabelsListToSave)
    println(s"invalid mention hash count ${invalidMentionHashCount}, invalid mention feature count:${invalidMentionFeatureCount}")
    scala.io.StdIn.readLine("-"*40)

    // It turns out that for both methods, there are no invalid mentions

  }
}


/**
  * Run the rule-based classfier(s) on the unlabeled event pairs, then save the predictions.
  *
  * Contribution by Zhengzhong
  */
object EvalUnlabeledEventPairsRuleClassifier extends App with LazyLogging {

  val totalChunkNum = 7
  val chunkSize = 1000
  val epsUnlabeled = new ArrayBuffer[EventPair]()
  val mentionsUnlabeled = new ArrayBuffer[CorefMention]()

  for (chunkNum <- 0 until totalChunkNum){
    val folderPath = "/work/zhengzhongliang/2020_ASKE/20210117/paper_"+(chunkNum*chunkSize).toString+"_"+((chunkNum+1)*chunkSize).toString+"/"
    val corpus = Corpus(folderPath)
    epsUnlabeled.appendAll(corpus.instances)
    mentionsUnlabeled.appendAll(corpus.mentions)
  }

  logger.info(s"total number of unlabeled event pairs loaded:${epsUnlabeled.length}")
  logger.info(s"total number of mentions loaded: ${mentionsUnlabeled.length}")

  // building event pair "event hash to index"
  val eventPairHashIdxMap = scala.collection.mutable.Map[String, Int]()
  val mentionHashIdxMap = scala.collection.mutable.Map[String, Int]()

  for (idx <- epsUnlabeled.indices){
    val ep = epsUnlabeled(idx)

    val e1Hash = ep.e1.hashCode().toString
    val e2Hash = ep.e2.hashCode().toString

    if (eventPairHashIdxMap.contains(e1Hash+ "," + e2Hash)) {println("repeated event pair identified by hash, we should not be here!")}
    else {eventPairHashIdxMap(e1Hash+ "," + e2Hash) = idx}

    if (mentionHashIdxMap.contains(e1Hash)) {mentionHashIdxMap(e1Hash) +=1 } else {mentionHashIdxMap(e1Hash) = 1}
    if (mentionHashIdxMap.contains(e2Hash)) {mentionHashIdxMap(e2Hash) +=1 } else {mentionHashIdxMap(e2Hash) = 1}

  }

  for {
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(mentionsUnlabeled).slice(0,1 ) // TODO: use only one classifier for now.
  } {
    logger.info(s"showing results for classifier ${lbl}.")
    // There are only two precedence classifiers returned from applyEachSieve.
    // The first is combinedRBPprecedence, the second is bioDRBpatterns.

    val predicted = sieveResult.getPrecedenceRelations
    val fullPredLabelsListToSave = ArrayBuffer[(Int, Int)]()

    var invalidMentionHashCount  = 0
    var invalidEventPairHashCount = 0

    for (precedRel <- predicted){
      // The event in the prediction can be accessed by: precedRel.before.sourceMention.get.text
      // The event hash can be accessed by: precedRel.before.sourceMention.get.hashCode().toString
      val e1 = precedRel.before.sourceMention.get
      val e2 = precedRel.after.sourceMention.get

      val e1Hash = e1.hashCode().toString
      val e2Hash = e2.hashCode().toString

      if (eventPairHashIdxMap.contains(e1Hash +","+e2Hash)){
        fullPredLabelsListToSave.append((eventPairHashIdxMap(e1Hash +","+e2Hash), 1))  // E1 precedes E2
      }

      else if (eventPairHashIdxMap.contains(e2Hash +","+e1Hash)) {
        fullPredLabelsListToSave.append((eventPairHashIdxMap(e2Hash +","+e1Hash), 2))   // E2 precedes E1

      }
      else {
        invalidEventPairHashCount += 1
      }

      if (!mentionHashIdxMap.contains(e1Hash)) {invalidMentionHashCount += 1}
      if (!mentionHashIdxMap.contains(e2Hash)) {invalidMentionHashCount += 1}

    }
    println(s"rule based classifier name:${lbl}")
    println(fullPredLabelsListToSave)
    println(s"invalid mention hash count ${invalidMentionHashCount}, invalid event pair count:${invalidEventPairHashCount}")


    val predLabelsSeq2Str = fullPredLabelsListToSave.map{x => x._1 + "," + x._2}.mkString(";")
    val predLabelSavePath ="/work/zhengzhongliang/2020_ASKE/20210220/unlabeled_extraction_model_rule.txt"

    val pw = new PrintWriter(new File(predLabelSavePath))
    pw.write(predLabelsSeq2Str)
    pw.close

    println(s"labels saved for classifier ${lbl} to ${predLabelSavePath}!")

    // Tuple to match: paper id, sentence id, text span. label
    // TODO, print the mention's hash, see if new mentions are predicted (not new event pairs)
  }


}

/**
  * This function loads a trained feature-based classifier and evaluate on the labeled evaluation set.
  * It reports the precision, recall and F1 score. This is used for testing whether the feature-based classifier works correctly.
  * k-fold cross validation is not implemented in this function.
  *
  * Contribution: Zhengzhong
  */
object EvalFeatureClassifierOnLabeledData extends App with LazyLogging {
  val config = ConfigFactory.load()
  val classifierPath = config.getString("assembly.classifier.model")
  val results = config.getString("assembly.classifier.results")
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirNewEval")).instances

  logger.info(s"number of loaded raw event pairs: ${eps.length}")

  // gather precedence relations corpus
  val precedenceAnnotations = CorpusReader.filterRelations(eps, precedenceRelations)
  // train
  logger.info(s"number of loaded pairs after filtering: ${precedenceAnnotations.size}")
  val precedenceDataset = AssemblyRelationClassifier.mkRVFDataset(precedenceAnnotations)

  val model = "lin-svm-l1"

  //val classifier:Classifier[String, String] = AssemblyRelationClassifier.loadFrom(classifierPath).classifier
  val classifier = AssemblyRelationClassifier.loadFrom(classifierPath)

  println(s"classifier class name:${classifier.getClass.getName}")

  val allPreds = new ArrayBuffer[String]()
  for (i <- precedenceAnnotations.indices) {
    val dataPoint = precedenceDataset.mkDatum(i)
    // TODO: print the features used here.
    val predicted = classifier.classify(dataPoint.asInstanceOf[RVFDatum[String, String]])
    //println(s"label pair predicted: ${predicted}, ${predicted=="None"}")

    allPreds.append(predicted)

  }

  require(allPreds.length == precedenceAnnotations.length)

  var tp = 0f
  var fp = 0f
  var fn = 0f
  for (idx <- precedenceAnnotations.indices){
    val label = precedenceAnnotations(idx).relation
    println(s"predict:${allPreds(idx)}, label:${label}, equal?${allPreds(idx)==label}")
    if (allPreds(idx) != "None" && allPreds(idx)==label ){tp +=1}
    if (allPreds(idx)!="None" && allPreds(idx)!=label ){fp +=1}
    if (allPreds(idx)=="None" && allPreds(idx)!=label) {fn+=1}
  }
  val precision = tp/(tp+fp)
  val recall = tp/(tp+fn)
  val f1 = precision*recall*2/(precision + recall)

  println(s"precision${precision}, recall:${recall}, f1:${f1}")

}

/**
  * This function loads the saved splits of the data, then train and evaluate the SVM classifier on the saved splits.
  * This function provides the baseline results of the SVM model under the same setting as in the python file.
  */
object EvalFeatureClassifierOnSavedLabeledSplits extends App with LazyLogging{
  // 1, load the train/test splits:
  val splitsJson = parse(new File("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/event_pairs_splits.json"))
  val allSplits = splitsJson.extract[Map[String, Map[String, Seq[Int]]]]


  // 2, load all labeled event pairs
  // note that the constraint of the sentence distance should be imposed, and this constraint should be the same as
  // implemented in the python file.
  val epsLabeled = (Corpus("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/train").instances ++
    Corpus("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/test").instances)
    .filter(x => (x.e2.sentence - x.e1.sentence).abs <= 1)

  logger.info(s"total number of labeled event pairs loaded:${epsLabeled.length}")
  logger.info(s"total number of labeled event pairs in the split:${allSplits("split0")("train").length + allSplits("split0")("test").length}")
  require(epsLabeled.length == allSplits("split0")("train").length + allSplits("split0")("test").length)


  // 3, train the feature-based classifier on each split and get the prediction.
  val kFolds = 5
  val model = "lin-svm-l1"
  val randomSeed:Int = 42

  val allLabels = new ArrayBuffer[String]()
  val allPreds = new ArrayBuffer[String]()

  for (split <- 0 until kFolds){
    val train_index = allSplits("split"+split.toString)("train")
    val test_index = allSplits("split"+split.toString)("test")

    val epsTrain = for {idx <- train_index} yield epsLabeled(idx)
    val epsTest = for {idx <- test_index} yield epsLabeled(idx)


    val precedenceDatasetTrain = AssemblyRelationClassifier.mkRVFDataset(CorpusReader.filterRelations(epsTrain, precedenceRelations))
    val precedenceAnnotationsTest = CorpusReader.filterRelations(epsTest, precedenceRelations)

    // 1, train the model
    val classifier = AssemblyRelationClassifier.getModel(model)

    AssemblyRelationClassifier.train(precedenceDatasetTrain, classifier)


    // 2, evaluate the model and saved the labels + predictions
    for (idx <- precedenceAnnotationsTest.indices){
      val ep = precedenceAnnotationsTest(idx)
      val label = ep.relation
      val pred = classifier.classOf(AssemblyRelationClassifier.mkRVFDatum(label, ep.e1, ep.e2))

      allLabels.append(label)
      allPreds.append(pred)

    }
  }

  var tp = 0f
  var fp = 0f
  var fn = 0f

  for (idx <- allLabels.indices){
    val pred = allPreds(idx)
    val label = allLabels(idx)

    if (pred != "None" && pred == label){tp +=1}
    if (pred !="None" && pred!=label){fp +=1}
    if (pred =="None" && pred!=label) {fn+=1}
  }


  val precision = tp/(tp+fp)
  val recall = tp/(tp+fn)
  val f1 = precision*recall*2/(precision + recall)

  logger.info(s"all splits p:${precision}, r:${recall}, f1:${f1}")
}


/**
  * Load the trained feature-based classifier and evaluate it on the unlabeled data. Then save the output.
  *
  * Contribution by Zhengzhong
  */
object EvalUnlabeledEventPairsFeatureClassifier extends App with LazyLogging {
  // 1, load the train/test splits:
  val splitsJson = parse(new File("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/event_pairs_splits.json"))
  val allSplits = splitsJson.extract[Map[String, Map[String, Seq[Int]]]]


  // 2, load all labeled event pairs
  // note that the constraint of the sentence distance should be imposed, and this constraint should be the same as
  // implemented in the python file.
  val epsLabeled = (Corpus("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/train").instances ++
    Corpus("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/test").instances)
    .filter(x => (x.e2.sentence - x.e1.sentence).abs <= 1)

  logger.info(s"total number of labeled event pairs loaded:${epsLabeled.length}")
  logger.info(s"total number of labeled event pairs in the split:${allSplits("split0")("train").length + allSplits("split0")("test").length}")
  require(epsLabeled.length == allSplits("split0")("train").length + allSplits("split0")("test").length)


  // 3, load all unlabeled event pairs:
  val totalChunkNum = 7
  val chunkSize = 1000
  val epsUnlabeled = new ArrayBuffer[EventPair]()

  for (chunkNum <- 0 until totalChunkNum){
    val folderPath = "/work/zhengzhongliang/2020_ASKE/20210117/paper_"+(chunkNum*chunkSize).toString+"_"+((chunkNum+1)*chunkSize).toString+"/"
    epsUnlabeled.appendAll(Corpus(folderPath).instances)
  }

  logger.info(s"total number of unlabeled event pairs loaded:${epsUnlabeled.length}")

  // 4, train the feature-based classifier on each split and annotate the unlabeled data.
  val kFolds = 5
  val model = "lin-svm-l1"
  val randomSeed:Int = 42
  for (split <- 0 until kFolds){
    val train_index = allSplits("split"+split.toString)("train")
    val test_index = allSplits("split"+split.toString)("test")

    val epsTrain = for {idx <- train_index} yield epsLabeled(idx)
    val epsTest = for {idx <- test_index} yield epsLabeled(idx)


    val precedenceDatasetTrain = AssemblyRelationClassifier.mkRVFDataset(CorpusReader.filterRelations(epsTrain, precedenceRelations))
    val precedenceAnnotationsTest = CorpusReader.filterRelations(epsTest, precedenceRelations)

    // 1, train the model
    val classifier = AssemblyRelationClassifier.getModel(model)

    AssemblyRelationClassifier.train(precedenceDatasetTrain, classifier)


    // 2, evaluate the model
    var tp = 0f
    var fp = 0f
    var fn = 0f
    for (idx <- precedenceAnnotationsTest.indices){
      val ep = precedenceAnnotationsTest(idx)
      val label = ep.relation
      val pred = classifier.classOf(AssemblyRelationClassifier.mkRVFDatum(label, ep.e1, ep.e2))

      if (pred != "None" && pred == label){tp +=1}
      if (pred !="None" && pred!=label){fp +=1}
      if (pred =="None" && pred!=label) {fn+=1}
    }
    val precision = tp/(tp+fp)
    val recall = tp/(tp+fn)
    val f1 = precision*recall*2/(precision + recall)

    logger.info(s"split:${split}, p:${precision}, r:${recall}, f1:${f1}")

    // 3, do the prediction on the unlabeled data.
    val allScores = new ArrayBuffer[Int]()
    val allPreds = new ArrayBuffer[Int]()
    for (ep <- epsUnlabeled){
      println("evaluating each unlabeled data")
      val datum = AssemblyRelationClassifier.mkRVFDatum("placeholder", ep.e1, ep.e2)
      val predScore = classifier.scoresOf(datum)
      val predLabel = predScore.argMax._1

      if (predLabel == "E1 precedes E2") {allPreds.append(1)}
      else if (predLabel == "E2 precedes E1") {allPreds.append(2)}
      else {allPreds.append(0)}

      println("-"*40)
      println(predScore)
      println(predLabel)

    }

    //println(predS)


    // 4, saved the prediction as text file.

    // TODO: comment this temporarily for debugging. Uncomment this later.
//    val predLabelsSeq2Str = allPreds.map{x => x.toString}.mkString("; ")
//    val predLabelSavePath = "/work/zhengzhongliang/2020_ASKE/20210220/unlabeled_extraction_model_" + model + "_"+split+".txt"
//
//    val pw = new PrintWriter(new File(predLabelSavePath))
//    pw.write(predLabelsSeq2Str)
//    pw.close

    //def printEpsByConfidenceScore

  }
}

/**
  * Serialize each paper in a directory to json
  *
  * Contribution by Gus.
  */
object SerializePapersToJSON extends App with LazyLogging {

  import org.clulab.reach.mentions.serialization.json._

  val config = ConfigFactory.load()
  val papersDir = new File(config.getString("papersDir"))
  val outDir = new File(config.getString("outDir"))
  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")

  logger.info(s"papersDir: ${papersDir.getAbsolutePath}")
  logger.info(s"outDir: ${outDir.getAbsolutePath}")
  logger.info(s"threads: $threadLimit")
  val papers = papersDir.listFiles.par
  papers.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))

  for {
    paper <- papers
    paperID = FilenameUtils.removeExtension(paper.getName)
    outFile = new File(s"$outDir/$paperID.json")
    if !outFile.exists
  } {
    val mentions = PaperReader.getMentionsFromPaper(paper)
    val cms: Seq[CorefMention] = mentions.map(_.toCorefMention)
    logger.info(s"extracted ${mentions.size} mentions for $paperID")
    cms.saveJSON(outFile, pretty = true)
    logger.info(s"saved json to $outFile")
  }
}

