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

import scala.io.Source
import org.json4s.jackson.Serialization

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
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(mentionsUnlabeled).slice(0, 1) // TODO: use only one classifier for now.
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
  * Load the trained feature-based classifier and evaluate it on the unlabeled data. Then save the output.
  *
  * Contribution by Zhengzhong
  */
object EvalUnlabeledEventPairsFeatureClassifier extends App with LazyLogging {
  // 1, load the train/test splits:
  //val splitsJson = parse(new File("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/event_pairs_splits.json"))
  val splitsJson = parse(new File("/work/zhengzhongliang/2020_ASKE/neural_baseline/ASKE_2020_CausalDetection/Experiments/event_pairs_splits.json"))
  val allSplits = splitsJson.extract[Map[String, Map[String, Seq[Int]]]]


  // 2, load all labeled event pairs
  // note that the constraint of the sentence distance should be imposed, and this constraint should be the same as
  // implemented in the python file.
  val epsLabeled = (Corpus("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/train").instances ++
    Corpus("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/test").instances)
    .filter(x => ((x.e2.sentence - x.e1.sentence ==1) || (x.e2.sentence == x.e1.sentence && x.e1.start <= x.e2.start)))
  // This filter should be consistent with the python script.

  logger.info(s"total number of labeled event pairs loaded:${epsLabeled.length}")
  logger.info(s"total number of labeled event pairs in the split:${allSplits("split0")("train").length + allSplits("split0")("dev").length + allSplits("split0")("test").length}")
  require(epsLabeled.length == allSplits("split0")("train").length + allSplits("split0")("dev").length + allSplits("split0")("test").length)


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
    val train_index = allSplits("split"+split.toString)("train") ++ allSplits("split"+split.toString)("dev")
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
    val allScores = new ArrayBuffer[Seq[(String, Double)]]()
    val allPreds = new ArrayBuffer[Int]()
    for (ep <- epsUnlabeled){
      val datum = AssemblyRelationClassifier.mkRVFDatum("placeholder", ep.e1, ep.e2)
      val predScore = classifier.scoresOf(datum)
      // Not sure the format of the returned scores. But roughly in the form [label1:score1, label2:score2, label3:score]
      // The sum of scores across all classes is 1, so sorting the score of None in the descending order should work.
      val predLabel = predScore.argMax._1

      if (predLabel == "E1 precedes E2") {allPreds.append(1)}
      else if (predLabel == "E2 precedes E1") {allPreds.append(2)}
      else {allPreds.append(0)}

      allScores.append(predScore.toSeq)

    }

    //TODO: for debugging only. Comment this out later.
//    printEpsByConfidenceScore(allScores)
//    scala.io.StdIn.readLine("waiting for the next split ...")


    // 4, saved the prediction as text file.

    // TODO: comment this temporarily for debugging. Uncomment this later.
    val predLabelsSeq2Str = allPreds.map{x => x.toString}.mkString("; ")
    val predLabelSavePath = "/work/zhengzhongliang/2020_ASKE/20210220/unlabeled_extraction_model_" + model + "_"+split+".txt"

    val pw = new PrintWriter(new File(predLabelSavePath))
    pw.write(predLabelsSeq2Str)
    pw.close

    // This is used to check the predictions of SVM on the unlabeled data.
    // This function prints the
    def printEpsByConfidenceScore(allScores: ArrayBuffer[Seq[(String, Double)]]):Unit = {
      val epsAndScores = epsUnlabeled zip allScores
      val epsAndScoresSorted = epsAndScores.sortBy(_._2(2)._2)

      for (idx<- epsAndScoresSorted.indices) {
        val epScorePair = epsAndScoresSorted(idx)
        if (idx < 300){
          println("-"*40)
          println("e1:",epScorePair._1.e1.text, "\n")
          println("e2:",epScorePair._1.e2.text, "\n")
          println(epScorePair._2, "\n")
        }

      }

    }

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

/**
  * This is the entry for the co-training of svm and lstm/bert.
  * This function should be called from a bash script.
  *
  */
object svmCoTraining extends LazyLogging {

  // This only handles one epoch and one split.
  def main(args:Array[String]): Unit ={

    val splitNumStr = args(0)
    val epochNumStr = args(1)

    val splitNum = splitNumStr.toInt
    val epochNum = epochNumStr.toInt
    assert(Seq(0,1,2,3,4).contains(splitNum))

    // 1, load the train/test splits:
    //val splitsJson = parse(new File("/work/zhengzhongliang/2020_ASKE/neural_baseline/ASKE_2020_CausalDetection/Experiments/event_pairs_splits.json"))
    val splitsJson = parse(new File("/home/zhengzhongliang/CLU_Projects/2020_ASKE/ASKE_2020_CausalDetection/Experiments/event_pairs_splits.json"))
    val allSplits = splitsJson.extract[Map[String, Map[String, Seq[Int]]]]

    // 2, load the labeled data
//    val epsLabeledAllSplits = (Corpus("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/train").instances ++
//      Corpus("/work/zhengzhongliang/2020_ASKE/20200831/mcc_new/test").instances)
//      .filter(x => ((x.e2.sentence - x.e1.sentence ==1) || (x.e2.sentence == x.e1.sentence && x.e1.start <= x.e2.start)))
    val epsLabeledAllSplits = (Corpus("/home/zhengzhongliang/CLU_Projects/2020_ASKE/20200831/mcc_new/train").instances ++
              Corpus("/home/zhengzhongliang/CLU_Projects/2020_ASKE/20200831/mcc_new/test").instances)
              .filter(x => ((x.e2.sentence - x.e1.sentence ==1) || (x.e2.sentence == x.e1.sentence && x.e1.start <= x.e2.start)))
    // This filter should be consistent with the python script.

    // 3, load the predictions of lstm/bert on labeled and unlabeled data (but not the test data)
    // We can assume the predictions to be in the (index, label) format. Only label 1 and 2 are saved.
    // The folder and file name: in the ASKE neural baseline folder.
    // The prediction file name: model name + teacher name + number of unlabeled data + split + epoch + random seed
    // The format of the file: index,label;index,label;....

    //    val neuralModelOutputFolderPath = "/work/zhengzhongliang/2020_ASKE/neural_baseline/ASKE_2020_CausalDetection/Experiments/saved_models/20210315/"
    //    val predFileName = "LSTM1Seg_svm_1700_split_"+splitNumStr+"_epoch_"+epochNumStr+"_seed_0.txt"

    val neuralModelOutputFolderPath = "/home/zhengzhongliang/CLU_Projects/2020_ASKE/ASKE_2020_CausalDetection/Experiments/saved_models/20210319_cotraining/"
    val predFileName = "unlabeled_preds_LSTM1Seg_svm_split_" + splitNumStr + ".json"

    val neuralModelPredsSeq = {
      if (epochNum!=0){
        val neuralResultsJson = parse(new File(neuralModelOutputFolderPath + predFileName))
        val neuralResultsMap = neuralResultsJson.extract[Map[String, JValue]]

        neuralResultsMap("unlabeled_preds").extract[Seq[Int]]
      }
      else {Seq.empty[Int]}
    }

    // 4, load the unlabeled data and assign labels to them. The labels are from the trained LSTM model.
    val totalChunkNum = 5
    val chunkSize = 1000
    val epsUnlabeled = new ArrayBuffer[EventPair]()

    val labelNumStringMap = Map(0 -> "None", 1 -> "E1 precedes E2", 2 -> "E2 precedes E1")

    var epUnlabeledIdx = 0
    for (chunkNum <- 0 until totalChunkNum){
      //val folderPath = "/work/zhengzhongliang/2020_ASKE/20210117/paper_"+(chunkNum*chunkSize).toString+"_"+((chunkNum+1)*chunkSize).toString+"/"
      val folderPath = "/home/zhengzhongliang/CLU_Projects/2020_ASKE/aske_exts_20210117/20210117/paper_"+(chunkNum*chunkSize).toString+"_"+((chunkNum+1)*chunkSize).toString+"/"
      if (epochNum==0){
        epsUnlabeled.appendAll(Corpus(folderPath).instances)
      }
      else{
        for (epUnlabeledRaw <- Corpus(folderPath).instances) {

          val pseudoLabel = neuralModelPredsSeq(epUnlabeledIdx)
          require(Seq(0, 1, 2).contains(pseudoLabel))

          val epWithPseudoLabel = epUnlabeledRaw.copy(relation =
            labelNumStringMap(pseudoLabel))

          epsUnlabeled.append(epWithPseudoLabel)

          epUnlabeledIdx += 1
        }
      }

    }

    if (epochNum != 0) {
      require(epsUnlabeled.length == neuralModelPredsSeq.length)
    }

    // 5, train the svm using the predictions of lstm/bert
    val precedenceDatasetTrain = {
      // If it's the first epoch, only use the labeled data for training.
      // Starting from the second epoch, use both the labeled and pseudo labeled data for training.

      // This is to get the train+dev instances for this single split of the labeled data.
      val epsLabeledOneSplit = new ArrayBuffer[EventPair]()
      (allSplits("split"+splitNumStr)("train")++allSplits("split"+splitNumStr)("dev")).foreach{x => epsLabeledOneSplit.append(epsLabeledAllSplits(x))}
      if (epochNum==0){
        AssemblyRelationClassifier.mkRVFDataset(CorpusReader.filterRelations(epsLabeledOneSplit, precedenceRelations))
      }
      else {
        AssemblyRelationClassifier.mkRVFDataset(CorpusReader.filterRelations(epsLabeledOneSplit++epsUnlabeled, precedenceRelations))
      }
    }

    val model = "lin-svm-l1"
    val classifier = AssemblyRelationClassifier.getModel(model)
    AssemblyRelationClassifier.train(precedenceDatasetTrain, classifier)


    // 6, run the svm on those unlabeled data
    logger.info("starting running trained svm on the unlabeled data. ")

    val allScores = new ArrayBuffer[Seq[(String, Double)]]()
    val allPreds = new ArrayBuffer[Int]()
    for (ep <- epsUnlabeled){
      val datum = AssemblyRelationClassifier.mkRVFDatum("placeholder", ep.e1, ep.e2)
      val predScore = classifier.scoresOf(datum)
      // Not sure the format of the returned scores. But roughly in the form [label1:score1, label2:score2, label3:score]
      // The sum of scores across all classes is 1, so sorting the score of None in the descending order should work.
      val predLabel = predScore.argMax._1

      if (predLabel == "E1 precedes E2") {allPreds.append(1)}
      else if (predLabel == "E2 precedes E1") {allPreds.append(2)}
      else {allPreds.append(0)}

      allScores.append(predScore.toSeq)

    }

    // 7, Evaluate the trained svm on the labeled test partition of this split.
    val epsLabeledTestOneSplit = new ArrayBuffer[EventPair]()
    allSplits("split"+splitNumStr)("test").foreach{x => epsLabeledTestOneSplit.append(epsLabeledAllSplits(x))}

    val precedenceAnnotationsTest = CorpusReader.filterRelations(epsLabeledTestOneSplit, precedenceRelations)
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

    logger.info(s"split:${splitNumStr}, epoch:${epochNumStr}, test p:${precision}, r:${recall}, f1:${f1}")


    // 8, save the output of the svm
    // file name should specify:
    //  (1) split number
    //  (2) epoch number

    // Content that should be saved includes:
    //  (1) the predictions of the unlabeled data
    //  (2) the evaluation results

    val resultSVMJson = Serialization.write(Map("unlabeled_preds" -> allPreds, "test_f1" -> f1))

    val predLabelSavePath = neuralModelOutputFolderPath + "unlabeled_preds_svm_split_" + splitNumStr + ".json"

    val pw = new PrintWriter(new File(predLabelSavePath))
    pw.write(resultSVMJson)
    pw.close

  }



}


object ForAlixEnvironment {

  def main(args:Array[String]) : Unit = {
    println("Alix JDK, scala, sbt all good!")
    println(args(0))

  }
}

object CheckDataMention extends App with LazyLogging {

  val epsLabeledAllSplits = Corpus("/home/zhengzhongliang/CLU_Projects/2020_ASKE/20200831/mcc_new/train").instances ++
    Corpus("/home/zhengzhongliang/CLU_Projects/2020_ASKE/20200831/mcc_new/test").instances

  // It has 945 examples in total, whereas the python script has only 922 examples.
  println("total number of eps:", epsLabeledAllSplits.length)


  /*
  Load the split information. File structure:
  {
    "split_index": [{"train": [], "dev": [], "test": []} ,{...} ,{...} ,{...}, {...}],
    "split_id": [{"train": [], "dev": [], "test": []} ,{...} ,{...} ,{...}, {...}],
  }
   */
  val splitInfoFilePath = "/home/zhengzhongliang/CLU_Projects/2020_ASKE/ASKE_2020_CausalDetection/Experiments2/scala_data/split_info_for_scala.json"
  val splitsJson = parse(new File(splitInfoFilePath))
  val allSplits = splitsJson.extract[Map[String, Seq[Map[String, Seq[Int]]]]]


  val ep_ids_all = scala.collection.mutable.Map[Int, Int]()
  for (ep <- epsLabeledAllSplits) {
    ep_ids_all(ep.id) = 0
  }

  var match_count = 0
  for (split_event_id <- allSplits("split_id")(0)("train") ++ allSplits("split_id")(0)("dev") ++ allSplits("split_id")(0)("test")){
    if (ep_ids_all.contains(split_event_id)){
      match_count += 1
    }
  }

  println("matched event count (should be 922): ", match_count)


}

object EvalRuleModelOnFinalSplit extends App with LazyLogging {
  /*
  This function evaluates the rule-based models on the split we use on 20220126
   */

  // Load the event pairs with the mentions.
  val corpusTrain = Corpus("/home/zhengzhongliang/CLU_Projects/2020_ASKE/20200831/mcc_new/train")
  val corpusTest = Corpus("/home/zhengzhongliang/CLU_Projects/2020_ASKE/20200831/mcc_new/test")
  val allEventPairs = corpusTrain.instances ++ corpusTest.instances
  val allMentions = corpusTrain.mentions ++ corpusTest.mentions


  // Load the split information
  val splitsInfoFilePath = "/home/zhengzhongliang/CLU_Projects/2020_ASKE/ASKE_2020_CausalDetection/Experiments2/scala_data/split_info_for_scala.json"
  val splitsInfoJson = parse(new File(splitsInfoFilePath))
  val splitsInfo = splitsInfoJson.extract[Map[String, Seq[Map[String, Seq[Int]]]]]

  // For the rule-based model, we don't have to run it on 5 seeds. Just get all mentions.
  val eventPairIdsOfInterest = (splitsInfo("split_id")(0)("train") ++ splitsInfo("split_id")(0)("dev") ++ splitsInfo("split_id")(0)("test")).toSet
  val eventPairsOfInterest = allEventPairs.filter{x => eventPairIdsOfInterest.contains(x.id)}
  logger.info(s"total number of event pairs of interest: ${eventPairsOfInterest.length}")

  // Build a reference map so that later when the rule-base classifier outputs the predictions we know where to save the results.
  // Map structure: e1_hash + e2_hash -> ep_hash
  val eventHashesPairToEventPairHashesMap = eventPairsOfInterest.map(ep =>
    (ep.e1.hashCode().toString + "," + ep.e2.hashCode().toString, ep.id)).toMap

  // Start evaluation.
  val resultMap = scala.collection.mutable.Map[String, scala.collection.mutable.Map[Int, Int]]()
  for {
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(allMentions)
  } {
    val predicted = sieveResult.getPrecedenceRelations

    logger.info(s"showing results for classifier ${lbl}. n pred: ${predicted.size}")
    // There are only two precedence classifiers returned from applyEachSieve.
    // The first is combinedRBPrecedence, the second is bioDRBpatterns.

    if (!resultMap.contains(lbl)) {
      resultMap(lbl) = scala.collection.mutable.Map[Int, Int]()  // event pair id -> prediction
    }


    var nInvalidPred = 0

    for (precedRel <- predicted){
      // The event in the prediction can be accessed by: precedRel.before.sourceMention.get.text
      // The event hash can be accessed by: precedRel.before.sourceMention.get.hashCode().toString
      val e1 = precedRel.before.sourceMention.get
      val e2 = precedRel.after.sourceMention.get

      val e1Hash = e1.hashCode().toString
      val e2Hash = e2.hashCode().toString

      if (eventHashesPairToEventPairHashesMap.contains(e1Hash +","+e2Hash)){
        resultMap(lbl)(eventHashesPairToEventPairHashesMap(e1Hash +","+e2Hash)) = 1  // E1 precedes E2
      }

      else if (eventHashesPairToEventPairHashesMap.contains(e2Hash +","+e1Hash)) {
        resultMap(lbl)(eventHashesPairToEventPairHashesMap(e2Hash +","+e1Hash)) = 2   // E2 precedes E1
      }
      else {
        println()
        nInvalidPred += 1
      }

    }
    println(s"rule based classifier name:${lbl}")
    println(resultMap(lbl))
    println(s"invalid event pair count:${nInvalidPred}")

    val saveFolderPath = "/home/zhengzhongliang/CLU_Projects/2020_ASKE/ASKE_2020_CausalDetection/Experiments2/saved_models_scala_20220127/"
    val saveFilePath = saveFolderPath + "rule_model_name_" + lbl + "_pred_all_splits.json"

    val resultMapJson = org.json4s.jackson.Serialization.write(resultMap(lbl))

    val pw = new PrintWriter(new File(saveFilePath))
    pw.write(resultMapJson)
    pw.close
  }

}

/**
  * This function loads the saved splits of the data, then train and evaluate the SVM classifier on the saved splits.
  * This function provides the baseline results of the SVM model under the same setting as in the python file.
  * Latest update: 20220126 using the final splits.
  */


object EvalFeatureClassifierOnSavedLabeledSplits extends App with LazyLogging{
  // 1, load the train/test splits:
  val splitsInfoFilePath = "/home/zhengzhongliang/CLU_Projects/2020_ASKE/ASKE_2020_CausalDetection/Experiments2/scala_data/split_info_for_scala.json"
  val splitsInfoJson = parse(new File(splitsInfoFilePath))
  val splitsInfo = splitsInfoJson.extract[Map[String, Seq[Map[String, Seq[Int]]]]]


  // 2, load all labeled event pairs
  // note that the constraint of the sentence distance should be imposed, and this constraint should be the same as
  // implemented in the python file.
  val corpusTrain = Corpus("/home/zhengzhongliang/CLU_Projects/2020_ASKE/20200831/mcc_new/train")
  val corpusTest = Corpus("/home/zhengzhongliang/CLU_Projects/2020_ASKE/20200831/mcc_new/test")
  val allEventPairs = corpusTrain.instances ++ corpusTest.instances
  val allEventPairsGroupedByEPID = allEventPairs.map{x => (x.id, x)}.toMap

  // 3, train the feature-based classifier on each split and get the prediction.
  val kFolds = 5
  val modelName = "lin-svm-l2"
  val randomSeed:Int = 0  // After experiments, the seed value does not impact the result.

  // Get the results of the dev set.
  val allLabelsDev = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()
  val allPredsDev = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()
  val allEpIdsDev = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()

  // Get the results of the test set
  val allLabels = new ArrayBuffer[Int]()
  val allPreds = new ArrayBuffer[Int]()
  val allEpIds = new ArrayBuffer[Int]()

  for (split <- 0 until kFolds){
    val trainIds = splitsInfo("split_id")(split)("train")
    val devIds = splitsInfo("split_id")(split)("dev")
    val testIds = splitsInfo("split_id")(split)("test")

    val epsTrain = trainIds.map{x => allEventPairsGroupedByEPID(x)} // use train and dev as train.
    val epsDev = devIds.map{x => allEventPairsGroupedByEPID(x)}
    val epsTest = testIds.map{x => allEventPairsGroupedByEPID(x)}

    // Note that the original code uses filterRelations function to remove the invalid event pairs.
    // e.g., CorpusReader.filterRelations(epsTrain, precedenceRelations)
    // But I don't think we should use that for a fair comparison between the neural model against this one.
    // So I don't use it. Instead, I use a new one which keep the bug ones, but set their labels to NEG.
    val precedenceAnnotationsTrain = CorpusReader.filterRelations2(epsTrain, precedenceRelations)
    val precedenceDatasetTrain = AssemblyRelationClassifier.mkRVFDataset(precedenceAnnotationsTrain)
    val precedenceAnnotationsDev = CorpusReader.filterRelations2(epsDev, precedenceRelations)
    val precedenceAnnotationsTest = CorpusReader.filterRelations2(epsTest, precedenceRelations)

    // Print the labels to make sure the data is good.
    val labelCount = scala.collection.mutable.Map[String, Int]()
    (precedenceAnnotationsTrain ++ precedenceAnnotationsDev ++ precedenceAnnotationsTest).foreach{
      x => {
        if (!labelCount.contains(x.relation)) {labelCount(x.relation) = 1}
        else {labelCount(x.relation) += 1}
      }
    }

    println("label count (should have sum 858):", labelCount)

    // 1, train the model
    val classifier = AssemblyRelationClassifier.getModel(modelName)
    AssemblyRelationClassifier.train(precedenceDatasetTrain, classifier)

    // 2, evaluate the model on the dev set.
    allLabelsDev(split) = new ArrayBuffer[Int]()
    allPredsDev(split) = new ArrayBuffer[Int]()
    allEpIdsDev(split) = new ArrayBuffer[Int]()
    for (idx <- precedenceAnnotationsDev.indices) {
      val ep = precedenceAnnotationsDev(idx)
      val label = ep.relation
      val pred = classifier.classOf(AssemblyRelationClassifier.mkRVFDatum(label, ep.e1, ep.e2))

      if (label == "E1 precedes E2"){
        allLabelsDev(split).append(1)
      }
      else if (label == "E2 precedes E1"){
        allLabelsDev(split).append(2)
      }
      else { // None
        allLabelsDev(split).append(0)
      }

      if (pred == "E1 precedes E2"){
        allPredsDev(split).append(1)
      }
      else if (pred == "E2 precedes E1"){
        allPredsDev(split).append(2)
      }
      else {  // None
        allPredsDev(split).append(0)
      }

      allEpIdsDev(split).append(ep.id)
    }

    // 3, evaluate the model and saved the ep ids + labels + predictions
    for (idx <- precedenceAnnotationsTest.indices){
      val ep = precedenceAnnotationsTest(idx)
      val label = ep.relation
      val pred = classifier.classOf(AssemblyRelationClassifier.mkRVFDatum(label, ep.e1, ep.e2))

      if (label == "E1 precedes E2"){
        allLabels.append(1)
      }
      else if (label == "E2 precedes E1"){
        allLabels.append(2)
      }
      else { // None
        allLabels.append(0)
      }

      if (pred == "E1 precedes E2"){
        allPreds.append(1)
      }
      else if (pred == "E2 precedes E1"){
        allPreds.append(2)
      }
      else {  // None
        allPreds.append(0)
      }

      allEpIds.append(ep.id)

    }
  }

  var tp = 0f
  var fp = 0f
  var fn = 0f

  for (idx <- allLabels.indices){
    val pred = allPreds(idx)
    val label = allLabels(idx)

    if (pred != 0 && pred == label){tp +=1}
    if (pred != 0 && pred!=label){fp +=1}
    if (pred == 0 && pred!=label){fn+=1}
  }


  val precision = tp/(tp+fp)
  val recall = tp/(tp+fn)
  val f1 = precision*recall*2/(precision + recall)

  logger.info(s"all splits p:${precision}, r:${recall}, f1:${f1}")
  logger.info(s"num all test samples: ${allEpIds.length}")

  // svm l2: p:0.4848485, r:0.23703703, f1:0.31840792
  // svm l1: p:0.394958, r:0.36434108, f1:0.37903228

  // Save the results:
  val saveFolderPath = "/home/zhengzhongliang/CLU_Projects/2020_ASKE/ASKE_2020_CausalDetection/Experiments2/saved_models_scala_20220127/"
  val saveFilePath = saveFolderPath + "svm_model_name_" + modelName + "_pred_all_splits.json"

  val resultMap = Map(
    "dev" -> Map(
      "all_ids" -> allEpIdsDev,
      "all_preds" -> allPredsDev,
      "all_labels" -> allLabelsDev
    ),
    "test" -> Map(
      "all_ids" -> allEpIds,
      "all_preds" -> allPreds,
      "all_labels" -> allLabels)
  )
  val resultMapJson = org.json4s.jackson.Serialization.write(resultMap)

  val pw = new PrintWriter(new File(saveFilePath))
  pw.write(resultMapJson)
  pw.close
}
