package org.clulab.reach.assembly.relations.classifier

import org.clulab.reach.assembly.relations.corpus.CorpusReader._
import org.clulab.reach.assembly.relations.corpus.{CorpusReader, EventPair}
import org.clulab.learning._
import ai.lum.common.FileUtils._
import org.apache.commons.io.FilenameUtils
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import com.typesafe.scalalogging.LazyLogging
import com.typesafe.config.ConfigFactory
import java.io.File


/** Used by Stratified K-fold CV */
case class DatasetStratifiedFold(test: Seq[Int], train: Seq[Int]) {
  def merge(other: DatasetStratifiedFold): DatasetStratifiedFold = {
    new DatasetStratifiedFold(this.test ++ other.test, this.train ++ other.train)
  }
}

case class Performance[L] (lbl: L, p: Double, r: Double, f1: Double, tp: Int, fp: Int, fn: Int) {
  def mkRow = f"$lbl\t$p%1.3f\t$r%1.3f\t$f1%1.3f\t$tp\t$fp\t$fn"
}

object Evaluator {

  def crossValidate(dataset: RVFDataset[String, String], clfType: String): Seq[(String, String)] = {
    Datasets.crossValidate[String, String](
      dataset,
      () => AssemblyRelationClassifier.getModel(clfType),
      numFolds = 20
    ).toSeq
  }

  /** Creates dataset folds to be used for cross validation */
  def mkStratifiedFolds[L, F](
    numFolds:Int,
    dataset:Dataset[L, F],
    seed:Int
  ):Iterable[DatasetStratifiedFold] = {
    val r = new Random(seed)

    val byClass: Map[Int, Seq[Int]] = r.shuffle[Int, IndexedSeq](dataset.indices).groupBy(idx => dataset.labels(idx))
    val folds = (for (i <- 0 until numFolds) yield (i, new ArrayBuffer[DatasetStratifiedFold])).toMap

    for {
      c <- 0 until dataset.numLabels
      i <- 0 until numFolds
    } {
      val cds = byClass(c)
      val classSize = cds.length
      val foldSize = classSize / numFolds
      val startTest = i * foldSize
      val endTest = if (i == numFolds - 1) math.max(classSize, (i + 1) * foldSize) else (i + 1) * foldSize

      val trainFolds = new ArrayBuffer[Int]
      if(startTest > 0)
        trainFolds ++= cds.slice(0, startTest)
      if(endTest < classSize)
        trainFolds ++= cds.slice(endTest, classSize)

      folds(i) += new DatasetStratifiedFold(cds.slice(startTest, endTest), trainFolds)
    }
    folds.map{dsfSet =>
      dsfSet._2.foldLeft(new DatasetStratifiedFold(Nil, Nil))(_ merge _)}
  }

  /**
    * Implements stratified cross validation; producing pairs of gold/predicted labels across the training dataset.
    * Each fold is as balanced as possible by label L.
    */
  def stratifiedCrossValidate[L, F](
    dataset:Dataset[L, F],
    classifierFactory: () => Classifier[L, F],
    numFolds:Int = 5,
    seed:Int = 73
  ): Seq[(L, L)] = {

    val folds = mkStratifiedFolds(numFolds, dataset, seed)
    val output = new ListBuffer[(L, L)]

    for(fold <- folds) {
      // Uncomment to confirm the size of each class in each fold
      // val balance = fold.test.map(dataset.labels(_)).groupBy(identity).mapValues(_.size)
      // logger.debug(s"fold: ${balance.mkString(", ")}")
      val classifier = classifierFactory()
      classifier.train(dataset, fold.train.toArray)
      for(i <- fold.test) {
        val sys = classifier.classOf(dataset.mkDatum(i))
        val gold = dataset.labels(i)
        output += new Tuple2(dataset.labelLexicon.get(gold), sys)
      }
    }

    output
  }

  def calculateAccuracy[L](scores: Seq[(L, L)]): Float = {
    scores.count(pair => pair._1 == pair._2).toFloat / scores.size.toFloat
  }

  /**
    * Calculate precision, recall, and f1 for each label base on scores of form (gold, predicted)
    *
    * @param scores
    * @tparam L
    * @return [[Map]] from label to [[Performance]]
    */
  def calculatePerformance[L](scores: Seq[(L, L)]): Seq[Performance[L]] = {

    val smoothing = 0.00001

    for {
      lbl <- scores.map(_._1).distinct
    } yield {
      val tp = scores.count(score => score._1 == lbl && score._2 == lbl)
      val fp = scores.count(score => score._1 != lbl && score._2 == lbl)
      val fn = scores.count(score => score._1 == lbl && score._2 != lbl)

      // micro performance
      val p = tp / (tp + fp + smoothing)
      val r = tp / (tp + fn + smoothing)
      val f1 = (2 * p * r) / (p + r + smoothing)

      // for the rule
      Performance[L] (lbl, p, r, f1, tp, fp, fn)
    }
  }

  def writeScoresToTSV(scores: Seq[(String, String)], outFile: String): Unit = {
    val f = new File(outFile)
    val header = s"Gold\tPredicted"

    val rows = scores.map(pair => s"${pair._1}\t${pair._2}").mkString("\n")
    val content =
      s"""$header
         |$rows
       """.stripMargin

    f.writeString(content, java.nio.charset.StandardCharsets.UTF_8)
  }

}

object ClassifyAssemblyRelations extends App with LazyLogging {

  import CorpusReader._

  val config = ConfigFactory.load()
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDir")).instances

  // gather precedence relations corpus
  val precedenceAnnotations = filterRelations(eps, precedenceRelations)
  val precedenceDataset = AssemblyRelationClassifier.mkRVFDataset(precedenceAnnotations)
  val pcf = AssemblyRelationClassifier.train(precedenceDataset)
  // get cross validation accuracy
  val scores = Evaluator.crossValidate(precedenceDataset, "lr-l2")
  val accuracy = Evaluator.calculateAccuracy(scores)
  logger.info(f"Precedence relation accuracy (using ${pcf.classifierType} with 5-fold cross validation):\t$accuracy%1.3f")

  // gather subsumption relations corpus
  val subsumptionAnnotations = filterRelations(eps, subsumptionRelations)
  val subsumptionDataset = AssemblyRelationClassifier.mkRVFDataset(subsumptionAnnotations)

  // gather equivalence relations corpus
  val equivalenceAnnotations = filterRelations(eps, subsumptionRelations)
  val equivalenceDataset = AssemblyRelationClassifier.mkRVFDataset(equivalenceAnnotations)
}

object TrainAssemblyRelationClassifier extends App with LazyLogging {
  val config = ConfigFactory.load()
  val classifierType = config.getString("assembly.classifier.classifier")
  val classifierPath = config.getString("assembly.classifier.model")
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDir")).instances

  // gather precedence relations corpus
  val precedenceAnnotations = CorpusReader.filterRelations(eps, precedenceRelations)
  // train
  logger.info(s"Training classifier using ${precedenceAnnotations.size}")
  val precedenceDataset = AssemblyRelationClassifier.mkRVFDataset(precedenceAnnotations)
  val pcf = AssemblyRelationClassifier.train(precedenceDataset, AssemblyRelationClassifier.getModel(classifierType))
  // save model
  logger.info(s"saving trained classifier to $classifierPath . . .")
  pcf.saveTo(classifierPath)
}

/** *
  * Train and evaluate precedence relation classifier
  */
object CrossValidateAssemblyRelationClassifier extends App with LazyLogging {

  val config = ConfigFactory.load()
  val classifierPath = config.getString("assembly.classifier.model")
  val results = config.getString("assembly.classifier.results")
  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDir")).instances

  // gather precedence relations corpus
  val precedenceAnnotations = CorpusReader.filterRelations(eps, precedenceRelations)
  // train
  logger.info(s"Training classifier using ${precedenceAnnotations.size}")
  val precedenceDataset = AssemblyRelationClassifier.mkRVFDataset(precedenceAnnotations)
  //  val pcf = AssemblyRelationClassifier.train(precedenceDataset)
  //  // results
  //
  //  // save model
  //  println(s"saving trained classifier to ${} . . .")
  //  pcf.saveTo(classifierPath)

  // evaluate
  // get cross validation accuracy
  logger.info(s"Running cross validation . . .")
  val models = Seq("lr-l2", "lr-l1", "lin-svm-l2", "lin-svm-l1")//, "rf")
  // evaluate each model
  val res = for {
      model <- models
    } yield {
      val scores = Evaluator.stratifiedCrossValidate(
        dataset = precedenceDataset,
        classifierFactory = () => AssemblyRelationClassifier.getModel(model),
        numFolds = 10
      )
      val performance = Evaluator.calculatePerformance(scores)
      val outFile = s"${FilenameUtils.removeExtension(results)}-$model.${FilenameUtils.getExtension(results)}"
      logger.info(s"Writing results to $outFile . . .")
      Evaluator.writeScoresToTSV(scores, outFile)
      (model, performance)
    }

  logger.info(s"model\tlabel\tp\tr\tf1\ttp\tfp\tfn")
  for {
    (model, performance) <- res
    lbl <- performance
  } {
    logger.info(s"$model\t${lbl.mkRow}")
  }
}
