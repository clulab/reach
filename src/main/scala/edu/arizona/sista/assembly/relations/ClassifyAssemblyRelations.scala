package edu.arizona.sista.assembly.relations

import java.io.File
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.assembly.relations.CorpusReader._
import edu.arizona.sista.learning.Datasets
import edu.arizona.sista.learning.RVFDataset
import org.apache.commons.io.FileUtils


object Evaluator {

  def crossValidate(dataset: RVFDataset[String, String]): Seq[(String, String)] = {
    Datasets.crossValidate[String, String](dataset, () => AssemblyRelationClassifier.getModel("lr")).toSeq
  }

  def calculateAccuracy(scores: Seq[(String, String)]): Float = {
    scores.count(pair => pair._1 == pair._2).toFloat / scores.size.toFloat
  }

  def writeScoresToTSV(scores: Seq[(String, String)], outFile: String): Unit = {
    val f = new File(outFile)
    val header = s"Gold\tPredicted"

    val rows = scores.map(pair => s"${pair._1}\t${pair._2}").mkString("\n")
    val content =
      s"""$header
          |$rows
       """.stripMargin

    FileUtils.writeStringToFile(f, content)
  }

}

object ClassifyAssemblyRelations extends App {

  import CorpusReader._

  val config = ConfigFactory.load()
  val annotations: Seq[PrecedenceAnnotation] = CorpusReader.annotationsFromFile(config.getString("assembly.annotations"))

  // gather precedence relations corpus
  val precedenceAnnotations = filterRelations(annotations, precedenceRelations)
  val precedenceDataset = AssemblyRelationClassifier.mkRVFDataset(precedenceAnnotations)
  val pcf = AssemblyRelationClassifier.train(precedenceDataset)
  // get cross validation accuracy
  val scores = Evaluator.crossValidate(precedenceDataset)
  val accuracy = Evaluator.calculateAccuracy(scores.toSeq)
  println(f"Precedence relation accuracy (using ${pcf.classifierType} with 5-fold cross validation):\t$accuracy%1.3f")

  // gather subsumption relations corpus
  val subsumptionAnnotations = filterRelations(annotations, subsumptionRelations)
  val subsumptionDataset = AssemblyRelationClassifier.mkRVFDataset(subsumptionAnnotations)

  // gather equivalence relations corpus
  val equivalenceAnnotations = filterRelations(annotations, subsumptionRelations)
  val equivalenceDataset = AssemblyRelationClassifier.mkRVFDataset(equivalenceAnnotations)
}

/** *
  * Train and evaluate precedence relation classifier
  */
object TrainAssemblyRelationClassifier extends App {

  val config = ConfigFactory.load()
  val annotationsPath = config.getString("assembly.annotations")
  val classifierPath = config.getString("assembly.classifier.model")
  val results = config.getString("assembly.classifier.results")
  val annotations: Seq[PrecedenceAnnotation] = CorpusReader.annotationsFromFile(config.getString("assembly.annotations"))

  // gather precedence relations corpus
  val precedenceAnnotations = CorpusReader.filterRelations(annotations, precedenceRelations)
  // train
  println(s"Training classifier using ${precedenceAnnotations.size}")
  val precedenceDataset = AssemblyRelationClassifier.mkRVFDataset(precedenceAnnotations)
  val pcf = AssemblyRelationClassifier.train(precedenceDataset)
  // results

  // save model
  println(s"saving trained classifier to ${} . . .")
  pcf.saveTo(classifierPath)

  // evaluate
  // get cross validation accuracy
  println(s"Running cross validation . . .")
  val scores = Evaluator.crossValidate(precedenceDataset)
  val accuracy = Evaluator.calculateAccuracy(scores)
  println(f"Accuracy: $accuracy%1.5f")
  println(s"Writing results to $results . . .")
  Evaluator.writeScoresToTSV(scores, results)
}
