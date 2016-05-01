package edu.arizona.sista.assembly.relations

import java.io._
import edu.arizona.sista.learning._
import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.PaperReader
import edu.arizona.sista.struct.Counter


class AssemblyRelationClassifier(
  val classifier: Classifier[String, String]
) extends Serializable {

  import AssemblyRelationClassifier._

  val classifierType = classifier.getClass.getCanonicalName

  /** pick the label with the most likely score */
  def classify(datum: RVFDatum[String, String]): String = getLabelScores(datum).argMax._1
  def classify(e1: Mention, e2: Mention): String = classify(mkRVFDatum(UNKNOWN, e1, e2))

  /** get the scores for each possible label */
  def getLabelScores(datum: RVFDatum[String, String]): Counter[String] =
    classifier.scoresOf(datum)

  def saveTo(s: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(s))
    oos.writeObject(this)
    oos.close()
  }

}

object AssemblyRelationClassifier {

  val UNKNOWN = "unknown"
  // reach system (makes use of context parameters specified in config)
  val rs = PaperReader.rs

  // convenience map for plugging in different classifiers
  def getModel(txt: String): Classifier[String, String] = txt match {
    case "lr" => new LogisticRegressionClassifier[String, String]
    case "lin-svm" => new LinearSVMClassifier[String, String]
    case other => new LogisticRegressionClassifier[String, String]
  }

  /** Takes an RVFDataset and a Classifier[String, String] */
  def train(
    dataset: RVFDataset[String, String],
    clf: Classifier[String, String] = getModel("lin-svm")
  ): AssemblyRelationClassifier = {
    // val clf = new LogisticRegressionClassifier[String, String]
    clf.train(dataset)
    new AssemblyRelationClassifier(clf)
  }

  def loadFrom(s: String): AssemblyRelationClassifier = {
    val ois = new ObjectInputStream(new FileInputStream(s))
    val clf = ois.readObject.asInstanceOf[AssemblyRelationClassifier]
    ois.close()
    clf
  }

  /** get features for an assembly label corresponding to a pair of mentions */
  def mkRVFDatum(label: String, e1: Mention, e2: Mention): RVFDatum[String, String] =
    FeatureExtractor.mkRVFDatum(e1, e2, label)

  def mkRVFDataset(annotations: Seq[PrecedenceAnnotation]): RVFDataset[String, String] = {
    val dataset = new RVFDataset[String, String]
    // add each valid annotation to dataset
    for {
      a <- annotations
      pair = CorpusReader.getE1E2(a)
      if pair.nonEmpty
    } {
      val (e1, e2) = pair.get
      val relation = a.relation
      val datum = FeatureExtractor.mkRVFDatum(e1, e2, relation)
      dataset += datum
    }
    dataset
  }
}