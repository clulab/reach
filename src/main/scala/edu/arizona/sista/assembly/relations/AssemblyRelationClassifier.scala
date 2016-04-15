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
  def classify(e1: Mention, e2: Mention): String = classify(mkDatum(UNKNOWN, e1, e2))

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
  val models = Map[String, Classifier[String, String]](
    "lr" -> new LogisticRegressionClassifier[String, String],
    "lin-svm" -> new LinearSVMClassifier[String, String]
  )

  /** Takes an RVFDataset and a Classifier[String, String] */
  def train(
    dataset: RVFDataset[String, String],
    clf: Classifier[String, String] = models("lin-svm")
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

  /** extract training pair from json annotation and return its features */
  def mkRVFDatum(anno: PrecedenceAnnotation): RVFDatum[String, String] = {
    val mentions = rs.extractFrom(anno.`e1-sentence`, anno.`paper-id`, "")

    def findMention(mns: Seq[Mention], label: String, start: Int, end: Int): Mention = {
      mns.filter{ m =>
        (m matches anno.`e1-label`) && (m.start == anno.`e1-start`) && (m.end == anno.`e1-end`)
      }.head
    }
    val e1 = findMention(mentions, anno.`e1-label`, anno.`e1-start`, anno.`e1-end`)
    val e2 = findMention(mentions, anno.`e2-label`, anno.`e2-start`, anno.`e2-end`)
    val relation = anno.relation
    FeatureExtractor.mkRVFDatum(e1, e2, relation)
  }

  def mkRVFDatum(label: String, e1: Mention, e2: Mention): RVFDatum[String, String] =
    FeatureExtractor.mkRVFDatum(e1, e2, label)

  def mkRVFDataset(annotations: Seq[PrecedenceAnnotation]): RVFDataset[String, String] = {
    val dataset = new RVFDataset[String, String]
    annotations.map(mkRVFDatum).foreach(d => dataset += d)
    dataset
  }
}


//object ClassifyMentionPairs extends App {
//  val extractor = AssemblyRelationClassifier.loadFrom("assembly.extractor.clf")
//  //FileUtils.writeStringToFile(new File(outDir, "results.txt"), output)
//}