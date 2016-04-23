package edu.arizona.sista.assembly.relations

import java.io._
import edu.arizona.sista.learning._
import edu.arizona.sista.odin.{RelationMention, EventMention, TextBoundMention, Mention}
import edu.arizona.sista.reach.PaperReader
import edu.arizona.sista.struct.Counter
import scala.annotation.tailrec


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

  /** extract training pair from json annotation and return its features */
  def mkRVFDatum(anno: PrecedenceAnnotation): RVFDatum[String, String] = {
    val mentions = rs.extractFrom(anno.text, anno.`paper-id`, "")

    /** Retrieve trigger from Mention */
    @tailrec
    def findTrigger(m: Mention): TextBoundMention = m match {
      case event: EventMention =>
        event.trigger
      case rel: RelationMention if (rel matches "ComplexEvent") && rel.arguments("controlled").nonEmpty =>
        // could be nested ...
        findTrigger(rel.arguments("controlled").head)
    }

    def findMention(mns: Seq[Mention], label: String, triggerText: String): Mention = {
      mns.filter{ m =>
        // label and trigger text should match
        (m matches label) && (findTrigger(m).text == triggerText)
      }.head
    }

    // prepare datum
    val e1 = findMention(mentions, anno.`e1-label`, anno.`e1-trigger`)
    val e2 = findMention(mentions, anno.`e2-label`, anno.`e2-trigger`)
    val relation = anno.relation
    FeatureExtractor.mkRVFDatum(e1, e2, relation)
  }

  def mkRVFDatum(label: String, e1: Mention, e2: Mention): RVFDatum[String, String] =
    FeatureExtractor.mkRVFDatum(e1, e2, label)

  def mkRVFDataset(annotations: Seq[PrecedenceAnnotation]): RVFDataset[String, String] = {
    val dataset = new RVFDataset[String, String]
    val ds: Seq[Option[RVFDatum[String, String]]] = for {
      a <- annotations
    } yield {
        // in training, some examples rely on text outside of the context (ex. ``this interaction'')
        try {
          Some(mkRVFDatum(a))
        } catch {
          case e: Exception =>
            println(s"problem with annotation ${a.id}")
            None
        }
    }
    ds.foreach{
      case Some(datum) => dataset += datum
      case _ => ()
    }
    dataset
  }
}