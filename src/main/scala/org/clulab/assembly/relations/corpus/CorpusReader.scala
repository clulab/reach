package org.clulab.assembly.relations.corpus

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.clulab.assembly.AssemblyManager
import org.clulab.assembly.relations.classifier.AssemblyRelationClassifier
import org.clulab.assembly.sieves.{Constraints, SieveUtils}
import org.clulab.odin.Mention
import org.clulab.reach.PaperReader._
import org.clulab.utils.Serializer
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json.JSONSerializer


case class AssemblyAnnotation(
  id: Int,
  text: String,
  coref: Boolean,
  // event 1
  `e1-label`: String,
  `e1-sentence`: String,
  `e1-sentence-index`: String,
  `e1-tokens`: Seq[String],
  // can be used to highlight event span in annotation UI
  `e1-start`: Int,
  `e1-end`: Int,
  `e1-trigger`: String,
  `e1-trigger-start`: Int,
  `e1-trigger-end`: Int,
  // event 2
  `e2-label`: String,
  `e2-sentence`: String,
  `e2-sentence-index`: String,
  `e2-tokens`: Seq[String],
  // can be used to highlight event span in annotation UI
  `e2-start`: Int,
  `e2-end`: Int,
  `e2-trigger`: String,
  `e2-trigger-start`: Int,
  `e2-trigger-end`: Int,
  // these will be filled out during annotation
  `annotator-id`: String,
  relation: String,
  confidence: Double = AnnotationUtils.HIGH,
  // details on annotation instance
  `cross-sentence`: Boolean,
  `paper-id`: String,
  notes: Option[String]
) {
  /** Copy constructor for ease of editing precedence annotations */
  def copy(
    annotationID: Int = id,
    annoText: String = text,
    involvesCoref: Boolean = coref,
    // event 1
    e1Label: String = `e1-label`,
    e1Sentence: String = `e1-sentence`,
    e1SentenceIndex: String = `e1-sentence-index`,
    e1Tokens: Seq[String] = `e1-tokens`,
    // can be used to highlight event span in annotation UI
    e1Start: Int = `e1-start`,
    e1End: Int = `e1-end`,
    e1Trigger: String = `e1-trigger`,
    e1TriggerStart: Int = `e1-trigger-start`,
    e1TriggerEnd: Int = `e1-trigger-end`,
    // event 2
    e2Label: String = `e2-label`,
    e2Sentence: String = `e2-sentence`,
    e2SentenceIndex: String = `e2-sentence-index`,
    e2Tokens: Seq[String] = `e2-tokens`,
    // can be used to highlight event span in annotation UI
    e2Start: Int = `e2-start`,
    e2End: Int = `e2-end`,
    e2Trigger: String = `e2-trigger`,
    e2TriggerStart: Int = `e2-trigger-start`,
    e2TriggerEnd: Int = `e2-trigger-end`,
    // these will be filled out during annotation
    annotatorID: String = `annotator-id`,
    rel: String = relation,
    confScore: Double = confidence,
    // details on annotation instance
    crossSentence: Boolean = `cross-sentence`,
    paperID: String = `paper-id`,
    notes: Option[String] = this.notes
  ): AssemblyAnnotation =
    AssemblyAnnotation(
      annotationID,
      annoText,
      involvesCoref,
      // event 1
      e1Label,
      e1Sentence,
      e1SentenceIndex,
      e1Tokens,
      // can be used to highlight event span in annotation UI
      e1Start,
      e1End,
      e1Trigger,
      e1TriggerStart,
      e1TriggerEnd,
      // event 2
      e2Label,
      e2Sentence,
      e2SentenceIndex,
      e2Tokens,
      // can be used to highlight event span in annotation UI
      e2Start,
      e2End,
      e2Trigger,
      e2TriggerStart,
      e2TriggerEnd,
      // these will be filled out during annotation
      annotatorID,
      rel,
      confScore,
      // annotation instance details
      crossSentence,
      paperID,
      notes
    )
}

object CorpusReader extends LazyLogging {

  // default label for negative class
  val NEG = AssemblyRelationClassifier.NEG

  val precedenceRelations =  Set("E1 precedes E2", "E2 precedes E1")
  val subsumptionRelations = Set("E1 specifies E2", "E2 specifies E1")
  val equivalenceRelations = Set("Equivalent")
  val noRelations = Set(NEG)

  // needed for .extract
  implicit val formats = DefaultFormats

  val datasetSource = config.getString("assembly.serializedDataset")

  // TODO: write replacement to read new json
  def annotationsFromFile(jsonFile: String): Seq[AssemblyAnnotation] = {

    val json = jsonFile match {
      case url if url.startsWith("http") =>
        val fileContents = scala.io.Source.fromURL(url).mkString
        JsonMethods.parse(fileContents)
      case other =>
        val f = new File(other)
        JsonMethods.parse(f)
    }
    //    val updatedJson = json transformField {
    //      case ("e1-label", x) => ("e1Label", x)
    //      case ("e1-sentence", x) => ("e1Sentence", x)
    //      }
    //    updatedJson.extract[Seq[PrecedenceAnnotation]]
    json.extract[Seq[AssemblyAnnotation]]
  }

  def annotationsFromFileNEW(jsonFile: String): Seq[AssemblyAnnotation] = ???

  /** set all labels not in the set of positive labels to NEG */
  def filterRelations(
    eps: Seq[EventPair],
    positiveLabels: Set[String]
  ): Seq[EventPair] = eps flatMap {
    // keep subsumption annotations
    case valid if positiveLabels contains valid.relation => Seq(valid)
    // ignore bugs
    case bug if bug.relation == "Bug" => Nil
    // set relation to NEG
    case other =>
      Seq(other.copy(relation = NEG))
  }

  /** Finds mention matching label and trigger text */
  def findMention(mns: Seq[Mention], label: String, triggerText: String): Mention = {
    mns.filter{ m =>
      // label and trigger text should match
      (m matches label) && (SieveUtils.findTrigger(m).text == triggerText)
    }.head
  }

  def buildEventPairs(aas: Seq[AssemblyAnnotation]): Seq[EventPair] = {

    val aasPaperIDLUT: Map[String, Seq[AssemblyAnnotation]] = aas.groupBy(_.`paper-id`)

    logger.info(s"Loading dataset from $datasetSource")

    val extractedMentionsLUT: Map[String, Seq[CorefMention]] = datasetLUT
    logger.info(s"Retrieving event pairs based on annotations for ${datasetLUT.size} documents")

    // get training instances
    val eps: Seq[EventPair] = for {
      (paperID, aas) <- aasPaperIDLUT.toSeq
      mentionPool = extractedMentionsLUT(paperID.replace("PMC", ""))
      aa <- aas
      ep = findEventPair(aa, mentionPool)
      if ep.nonEmpty
    } yield ep.get

    eps
  }

  def datasetLUT: Map[String, Seq[CorefMention]] = {
    def parseJSON(f: File): Option[Seq[CorefMention]] = try {
        Some(JSONSerializer.toCorefMentions(f))
    } catch {
      case e: org.json4s.ParserUtil.ParseException => {
        logger.info(s"Failed to parse $f")
        None
      }
    }
    val docMentionPairs = for {
      f <- new File(datasetSource).listFiles
      cms: Option[Seq[CorefMention]] = parseJSON(f)
      if cms.nonEmpty
      paperID = cms.get.head.document.id.get.split("_").head.replace("PMC", "")
    } yield  paperID -> cms.get

    docMentionPairs.toMap.withDefaultValue(Nil)
  }

  def findEventPair(anno: AssemblyAnnotation, candidates: Seq[CorefMention]): Option[EventPair] = try {
    // prepare datum
    val e1 = findMention(candidates, anno.`e1-label`, anno.`e1-trigger`).toCorefMention
    val e2 = findMention(candidates, anno.`e2-label`, anno.`e2-trigger`).toCorefMention
    val ep = EventPair(anno.text, e1, e2, anno.relation)
    Some(ep)
  } catch {
    case e: Exception =>
      logger.error(s"problem with annotation ${anno.id}")
      None
  }

  /** Find equivalent event pairs in a given window **/
  def equivalentEventPairs(
    eps: Seq[EventPair],
    dsLUT: Map[String, Seq[CorefMention]],
    kWindow: Int): Seq[EventPair] = {

    // TODO: make sure this is called with the filtered event pairs (i.e., only those pairs with one of the "precedes" label)
    val epsLUT: Map[String, Seq[EventPair]] = eps.groupBy(_.pmid)
    val proximalEquivalentPairs = for {
      (paperID: String, pairs: Seq[EventPair]) <- epsLUT
      epMentions: Seq[CorefMention] = pairs.flatMap{case pair: EventPair => Seq(pair.e1, pair.e2)}
      mns = dsLUT(paperID)
      // initialize an AssemblyManager with all relevant mentions for the paper
      am = AssemblyManager(mns ++ epMentions)
      // find equivalent mentions for the pair within this paper
      p <- pairs
      e1EER = am.getEER(p.e1)
      e1EquivMentions = am.getEquivalentEERs(e1EER).flatMap(_.evidence).toSeq
      e2EER = am.getEER(p.e2)
      e2EquivMentions = am.getEquivalentEERs(e2EER).flatMap(_.evidence).toSeq
      e1equiv <- e1EquivMentions
      e2equiv <- e2EquivMentions
      // TODO: must both of these be true?
      if e1equiv != p.e1
      if e2equiv != p.e2
      // impose window constraints
      // TODO: check these
      if Constraints.withinWindow(e1equiv, e2equiv, kWindow)
      if Constraints.withinWindow(e1equiv, p.e1, kWindow) || Constraints.withinWindow(e1equiv, p.e2, kWindow)
      if Constraints.withinWindow(e2equiv, p.e1, kWindow) || Constraints.withinWindow(e2equiv, p.e2, kWindow)
      text: String = CorpusBuilder.getSententialSpan(e1equiv, e2equiv)
    // we don't know which is textually precedes the other
    } yield EventPair(text, Set(e1equiv.toCorefMention, e2equiv.toCorefMention))
    proximalEquivalentPairs.toSeq
  }

  def readCorpus: Seq[EventPair] = {
    val aas: Seq[AssemblyAnnotation] = annotationsFromFile(config.getString("assembly.classifier.trainingFile"))
    val eps: Seq[EventPair] = buildEventPairs(aas)
    eps
  }
}

/**
  * Houses values for three confidence bins, etc.
  */
object AnnotationUtils {
  val LOW = 0.25
  val NORMAL = 0.75
  val HIGH = 1.0
}