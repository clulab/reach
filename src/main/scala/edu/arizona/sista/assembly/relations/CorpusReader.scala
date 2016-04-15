package edu.arizona.sista.assembly.relations

import java.io.File
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods


case class PrecedenceAnnotation(
  id: Int,
  text: String,
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
  `cross-sentence`: Boolean,
  `paper-id`: String
) {
  /** Copy constructor for ease of editing precedence annotations */
  def copy(
    annotationID: Int = id,
    annoText: String = text,
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
    crossSentence: Boolean = `cross-sentence`,
    paperID: String = `paper-id`
  ): PrecedenceAnnotation =
    PrecedenceAnnotation(
      annotationID,
      annoText,
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
      crossSentence,
      paperID
  )
}

object CorpusReader {

  // default label for negative class
  val NEG = "None"

  val precedenceRelations =  Set("E1 precedes E2", "E2 precedes E1")
  val subsumptionRelations = Set("E1 subsumes E2", "E2 subsumes E1")
  val equivalenceRelations = Set("Equivalent")

  // needed for .extract
  implicit val formats = DefaultFormats

  def annotationsFromFile(jsonFile: String): Seq[PrecedenceAnnotation] = {
    val json = JsonMethods.parse(new File(jsonFile))
    //    val updatedJson = json transformField {
    //      case ("e1-label", x) => ("e1Label", x)
    //      case ("e1-sentence", x) => ("e1Sentence", x)
    //      }
    //    updatedJson.extract[Seq[PrecedenceAnnotation]]
    json.extract[Seq[PrecedenceAnnotation]]
  }

  /** set all labels not in the set of positive labels to NEG */
  def filterRelations(
    annotations: Seq[PrecedenceAnnotation],
    positiveLabels: Set[String]
  ): Seq[PrecedenceAnnotation] = annotations flatMap {
    // keep subsumption annotations
    case valid if positiveLabels contains valid.relation => Seq(valid)
    // ignore bugs
    case bug if bug.relation == "Bug" => Nil
    // set relation to NEG
    case other =>
      Seq(other.copy(rel = NEG))
  }
}