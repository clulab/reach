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
)

object CorpusReader {

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
}