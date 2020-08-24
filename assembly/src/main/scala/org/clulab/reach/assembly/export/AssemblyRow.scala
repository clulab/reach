package org.clulab.reach.assembly.export

import org.clulab.odin.Mention
import org.clulab.reach.assembly.representations.EntityEventRepresentation
import org.clulab.reach.mentions._


/** Fundamental attributes of an EER at export */
trait EERDescription {
  val eer: EntityEventRepresentation
  val input: String
  val output: String
  val controller: String
  val eerID: String
  val label: String
  val precededBy: Set[String]
  val negated: Boolean
  //val isDirect: Boolean
  val evidence: Set[Mention]
  val seen: Int
}

/** A Row in the column-based, AssemblyExporter format */
class AssemblyRow(
  val input: String,
  val output: String,
  // FIXME: these should be generated from within AssemblyRow
  val source: String, // only for Translocation
  val destination: String, // only for Translocation
  val controller: String,
  val eerID: String,
  val label: String,
  val precededBy: Set[String],
  val negated: Boolean,
  val evidence: Set[Mention],
  // to make debugging easier
  val eer: EntityEventRepresentation
) extends EERDescription {
  // this might serve as a proxy for confidence, though
  // it would also be good to know how many times this event
  // was involved in other events (input, controller, etc)
  val seen: Int = evidence.size
  // the set of paper ids where mentions of this event were found
  val docIDs: Set[String] = evidence.map(_.document
    .id
    .getOrElse(AssemblyExporter.UNKNOWN)
    // FIXME: hack to trim section/chunk ID
    .split("_").head
  ).map(id => s"PMC$id".replaceAll("^(PMC)*", "PMC"))

  def getTextualEvidence: Seq[String] = {
    evidence.toSeq.map { m =>
      val text = m.sentenceObj.getSentenceText
      cleanText(text)
    }
  }

  def contextFromEvidence(contextLabel: String): String = {
    val contextMentions = for {
      m <- evidence
      cm = m.toCorefMention
      if cm.context.nonEmpty
      contextMap = cm.context.get
      if contextMap contains contextLabel
      entry <- contextMap(contextLabel)
    } yield entry
    contextMentions.mkString(AssemblyExporter.CONCAT)
  }

  protected def cleanText(contents: String): String = {
    // replace multiple whitespace characters (newlines, tabs, etc) with a single space
    val cleanContents = contents.replaceAll("\\s+", " ")
    // check for only whitespace
    AssemblyExporter.WHITESPACE.pattern.matcher(cleanContents).matches match {
      case false => cleanContents
      case true => AssemblyExporter.NONE
    }
  }

  def isIndirect: Boolean = evidence.exists{ e =>
    e.toCorefMention match {
      case em: CorefEventMention => ! em.isDirect
      // mark all activations as indirect
      case activation if activation matches "Activation" => true
      // NOTE: should already be covered by CorefEventMention case
      case se if se matches "SimpleEvent" => false
      // bindings are direct
      case binding if binding matches "Binding" => false
      case complex if complex matches "Complex" => false
      // we'll call entities "direct"
      // NOTE: likely entity rows will not be reported
      case entity if entity matches "Entity" => false
      // a reg with a simple event as its controlled
      case reg if ExportFilters.regWithSimpleEventWithController(reg) => false
      // assume it's indirect otherwise?
      case other => true
    }
  }

  private def setToString(s: Set[String]): String = s.toSeq.sorted.mkString(", ")

  protected val baseColumns: Map[String, String] = {
    Map(
      AssemblyExporter.INPUT -> cleanText(input),
      AssemblyExporter.OUTPUT -> cleanText(output),
      AssemblyExporter.CONTROLLER -> cleanText(controller),
      AssemblyExporter.EVENT_ID -> cleanText(eerID),
      AssemblyExporter.EVENT_LABEL -> label,
      AssemblyExporter.PRECEDED_BY -> setToString(precededBy),
      AssemblyExporter.NEGATED -> negated.toString,
      AssemblyExporter.TRIGGERS -> AssemblyExporter.getSortedTriggers(evidence.toSeq),
      AssemblyExporter.SEEN -> seen.toString,
      AssemblyExporter.EVIDENCE -> getTextualEvidence.mkString(AssemblyExporter.CONCAT),
      AssemblyExporter.SEEN_IN -> setToString(docIDs),
      AssemblyExporter.INDIRECT -> isIndirect.toString,
      //AssemblyExporter.IS_DIRECT -> isDirect.toString,
      AssemblyExporter.CONTEXT_SPECIES -> contextFromEvidence(AssemblyExporter.SPECIES),
      AssemblyExporter.CONTEXT_ORGAN -> contextFromEvidence(AssemblyExporter.ORGAN),
      AssemblyExporter.CONTEXT_CELL_LINE -> contextFromEvidence(AssemblyExporter.CELL_LINE),
      AssemblyExporter.CONTEXT_CELL_TYPE -> contextFromEvidence(AssemblyExporter.CELL_TYPE),
      AssemblyExporter.CONTEXT_CELLULAR_COMPONENT -> contextFromEvidence(AssemblyExporter.CELLULAR_COMPONENT),
      AssemblyExporter.CONTEXT_TISSUE_TYPE -> contextFromEvidence(AssemblyExporter.TISSUE_TYPE),
      // for translocations only: source and destination
      AssemblyExporter.TRANSLOCATION_SOURCE -> cleanText(source),
      AssemblyExporter.TRANSLOCATION_DESTINATION -> cleanText(destination)
    )
  }

  val columns: Map[String, String] = baseColumns

  def toRow(cols: Seq[String], sep: String = AssemblyExporter.SEP): String = {
    val r = for {
      col <- cols
    } yield columns(col)
    r.mkString(sep)
  }
}

object AssemblyRow {
  def apply(
    input: String,
    output: String,
    // FIXME: these should be generated from within AssemblyRow
    source: String, // only for Translocation
    destination: String, // only for Translocation
    controller: String,
    eerID: String,
    label: String,
    precededBy: Set[String],
    negated: Boolean,
    evidence: Set[Mention],
    // to make debugging easier
    eer: EntityEventRepresentation
  ) = new AssemblyRow(input, output, source, destination, controller, eerID, label, precededBy, negated, evidence, eer)
}