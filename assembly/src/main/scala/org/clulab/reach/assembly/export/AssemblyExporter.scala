package org.clulab.reach.assembly.export

import org.apache.commons.io.FileUtils
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.representations._
import org.clulab.odin.Mention
import org.clulab.reach.assembly._
import org.clulab.reach.grounding.ReachKBConstants
import org.clulab.reach.mentions._
import com.typesafe.scalalogging.LazyLogging
import scala.util.matching.Regex
import java.io.File


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

case class Row(
  input: String,
  output: String,
  controller: String,
  eerID: String,
  label: String,
  precededBy: Set[String],
  negated: Boolean,
  evidence: Set[Mention],
  // to make debugging easier
  eer: EntityEventRepresentation
) extends EERDescription {
  // this might serve as a proxy for confidence, though
  // it would also be good to know how many times this event
  // was involved in other events (input, controller, etc)
  val seen = evidence.size
  // the set of paper ids where mentions of this event were found
  val docIDs = evidence.map(_.document
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

  private def cleanText(contents: String): String = {
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

  val columns: Map[String, String] = {
    Map(
      AssemblyExporter.INPUT -> cleanText(input),
      AssemblyExporter.OUTPUT -> cleanText(output),
      AssemblyExporter.CONTROLLER -> cleanText(controller),
      AssemblyExporter.EVENT_ID -> cleanText(eerID),
      AssemblyExporter.EVENT_LABEL -> label,
      AssemblyExporter.PRECEDED_BY -> setToString(precededBy),
      AssemblyExporter.NEGATED -> negated.toString,
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
      AssemblyExporter.CONTEXT_TISSUE_TYPE -> contextFromEvidence(AssemblyExporter.TISSUE_TYPE)
    )
  }

  def toRow(cols: Seq[String], sep: String = AssemblyExporter.SEP): String = {
    val r = for {
      col <- cols
    } yield columns(col)
    r.mkString(sep)
  }
}

/**
 * UA assembly exporter <br>
 * Used to produce a tsv file
  *
  * @param manager
 */
class AssemblyExporter(val manager: AssemblyManager) extends LazyLogging {

  import AssemblyExporter._

  // distinct EntityEventRepresentations
  val distinctEERS = manager.distinctEERs

  // LUT for retrieving IDs to distinct EERs
  // TODO: A better version of this should probably belong to the manager
  val EERLUT: Map[Int, String] = distinctEERS.map{
    case eer =>
      (eer.equivalenceHash, mkEventID(eer))
  }.toMap

  val grounding2Text: Map[GroundingID, String] = {
    val pairs: Set[(GroundingID, String)] = for {
      pair <- manager.distinctSimpleEntitiesWithEvidence
      entity: SimpleEntity = pair._1
      evidence: Set[Mention] = pair._2.map(resolveEvidence)
      id: GroundingID = entity.grounding
      text: String = if (evidence.nonEmpty) s"${evidence.map(_.text.toLowerCase).max}::$id" else s"???::$id"
    } yield (id, text)

    pairs.toMap
  }

  def mkEventID(eer: EntityEventRepresentation): String = {
    s"E${eer.uniqueID}"
  }

  def getText(entity: SimpleEntity): String = {
   val text = entity.sourceMention match {
      // get the resolved form
      case Some(m) => resolveEvidence(m).text
      case noSource =>
        val altText =
          entity.withSameGrounding
            .flatMap(_.evidence.map(resolveEvidence).map(_.text)) // get the resolved form
        if (altText.nonEmpty) altText.head else "???"
    }
    text
  }

  /**
   * Create text representation of SimpleEntity
    *
    * @param entity a SimpleEntity
   * @return a String representing entity and its modifications, mutations, etc
   */
  def createSimpleEntityText(entity: SimpleEntity): String = {

    // partition modifications
    val (muts, other) =
      entity.modifications.partition(_.isInstanceOf[MutantEntity])

    // mutations
    val mutations = muts.map(_.asInstanceOf[MutantEntity])

    // construct mutant forms, if present
    val mutantForms: String = mutations
      .map(m => s":[${m.mutantType}]")
      //ex. :[mutant1]:[mutant2]
      .toSeq.mkString(":")

    // find PTMs
    val ptms: Set[representations.PTM] = other.filter(_.isInstanceOf[representations.PTM])map(_.asInstanceOf[representations.PTM])
    val features: String = ptms
      .map(getPTMrepresentation)
      .mkString(".")

    val text = getText(entity)
    s"$text::${entity.grounding}$mutantForms$features"
  }

  def createInput(eer: EntityEventRepresentation, mods: String = ""): String = eer match {

    case entity: SimpleEntity => s"${createSimpleEntityText(entity)}$mods"

    case complex: Complex =>
      complex.members.map(m => createInput(m, mods)).mkString(", ")

    case se: SimpleEvent =>
      se.input.values.flatten.map(m => createInput(m, mods)).mkString(", ")

    // inputs to an activation are entities
    case act: Activation =>
      act.controlled.map {
        // get IDs of any events
        case event: Event => EERLUT.getOrElse(event.equivalenceHash, reportError(act, event))
        // represent entities directly
        case entity: Entity =>
          val activationMod = act.polarity match {
            // negative activations start with activated input
            case AssemblyManager.negative => ".a"
            case _ => ""
          }
          createInput(entity, s"$mods$activationMod")
      }.mkString(", ")

    // inputs to a regulation are other events
    case reg: Regulation =>
      // get event IDs for each controlled
      reg.controlled.map(c => EERLUT.getOrElse(c.equivalenceHash, reportError(reg, c))).mkString(", ")
  }

  def createOutput(eer: EntityEventRepresentation, mods: String = ""): String = eer match {

    case complex: Complex =>
      complex.members.map(m => createInput(m, mods)).mkString("{", ", ", "}")

    case se: SimpleEvent =>
      se.output.map{
        case binding: Complex => createOutput(binding)
        case other => createInput(other, mods)
      }.mkString(", ")

    // positive activations produce an acitvated output entity
    case posact: Activation if posact.polarity == AssemblyManager.positive =>
      posact.controlled.map(c => createInput(c, s"$mods.a")).mkString(", ")

    case ce: ComplexEvent =>
      ce.controlled.map(c => createOutput(c, mods)).mkString(", ")

    // PTM-like cases
    case entity: Entity => createInput(entity, mods)

    case _ => NONE
  }

  def createController(eer: EntityEventRepresentation): String = eer match {

    case ce: ComplexEvent =>
      ce.controller.map {
        case entity: SimpleEntity => createSimpleEntityText(entity)
        case complex: Complex => createInput(complex)
        case c => s"${createOutput(c)}.${ce.polarity}"
      }.mkString(", ")

    case _ => NONE

  }

  /**
   * Converts predecessors of Event to Event IDs
    *
    * @param eer an [[EntityEventRepresentation]]
   * @return a Set of String representing Event IDs of eer's predecessors
   */
  def precededBy(eer: EntityEventRepresentation): Set[String] = eer match {
    case entity: Entity => Set.empty[String]
    case event: Event =>
      event.predecessors.map(se => EERLUT(se.equivalenceHash))
  }

  def writeRows(
    f: File,
    cols: Seq[String],
    sep: String = AssemblyExporter.SEP,
    rowFilter: Set[Row] => Set[Row]
  ): Unit = {

    val results = rowsToString(cols, sep, rowFilter)
    // write the output to disk
    FileUtils.writeStringToFile(f, results)
  }

  def rowsToString(
    cols: Seq[String],
    sep: String = AssemblyExporter.SEP,
    rowFilter: Set[Row] => Set[Row]
  ): String = {
    val rowsForOutput = rowFilter(getRows)
    // validate output
    validateOutput(rowsForOutput)
    // prepare output
    val header =  s"${cols.mkString("\t")}\n"
    val text =
      rowsForOutput
        .toSeq
        .sortBy(r => (r.label, -r.docIDs.size, -r.seen))
        .map(_.toRow(cols, sep))
        .mkString("\n")

    header + text
  }

  // INPUT, OUTPUT, CONTROLLER, PRECEDED BY, EVENT ID, SEEN, EXAMPLE-TEXT
  def getRows: Set[Row] = {

      val rows: Set[Row] = distinctEERS
        .map { event =>
          Row(
            createInput(event),
            createOutput(event),
            createController(event),
            EERLUT(event.equivalenceHash),
            getEventLabel(event),
            precededBy(event),
            event.negated,
            event.evidence,
            event
         )
      }
    rows
    }

  /** for debugging purposes */
  def reportError(ce: ComplexEvent, c: EntityEventRepresentation): String = {
    logger.error(s"$c is a controlled of a ComplexEvent but it is not a Complex or Event!")
    val labels = c.evidence.map(_.label)
    val evidence = ce.evidence.map(_.text)
    logger.error(s"Evidence for ComplexEvent: $evidence")
    logger.error(s"Evidence for controlled: ${c.evidence.map(_.text)}")
    logger.error(s"Labels assigned to evidence: $labels")
    "???"
  }

  /** for debugging purposes */
  def rowsWithPrecededByIDProblems(rows: Set[Row]): Set[Row] = {
    // make sure the output is valid
    val eventIDs: Set[String] = rows.map(_.eerID)
    val problems = rows.filter(r => ! r.precededBy.forall(eid => eventIDs contains eid))
    problems
  }

  /** for debugging purposes */
  def rowsWithOutputIDProblems(rows: Set[Row]): Set[Row] = {
    // make sure the output is valid
    val eventIDs: Set[String] = rows.map(_.eerID)
    val problems = rows
      .filter(r => r.label == REGULATION)
      .filterNot(r => eventIDs contains r.input)
    problems
  }
}


object AssemblyExporter {

  import AssemblyManager._

  val WHITESPACE = new Regex("\\s+")

  val UNKNOWN = "UNKNOWN"
  val NONE = "NONE"

  val PTMLUT: Map[String, String] = Map(
      "Phosphorylation" -> ".p",
      "Ubiquitination" -> ".u"
  )

  // eer labels (for filtering)
  val ENTITY = "entity"
  val REGULATION = "Regulation"
  val ACTIVATION = "Activation"

  // context types
  val SPECIES = "Species"
  val ORGAN = "Organ"
  val CELL_LINE = "CellLine"
  val CELL_TYPE = "CellType"
  val CELLULAR_COMPONENT = "Cellular_component"
  val TISSUE_TYPE = "TissueType"

  // columns
  val INPUT = "INPUT"
  val OUTPUT = "OUTPUT"
  val CONTROLLER = "CONTROLLER"
  val EVENT_ID = "EVENT ID"
  val EVENT_LABEL = "EVENT LABEL"
  val PRECEDED_BY = "PRECEDED BY"
  val NEGATED = "NEGATED"
  val SEEN = "SEEN"
  val EVIDENCE = "EVIDENCE"
  val SEEN_IN = "SEEN IN"
  //val IS_DIRECT = "IS DIRECT?"
  val INDIRECT = "INDIRECT?"
  val CONTEXT_SPECIES = "CONTEXT (SPECIES)"
  val CONTEXT_ORGAN = "CONTEXT (ORGAN)"
  val CONTEXT_CELL_LINE = "CONTEXT (CELL LINE)"
  val CONTEXT_CELL_TYPE = "CONTEXT (CELL TYPE)"
  val CONTEXT_CELLULAR_COMPONENT = "CONTEXT (CELLULAR COMPONENT)"
  val CONTEXT_TISSUE_TYPE = "CONTEXT (TISSUE TYPE)"

  val SEP = "\t"
  val CONCAT = " ++++ "

  val DEFAULT_COLUMNS = Seq(
    AssemblyExporter.INPUT,
    AssemblyExporter.OUTPUT,
    AssemblyExporter.CONTROLLER,
    AssemblyExporter.EVENT_ID,
    AssemblyExporter.EVENT_LABEL,
    AssemblyExporter.PRECEDED_BY,
    AssemblyExporter.NEGATED,
    AssemblyExporter.INDIRECT,
    // context
    AssemblyExporter.CONTEXT_SPECIES,
    AssemblyExporter.CONTEXT_ORGAN,
    AssemblyExporter.CONTEXT_CELL_LINE,
    AssemblyExporter.CONTEXT_CELL_TYPE,
    AssemblyExporter.CONTEXT_CELLULAR_COMPONENT,
    AssemblyExporter.CONTEXT_TISSUE_TYPE,
    // evidence
    AssemblyExporter.SEEN,
    AssemblyExporter.EVIDENCE,
    AssemblyExporter.SEEN_IN
  )

  /** Validate the rows before writing */
  def validateOutput(rowsForOutput: Set[Row]): Unit = {
    // make sure the output is valid
    val eventIDs: Set[String] = rowsForOutput.map(_.eerID)
    val precededByIDs: Set[String] = rowsForOutput.flatMap(_.precededBy)
    val regInputIDs = rowsForOutput
      .filter(r => r.label == REGULATION)
      .map(_.input)
    // check if all precededBy IDs exist as row IDs
    require(precededByIDs.forall(eid => eventIDs contains eid), "Event ID in preceded by does not correspond to any row's ID!")
    // check if all regs have output IDs that correspond to some row ID
    require(regInputIDs.forall(eid => eventIDs contains eid), "Regulation's input ID not found in EventIDs for rows!")
  }

  def resolveEvidence(m: Mention): CorefMention = {
    getResolvedForm(m.toCorefMention)
  }

  def getPTMrepresentation(ptm: representations.PTM): String = {
    // attempt to retrieve the abbreviated form of the label
    // if the key is missing from the LUT,
    // return the lowercase form of the first letter of the PTM's label
    val altPTM = s".${ptm.label.toLowerCase.head.toString}"
    val ptmText = PTMLUT.getOrElse(ptm.label, altPTM)
    ptm.site match {
      // has site
      case Some(site) => s"$ptmText@$site"
      // no site
      case None => ptmText
    }
  }

  def getEventLabel(e: EntityEventRepresentation): String = e match {
    case reg: Regulation => s"$REGULATION (${reg.polarity})"
    case act: Activation => s"$ACTIVATION (${act.polarity})"
    case se: SimpleEvent => se.label
    case ptm: SimpleEntity if ptm.modifications.exists(_.isInstanceOf[representations.PTM]) =>
      ptm.modifications.find(_.isInstanceOf[representations.PTM]).get.asInstanceOf[representations.PTM].label
    //case comp: Complex => "entity"
    // filter these out later
    case entity => "entity"
  }
}


object ExportFilters {

  /** The EERDescription may not be an entity */
  def isEvent(row: EERDescription): Boolean = {
    // must have a valid eventID
    row.label != AssemblyExporter.ENTITY && (row.label.nonEmpty || row.label != AssemblyExporter.NONE)
  }

  /**
    * Recursively checks whether or not a Mention m contains a Mention with the a UAZ grounding
    *
    * @param m an Odin Mention
    * @return true or false
    */
  def hasUAZgrounding(m: Mention): Boolean = {
    m match {
      case entity if entity matches "Entity" =>
        entity.toCorefMention.nsId.toLowerCase.startsWith(ReachKBConstants.DefaultNamespace)
      case site if site matches "Site" => false
      case event if event matches "Event" =>
        event.arguments.values.flatten.exists(hasUAZgrounding)
    }
  }

  /**
    * Recursively checks whether or not a Mention m contains a Mention with the label Family
    *
    * @param m an Odin Mention
    * @return true or false
    */
  def containsFamily(m: Mention): Boolean = m match {
    case entity if entity matches "Entity" =>
      entity matches "Family"
    case site if site matches "Site" => false
    case event if event matches "Event" =>
      event.arguments.values.flatten.exists(containsFamily)
  }

  def isValidRegulation(m: Mention): Boolean = m match {
    case entity if entity matches "Entity" => true
    case se if se matches "SimpleEvent" => true
    case act if act matches "ActivationEvent" => false
    case reg if reg matches "Regulation" => reg.arguments.values.flatten.forall(isValidRegulation)
  }

  def seenAtLeastOnce(row: EERDescription): Boolean = row.seen >= 1

  def filterPrecededBy(row: Row, eventIDs: Set[String]): Row = row match {
    case nopredecessors if nopredecessors.precededBy.isEmpty => nopredecessors
    case validPreds if validPreds.precededBy.forall(eid => eventIDs contains eid) =>
      validPreds

    case problem =>
      // keep only the predecessors that exist in the current set of rows
      val validPs: Set[String] = problem.precededBy intersect eventIDs

      Row(
        problem.input,
        problem.output,
        problem.controller,
        problem.eerID,
        problem.label,
        validPs,
        problem.negated,
        problem.evidence,
        problem.eer
      )
  }

  def isValidMITREMention(m: Mention): Boolean = m match {
    case entity if entity matches "Entity" => true
    case se if se matches "SimpleEvent" => true
    case reg if reg matches "Regulation" => isValidRegulation(reg)
    // no activations for MITRE
    case act if act matches "ActivationEvent" => false
    // assume false...
    case other => false
  }

  /**
    * Applies MITRE requirements to assembled events
    *
    * @param rows a Set[Row]
    * @return a filtered Set of [[Row]]
    */
  def MITREfilter(rows: Set[Row]): Set[Row] = {
    // 1a. A finding is to be reported only if it is supported by >= 3 different examples.
    val filteredRows = rows.filter(_.seen >= 3)
      // 1b. Evidence come from at least 2 different sections.
      .filter(_.docIDs.size >= 2)
      // 2a. No Activations, etc.
      .filter(r => r.evidence.forall(isValidMITREMention))
      // 2b. Findings cannot include protein families.
      .filter(r => r.evidence.forall(e => ! containsFamily(e)))
      // 3. Findings should not have an unresolved (UAZ) grounding.
      // FIXME: probably this should operate over EERs, not their evidence
      .filter(r => r.evidence.forall(e => ! hasUAZgrounding(e)))
    // 4. Only keep precedence entries that exist in set of filteredRow's event IDs
    val eventIDs = filteredRows.map(_.label)
    filteredRows.map(r => filterPrecededBy(r, eventIDs))
  }


  def regWithSimpleEventWithController(m: Mention): Boolean = m match {
    case hasControllerControlled if (hasControllerControlled.arguments contains "controller") && (hasControllerControlled.arguments contains "controlled") =>
      hasControllerControlled.arguments("controlled").exists {
        case se if se.matches("SimpleEvent") => true
        case reg if reg matches AssemblyExporter.REGULATION => regWithSimpleEventWithController(reg)
        // Activations, etc
        case other => false
      }
    case other => false
  }
}