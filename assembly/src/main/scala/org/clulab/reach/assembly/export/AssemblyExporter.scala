package org.clulab.reach.assembly.export

import ai.lum.common.FileUtils._
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.representations._
import org.clulab.odin.{ EventMention, Mention }
import org.clulab.reach.assembly._
import org.clulab.reach.grounding.ReachKBConstants
import org.clulab.reach.mentions._
import com.typesafe.scalalogging.LazyLogging

import scala.util.matching.Regex
import java.io.File


/**
  * UA assembly exporter <br>
  * Used to produce a tsv file
  *
  * @param manager
  */
class AssemblyExporter(val manager: AssemblyManager) extends LazyLogging {

  import AssemblyExporter._

  val ignoreMods = false

  // distinct EntityEventRepresentations
  val distinctEERS = manager.distinctEERs

  // LUT for retrieving IDs to distinct EERs
  // TODO: A better version of this should probably belong to the manager
  val EERLUT: Map[Int, String] = distinctEERS.map{ eer => (eer.equivalenceHash(ignoreMods = ignoreMods), mkEventID(eer)) }.toMap

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

  // FIXME: Change EER mods to include Location
  def createSource(eer: EntityEventRepresentation): String = createTranslocationArgument(eer, "source")
  def createDestination(eer: EntityEventRepresentation): String = createTranslocationArgument(eer, "destination")
  def createTranslocationArgument(eer: EntityEventRepresentation, name: String): String = eer match {
    case se: SimpleEvent =>
      if(se.label == "Translocation") {
        for(e <- se.evidence) {
          if(e.arguments.contains(name)){
            // FIXME: use all mentions, not just the head (this matters only when we store non-identical mentions in the same EER)
            val arg = e.arguments(name).head.toCorefMention
            val id = arg.nsId()
            return id
          }
        }
      }
      NONE
    case _ => NONE
  }

  def createInput(eer: EntityEventRepresentation, mods: String = ""): String = eer match {

    case entity: SimpleEntity => s"${createSimpleEntityText(entity)}$mods"

    case complex: Complex =>
      complex.members.map(m => createInput(m, mods)).mkString(", ")

    case se: SimpleEvent =>
      se.input.values.flatten.map(m => createInput(m, mods)).mkString(", ")

    case assoc: Association =>
      assoc.controlled.map{
        // get IDs of any events
        case event: Event => EERLUT.getOrElse(event.equivalenceHash(ignoreMods = ignoreMods), reportError(assoc, event))
        // represent entities directly
        case entity: Entity =>
          createInput(entity, s"$mods")
      }.mkString(", ")


    // inputs to an activation are entities
    case act: Activation =>
      act.controller.map {
        // get IDs of any events
        case event: Event => EERLUT.getOrElse(event.equivalenceHash(ignoreMods = ignoreMods), reportError(act, event))
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
      reg.controlled.map(c => EERLUT.getOrElse(c.equivalenceHash(ignoreMods = ignoreMods), reportError(reg, c))).mkString(", ")
  }

  def createOutput(eer: EntityEventRepresentation, mods: String = ""): String = eer match {

    case complex: Complex =>
      complex.members.map(m => createInput(m, mods)).mkString("{", ", ", "}")

    case se: SimpleEvent =>
      se.output.map{
        case binding: Complex => createOutput(binding)
        case other => createInput(other, mods)
      }.mkString(", ")

    case assoc: Association =>
      assoc.controlled.map{
        // get IDs of any events
        case event: Event => EERLUT.getOrElse(event.equivalenceHash(ignoreMods = ignoreMods), reportError(assoc, event))
        // represent entities directly
        case entity: Entity =>
          createInput(entity, s"$mods")
      }.mkString(", ")

    // positive activations produce an activated output entity
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
        case c => createOutput(c)
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
      event.predecessors.map(se => EERLUT(se.equivalenceHash(ignoreMods = ignoreMods)))
  }

  def writeRows(
    f: File,
    cols: Seq[String],
    sep: String = AssemblyExporter.SEP,
    rowFilter: Seq[AssemblyRow] => Seq[AssemblyRow]
  ): Unit = {

    val results = rowsToString(cols, sep, rowFilter)
    // write the output to disk
    f.writeString(results, java.nio.charset.StandardCharsets.UTF_8)
  }

  def rowsToString(
    cols: Seq[String],
    sep: String = AssemblyExporter.SEP,
    rowFilter: Seq[AssemblyRow] => Seq[AssemblyRow]
  ): String = {
    val rowsForOutput = rowFilter(getRows)
    // validate output
    validateOutput(rowsForOutput)
    // prepare output
    val header =  s"${cols.mkString("\t")}\n"
    val text = rowsForOutput
        .map { row => (row.label, -row.docIDs.size, -row.seen, row.toRow(cols, sep)) }
        .sorted
        .map(_._4)
        .mkString("\n")

    header + text
  }

  // INPUT, OUTPUT, CONTROLLER, PRECEDED BY, EVENT ID, SEEN, EXAMPLE-TEXT
  def getRows: Seq[AssemblyRow] = {

    val rows: Set[AssemblyRow] = distinctEERS
      .map { event =>
        AssemblyRow(
          createInput(event),
          createOutput(event),
          createSource(event),
          createDestination(event),
          createController(event),
          EERLUT(event.equivalenceHash(ignoreMods = ignoreMods)),
          getEventLabel(event),
          precededBy(event),
          event.negated,
          event.evidence,
          event
        )
      }
    rows.toSeq
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
  def rowsWithPrecededByIDProblems(rows: Seq[AssemblyRow]): Seq[AssemblyRow] = {
    // make sure the output is valid
    val eventIDs: Seq[String] = rows.map(_.eerID).distinct
    val problems = rows.filter(r => ! r.precededBy.forall(eid => eventIDs contains eid))
    problems
  }

  /** for debugging purposes */
  def rowsWithOutputIDProblems(rows: Seq[AssemblyRow]): Seq[AssemblyRow] = {
    // make sure the output is valid
    val eventIDs: Seq[String] = rows.map(_.eerID).distinct
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
  val ASSOCIATION = "Association"
  val TRANSLOCATION = "Translocation"

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
  val TRIGGERS = "TRIGGERS"
  val TRANSLOCATION_SOURCE = "TRANSLOCATION (SOURCE)"
  val TRANSLOCATION_DESTINATION = "TRANSLOCATION (DESTINATION)"

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
    // trigger
    AssemblyExporter.TRIGGERS,
    // evidence
    AssemblyExporter.SEEN,
    AssemblyExporter.EVIDENCE,
    AssemblyExporter.SEEN_IN
  )

  /** Validate the rows before writing */
  def validateOutput(rowsForOutput: Seq[AssemblyRow]): Unit = {
    // make sure the output is valid
    val eventIDs: Seq[String] = rowsForOutput.map(_.eerID).distinct
    val precededByIDs: Seq[String] = rowsForOutput.flatMap(_.precededBy).distinct
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
    case assoc: Association => s"$ASSOCIATION (${assoc.polarity})"
    case se: SimpleEvent => se.label
    case ptm: SimpleEntity if ptm.modifications.exists(_.isInstanceOf[representations.PTM]) =>
      ptm.modifications.find(_.isInstanceOf[representations.PTM]).get.asInstanceOf[representations.PTM].label
    //case comp: Complex => "entity"
    // filter these out later
    case entity => "entity"
  }

  def getTrigger(m: Mention): Option[String] = m match {
    case em: EventMention => em.trigger.lemmas.map(_.mkString(" "))
    case _ => None
  }

  def getSortedTriggers(mns: Seq[Mention]): String = {
    // attempt to get the lemmatized trigger for each mention
    val triggers = mns.flatMap(getTrigger)
    val triggerCounts: Seq[(Int, String)] = for {
    // count each distinct trigger
      trig: String <- triggers.distinct
      cnt = triggers.count(_ == trig)
    } yield cnt -> trig

    // sort by counts
    triggerCounts.sortBy(_._1).reverse
      .map(_._2) // get the triggers
      .mkString(AssemblyExporter.CONCAT) // concatenate
  }
}