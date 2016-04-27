package edu.arizona.sista.assembly

import edu.arizona.sista.assembly.representations._
import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.grounding.ReachKBConstants
import edu.arizona.sista.reach.mentions._
import java.io.File
import org.apache.commons.io.FileUtils

case class Row(
  input: String,
  output: String,
  controller: String,
  eventID: String,
  eventLabel: String,
  precededBy: Set[String],
  negated: Boolean,
  evidence: Set[Mention],
  // to make debugging easier
  sourceRepresentation: EntityEventRepresentation
) {
  // this might serve as a proxy for confidence, though
  // it would also be good to know how many times this event
  // was involved in other events (input, controller, etc)
  val seen = evidence.size
  // the set of paper ids where mentions of this event were found
  val docIDs = evidence.map(_.document.id.getOrElse("UNKNOWN"))

  def getTextualEvidence: Seq[String] = {
    evidence.toSeq.map(_.sentenceObj.getSentenceText)
  }

  def toTSVrow: String = {
    val precedingEvents = precededBy.toSeq.sorted.mkString(", ")
    val seenIn = docIDs.toSeq.sorted.mkString(", ")
    val examples = getTextualEvidence.mkString(" ++++ ")
    s"$input\t$output\t$controller\t$eventID\t$eventLabel\t$precedingEvents\t$negated\t$seen\t$examples\t$seenIn"
  }

  def toShellRow: String = {
    val precedingEvents = precededBy.toSeq.sorted.mkString(", ")
    s"""$eventID: ${if(negated) "! " else ""}$input""" +
       s"""==${if (controller.nonEmpty) "[" + controller + "]" else ""}==>""" +
       s"""$output""" +
       s"""${if (precedingEvents.nonEmpty) s"\n\tpreceding events: $precedingEvents" else ""}\n\n"""
  }
}

class AssemblyExporter(val manager: AssemblyManager) {

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
      case Some(m) => m.text
      case noSource =>
        val altText =
          entity.withSameGrounding
            .flatMap(_.evidence.map(resolveEvidence).map(_.text))
        if (altText.nonEmpty) altText.head else "???"
    }
    text
  }

  /**
   * Create text representation of SimpleEntity
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

  def createInput(eer: EntityEventRepresentation): String = eer match {

    case entity: SimpleEntity => createSimpleEntityText(entity)

    case complex: Complex =>
      complex.members.map(createInput).mkString(", ")

    case se: SimpleEvent =>
      se.input.values.flatten.map(createInput).mkString(", ")

    // inputs to an activation are entities
    case act: Activation =>
      act.controlled.map {
        // get IDs of any events
        case event: Event => EERLUT.getOrElse(event.equivalenceHash, reportError(act, event))
        // represent entities directly
        case entity: Entity => createInput(entity)
      }.mkString(", ")

    // inputs to a regulation are other events
    case reg: Regulation =>
      // get event IDs for each controlled
      reg.controlled.map(c => EERLUT.getOrElse(c.equivalenceHash, reportError(reg, c))).mkString(", ")
  }

  def reportError(ce: ComplexEvent, c: EntityEventRepresentation): String = {
    println(s"$c is a controlled of a ComplexEvent but it is not a Complex or Event!")
    val labels = c.evidence.map(_.label)
    val evidence = ce.evidence.map(_.text)
    println(s"Evidence for ComplexEvent: $evidence")
    println(s"Evidence for controlled: ${c.evidence.map(_.text)}")
    println(s"Labels assigned to evidence: $labels")
    "???"
  }
  def createOutput(eer: EntityEventRepresentation): String = eer match {

    case complex: Complex =>
      complex.members.map(createInput).mkString("{", ", ", "}")

    case se: SimpleEvent =>
      se.output.map{
        case binding: Complex => createOutput(binding)
        case other => createInput(other)
      }.mkString(", ")

    case act: Activation =>
      act.controlled.map(c => s"${createInput(c)}.a").mkString(", ")

    case reg: Regulation =>
      reg.controlled.map(createOutput).mkString(", ")

    case _ => ""
  }

  def createController(eer: EntityEventRepresentation): String = eer match {

    case ce: ComplexEvent =>
      ce.controller.map {
        case entity: SimpleEntity => createSimpleEntityText(entity)
        case c => s"${createOutput(c)}.${ce.polarity}"
      }.mkString(", ")

    case _ => ""

  }

  /**
   * Converts predecessors of Event to Event IDs
   * @param eer an [[EntityEventRepresentation]]
   * @return a Set of String representing Event IDs of eer's predecessors
   */
  def precededBy(eer: EntityEventRepresentation): Set[String] = eer match {
    case entity: Entity => Set.empty[String]
    case event: Event =>
      event.predecessors.map(se => EERLUT(se.equivalenceHash))
  }

  /** for debugging purposes */
  def rowsWithPrecededByIDProblems(rows: Set[Row]): Set[Row] = {
    // make sure the output is valid
    val eventIDs: Set[String] = rows.map(_.eventID)
    val problems = rows.filter(r => ! r.precededBy.forall(eid => eventIDs contains eid))
    problems
  }

  /** for debugging purposes */
  def rowsWithOutputIDProblems(rows: Set[Row]): Set[Row] = {
    // make sure the output is valid
    val eventIDs: Set[String] = rows.map(_.eventID)
    val problems = rows
      .filter(r => r.eventLabel == "Regulation")
      .filterNot(r => eventIDs contains r.input)
    problems
  }

  /** Validate the rows before writing */
  def validateOutput(rowsForOutput: Set[Row]): Unit = {
    // make sure the output is valid
    val eventIDs: Set[String] = rowsForOutput.map(_.eventID)
    val precededByIDs: Set[String] = rowsForOutput.flatMap(_.precededBy)
    val regInputIDs = rowsForOutput
      .filter(r => r.eventLabel == "Regulation")
      .map(_.input)
    // check if all precededBy IDs exist as row IDs
    require(precededByIDs.forall(eid => eventIDs contains eid), "Event ID in preceded by does not correspond to any row's ID!")
    // check if all regs have output IDs that correspond to some row ID
    require(regInputIDs.forall(eid => eventIDs contains eid), "Regulation's input ID not found in EventIDs for rows!")
  }

  def writeTSV(outfile: String, rowFilter: Set[Row] => Set[Row]): Unit = {
    val rowsForOutput = rowFilter(getEventRows)

    // validate output
    validateOutput(rowsForOutput)

    // prepare output
    val f = new File(outfile)
    val header = s"INPUT\tOUTPUT\tCONTROLLER\tEVENT ID\tEVENT LABEL\tPRECEDED BY\tNEGATED?\tSEEN\tEVIDENCE\tSEEN IN\n"
    val text =
    // only events
      rowsForOutput
        .toSeq
        .sortBy(r => (r.eventLabel, -r.docIDs.size, -r.seen))
        .map(_.toTSVrow)
        .mkString("\n")
    // write the output to disk
    FileUtils.writeStringToFile(f, header + text)
  }

  def shellOutput(rowFilter: Set[Row] => Set[Row]): String = {
    val rowsForOutput = rowFilter(getEventRows)

    // validate output
    validateOutput(rowsForOutput)

    val text =
    // only events
      rowsForOutput
        .toSeq
        .sortBy(r => (r.eventID))
        .map(_.toShellRow)
        .mkString
    text + "=" * 50
  }

  def getEventLabel(e: EntityEventRepresentation): String = e match {
    case reg: Regulation => "Regulation"
    case act: Activation => "Activation"
    case se: SimpleEvent => se.label
    case ptm: SimpleEntity if ptm.modifications.exists(_.isInstanceOf[representations.PTM]) =>
      ptm.modifications.find(_.isInstanceOf[representations.PTM]).get.asInstanceOf[representations.PTM].label
    //case comp: Complex => "entity"
    // filter these out later
    case entity => "entity"
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

  def getEventRows: Set[Row] = getRows.filter(_.eventLabel != "entity")

}

object AssemblyExporter {

  import AssemblyManager._

  val PTMLUT: Map[String, String] = Map(
      "Phosphorylation" -> ".p",
      "Ubiquitination" -> ".u"
  )

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

  /**
   * Recursively checks whether or not a Mention m contains a Mention with the label Family
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

  def isValidMITREMention(m: Mention): Boolean = m match {
    case entity if entity matches "Entity" => true
    case se if se matches "SimpleEvent" => true
    case reg if reg matches "Regulation" => isValidRegulation(reg)
    // no activations for MITRE
    case act if act matches "ActivationEvent" => false
    // assume false...
    case other => false
  }

  def isValidRegulation(m: Mention): Boolean = m match {
    case entity if entity matches "Entity" => true
    case se if se matches "SimpleEvent" => true
    case act if act matches "ActivationEvent" => false
    case reg if reg matches "Regulation" => reg.arguments.values.flatten.forall(isValidRegulation)
  }
  /**
   * Applies MITRE requirements to assembled events
   * @param rows a Set[Row]
   * @return a filtered Set of [[Row]]
   */
  def MITREfilter(rows: Set[Row]): Set[Row] = {
    // 1a. A finding is to be reported only if it is supported by >= 3 different examples.
    val filteredRows = rows.filter(_.seen >= 3)
      // 1b. Evidence come from at least 2 different sections.
      .filter(_.docIDs.toSet.size >= 2)
      // 2a. No Activations, etc.
      .filter(r => r.evidence.forall(isValidMITREMention))
      // 2b. Findings cannot include protein families.
      .filter(r => r.evidence.forall(e => ! containsFamily(e)))
      // 3. Findings should not have an unresolved (UAZ) grounding.
      // FIXME: probably this should operate over EERs, not their evidence
      .filter(r => r.evidence.forall(e => ! hasUAZgrounding(e)))
    // 4. Only keep precedence entries that exist in set of filteredRow's event IDs
    val eventIDs = filteredRows.map(_.eventID)
    filteredRows.map(r => filterPrecededBy(r, eventIDs))
  }

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
        problem.eventID,
        problem.eventLabel,
        validPs,
        problem.negated,
        problem.evidence,
        problem.sourceRepresentation
      )
  }
  /**
   * Recursively checks whether or not a Mention m contains a Mention with the a UAZ grounding
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
}