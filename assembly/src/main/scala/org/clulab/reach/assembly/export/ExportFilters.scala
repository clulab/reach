package org.clulab.reach.assembly.export

import org.clulab.odin.Mention
import org.clulab.reach.mentions._
import org.clulab.reach.grounding.ReachKBConstants


object ExportFilters {

  import AssemblyExporter._

  /** The EERDescription may not be an entity */
  def isEvent(row: EERDescription): Boolean = {
    // must have a valid eventID
    row.label != AssemblyExporter.ENTITY && (row.label.nonEmpty || row.label != AssemblyExporter.NONE)
  }

  /** Must have a Controller or be a valid Translocation */
  def hasController(r: AssemblyRow): Boolean = r match {
    case valid if valid.controller != NONE => true
    case translocation if translocation.label == TRANSLOCATION =>
      // Translocations w/o controllers are accepted if both source and destination are given
      r.source != NONE && r.destination != NONE
    case _ => false
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

  def filterPrecededBy(row: AssemblyRow, eventIDs: Set[String]): AssemblyRow = row match {
    case nopredecessors if nopredecessors.precededBy.isEmpty => nopredecessors
    case validPreds if validPreds.precededBy.forall(eid => eventIDs contains eid) =>
      validPreds

    case problem =>
      // keep only the predecessors that exist in the current set of rows
      val validPs: Set[String] = problem.precededBy intersect eventIDs

      AssemblyRow(
        problem.input,
        problem.output,
        // FIXME: these should be generated from within AssemblyRow
        problem.source,
        problem.destination,
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
    * @return a filtered Seq of [[AssemblyRow]]
    */
  def MITREfilter(rows: Seq[AssemblyRow]): Seq[AssemblyRow] = {
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
    val eventIDs: Set[String] = filteredRows.map(_.label).toSet
    filteredRows.map(r => filterPrecededBy(r, eventIDs)).distinct
  }


  def regWithSimpleEventWithController(m: Mention): Boolean = m match {
    case hasControllerControlled if (hasControllerControlled.arguments contains "controller") && (hasControllerControlled.arguments contains "controlled") =>
      hasControllerControlled.arguments("controlled").exists {
        case se if se.matches("SimpleEvent") => true
        case reg if reg matches REGULATION => regWithSimpleEventWithController(reg)
        // Activations, etc
        case other => false
      }
    case other => false
  }
}