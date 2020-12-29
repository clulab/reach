package org.clulab.reach.export.cmu

import java.util.regex.Pattern

import org.clulab.odin.Mention
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.export.{AssemblyExporter, AssemblyRow, ExportFilters}
import org.clulab.reach.assembly.representations._
import org.clulab.reach.assembly.sieves.{AssemblySieve, DeduplicationSieves}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
  * Yet another tabular format
  * This follows the specification from CMU, to integrate nicely with the DyCE model
  * Created by mihais on 11/23/16.
  */
class CMUExporter(manager: AssemblyManager) extends AssemblyExporter(manager) {

  def createMechanismType(eer: EntityEventRepresentation): String = eer match {
    case ne: Regulation =>
      // FIXME: use all mentions, not just the head (this matters only when we store non-identical mentions in the same EER)
      val simpleEvent = findSimpleEventControlled(ne.evidence.head)
      if (simpleEvent.isDefined) simpleEvent.get.label
      else AssemblyExporter.NONE
    case _ => AssemblyExporter.NONE
  }

  def findSimpleEventControlled(event: Mention): Option[Mention] = {
    if(event.label.contains(AssemblyExporter.REGULATION.toLowerCase())) {
      if(event.arguments.contains("controlled"))
      // FIXME: use all mentions, not just the head (this matters only when we store non-identical mentions in the same EER)
        return findSimpleEventControlled(event.arguments.get("controlled").get.head)
      else
        return None
    } else if (event.label.contains(AssemblyExporter.ACTIVATION.toLowerCase())) {
      // we are looking for mechanistic events, not activations
      return None
    }

    Some(event)
  }

  def createNestedControllers(eer: EntityEventRepresentation): (List[String], List[String]) = {
    val positiveControllers = new ListBuffer[String]
    val negativeControllers = new ListBuffer[String]
    fetchNestedControllers(eer, positiveControllers, negativeControllers)
    Tuple2(positiveControllers.toList, negativeControllers.toList)
  }

  private def fetchNestedControllers(
    eer: EntityEventRepresentation,
    positiveControllers:ListBuffer[String],
    negativeControllers:ListBuffer[String]): Boolean = {

    eer match {
      case se: SimpleEvent =>
        if(se.label.startsWith("De") || se.label == "Ubiquitination")
          false
        else if(se.label == "Translocation" && se.inputPointers.contains("theme")) {
          // for translocations, the element itself serves as its own controller
          positiveControllers += createOutput(se.manager.getEER(se.inputPointers("theme").head))
          true
        } else
          true
      case act: Activation =>
        if(act.polarity == "Positive") {
          for(c <- act.controller)
            positiveControllers += createOutput(c)
          true
        } else {
          for(c <- act.controller)
            negativeControllers += createOutput(c)
          false
        }
      case reg: Regulation =>
        var polarity =fetchNestedControllers(reg.controlled.head, positiveControllers, negativeControllers)
        if(reg.polarity == "Negative") polarity = ! polarity
        if(polarity) {
          for(c <- reg.controller)
            positiveControllers += createOutput(c)
        } else {
          for(c <- reg.controller)
            negativeControllers += createOutput(c)
        }
        polarity
      case _ => true
    }
  }

  override def getRows: Seq[_ <: AssemblyRow] = {

    val rows: Set[CMURow] = distinctEERS
      .map { event =>
        CMURow(
          createInput(event),
          createOutput(event),
          createSource(event),
          createDestination(event),
          createController(event),
          createNestedControllers(event),
          EERLUT(event.equivalenceHash(false)), // this means we are ignoring modifications in the equivalenceHash
          AssemblyExporter.getEventLabel(event),
          createMechanismType(event),
          precededBy(event),
          event.negated,
          event.evidence,
          event
        )
      }

    rows.toSeq
  }
}

object CMUExporter {

  // columns for the CMU tabular format
  val CMU_ELEMENT_NAME = "Element Name"
  val CMU_ELEMENT_TYPE = "Element Type"
  val CMU_DATABASE_NAME = "Database Name"
  val CMU_ELEMENT_IDENTIFIER = "Element Identifier"
  val CMU_LOCATION = "Location"
  val CMU_LOCATION_IDENTIFIER = "Location Identifier"
  val CMU_CELL_LINE = "Cell Line"
  val CMU_CELL_TYPE = "Cell Type"
  val CMU_ORGANISM = "Organism"
  val CMU_POS_REG_NAME = "PosReg Name"
  val CMU_POS_REG_TYPE = "PosReg Type"
  val CMU_POS_REG_ID = "PosReg ID"
  val CMU_POS_REG_LOCATION = "PosReg Location"
  val CMU_POS_REG_LOCATION_ID = "PosReg Location ID"
  val CMU_NEG_REG_NAME = "NegReg Name"
  val CMU_NEG_REG_TYPE = "NegReg Type"
  val CMU_NEG_REG_ID = "NegReg ID"
  val CMU_NEG_REG_LOCATION = "NegReg Location"
  val CMU_NEG_REG_LOCATION_ID = "NegReg Location ID"
  val CMU_IS_INDIRECT = "Interaction  Direct (D) or Indirect (I)"
  val CMU_MECHANISM_TYPE = "Mechanism Type for Direct"
  val CMU_PAPER_ID = "Paper ID"
  val CMU_EVIDENCE = "Evidence"

  val CMU_COLUMNS = Seq(
    CMU_ELEMENT_NAME,
    CMU_ELEMENT_TYPE,
    CMU_DATABASE_NAME,
    CMU_ELEMENT_IDENTIFIER,
    CMU_LOCATION,
    CMU_LOCATION_IDENTIFIER,
    CMU_CELL_LINE,
    CMU_CELL_TYPE,
    CMU_ORGANISM,
    CMU_POS_REG_NAME,
    CMU_POS_REG_TYPE,
    CMU_POS_REG_ID,
    CMU_POS_REG_LOCATION,
    CMU_POS_REG_LOCATION_ID,
    CMU_NEG_REG_NAME,
    CMU_NEG_REG_TYPE,
    CMU_NEG_REG_ID,
    CMU_NEG_REG_LOCATION,
    CMU_NEG_REG_LOCATION_ID,
    CMU_IS_INDIRECT,
    CMU_MECHANISM_TYPE,
    CMU_PAPER_ID,
    CMU_EVIDENCE
  )

  val ELEMENT_PATTERN = Pattern.compile("""([^:]+)::([^:]+):(.+)""", Pattern.CASE_INSENSITIVE)

  // FIXME: replace with an actual KB lookup from the subcellular location KB (Tom)
  val CMU_KNOWN_LOCATIONS:Map[String, String] = Map(
    "go:0005737" -> "cytoplasm",
    "go:0005886" -> "plasma membrane",
    "go:0005634" -> "nucleus",
    "go:0005739" -> "mitochondria",
    "go:0005576" -> "external",
    "go:0005783" -> "endoplasmic reticulum"
  )

  def cmuFilter(rows: Seq[AssemblyRow]): Seq[AssemblyRow] = removeChildren(keepEvents(rows)).distinct

  def keepEvents(rows: Seq[AssemblyRow]): Seq[AssemblyRow] = rows.filter { r =>
    // remove unseen
    (r.seen > 0) &&
    // keep only the events
    ExportFilters.isEvent(r) &&
    // CMU only cares about events that have a controller! (or Translocations)
    ExportFilters.hasController(r)
  }.distinct

  def removeChildren(rows: Seq[AssemblyRow]): Seq[AssemblyRow] = {
    // remove events that appear as input of other events in this set
    val children = new mutable.HashSet[String]()
    for(row <- rows)
      if(row.input != AssemblyExporter.NONE)
        children += row.input

    rows.filter { r => ! children.contains(r.eerID) }
  }.distinct

  def tabularOutput(mentions: Seq[Mention]): String = {
    val cmue: CMUExporter = createExporter(mentions)
    cmue.rowsToString(CMUExporter.CMU_COLUMNS, AssemblyExporter.SEP, cmuFilter)
  }

  def createExporter(mentions: Seq[Mention]): CMUExporter = {
    // perform deduplication
    val dedup = new DeduplicationSieves()
    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions)
    val am: AssemblyManager = orderedSieves.apply(mentions)
    val cmue = new CMUExporter(am)

    cmue
  }
}
