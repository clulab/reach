package edu.arizona.sista.assembly

import edu.arizona.sista.odin.Mention
import java.io.File
import org.apache.commons.io.FileUtils

case class Row(
  input: String,
  output: String,
  controller: String,
  eventLabel: String,
  precededBy: Seq[IDPointer],
  negated: Boolean,
  evidence: Set[Mention]
) {
  // this might serve as a proxy for confidence, though
  // it would also be good to know how many times this event
  // was involved in other events (input, controller, etc)
  val seen = evidence.size
  // the set of paper ids where mentions of this event were found
  val papers = evidence.map(_.document.id.getOrElse("UNKNOWN"))

  def getTextualEvidence: Seq[String] = {
    evidence.toSeq.map(_.sentenceObj.getSentenceText)
  }

  def toTSVrow: String = {
    val precedingEvents = precededBy.distinct.sorted.mkString(", ")
    val seenIn = papers.toSeq.sorted.mkString(", ")
    val examples = getTextualEvidence.mkString(" ++++ ")
    s"$input\t$output\t$controller\t$eventLabel$precedingEvents\t$seen\t$examples\t$seenIn"
  }
}

class AssemblyExporter(am: AssemblyManager) {

  import AssemblyExporter._

  val distinctEERs = am.distinctEEReprs

  // LUT for retrieving unique IDs
  val EERLUT: Map[Int, IDPointer] = distinctEERs.zipWithIndex.map{
    case pair =>
      val repr: EntityEventRepresentation = pair._1
      val id: Int = pair._2
      (repr.equivalenceHash, id)
  }.toMap

  val grounding2Text: Map[GroundingID, String] = {
    val pairs: Set[(GroundingID, String)] = for {
      pair <- am.distinctSimpleEntitiesWithEvidence
      entity: SimpleEntity = pair._1
      evidence: Set[Mention] = pair._2
      id: GroundingID = entity.grounding
      text: String = if (evidence.nonEmpty) s"${evidence.map(_.text.toLowerCase).max}::$id" else s"???::$id"
    } yield (id, text)

    pairs.toMap
  }

  def createInput(repr: EntityEventRepresentation): String = repr match {

    case entity: SimpleEntity =>

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
        val ptms: Set[PTM] = other.filter(_.isInstanceOf[PTM])map(_.asInstanceOf[PTM])
        val features: String = ptms
          .map(getPTMrepresentation)
          .mkString(".")

        val text = grounding2Text.getOrElse(entity.grounding, entity.grounding)
       s"$text$mutantForms$features"

    case complex: Complex =>
      complex.members.map(createInput).mkString(", ")

    case se: SimpleEvent =>
      se.input.values.flatten.map(createInput).mkString(", ")

    case reg: Regulation =>
      // get event IDs for each controlled
      reg.controlled.map(c => EERLUT(c.equivalenceHash)).mkString(", ")
  }

  def createOutput(repr: EntityEventRepresentation): String = repr match {

    case complex: Complex =>
      complex.members.map(createInput).mkString("{", ", ", "}")

    case se: SimpleEvent =>
      se.output.map(createInput).mkString(", ")

    case reg: Regulation =>
      reg.controlled.map(createOutput).mkString(", ")

    case _ => ""
  }

  def createController(repr: EntityEventRepresentation): String = repr match {

    case reg: Regulation =>
      reg.controller.map{ c =>
        s"${createInput(c)}.${reg.polarity}"
      }.mkString(", ")

    case _ => ""

  }

  // TODO: implement a stub in AssemblyManager
  def precededBy(repr: EntityEventRepresentation): Seq[IDPointer] = {
    Nil
  }

  def writeTSV(outfile: String): Unit = {
    val f = new File(outfile)
    val text = getRows
      .map(_.toString)
      .toSeq
      .sorted
      .mkString("\n")
    FileUtils.writeStringToFile(f, text)
  }

  // INPUT, OUTPUT, CONTROLLER, PRECEDED BY, EVENT ID, SEEN, EXAMPLE-TEXT
  def getRows: Set[Row] = {

      val rows: Set[Row] = distinctEERs
        // ignore SimpleEntities not in other events
        .filter(r => ! r.isInstanceOf[SimpleEntity])
        .map { event =>
          Row(
            createInput(event),
            createOutput(event),
            createController(event),
            precededBy(event),
            event.negated,
            event.evidence
         )
      }
    rows
    }
}

object AssemblyExporter {

  val PTMLUT: Map[String, String] = Map(
      "Phosphorylation" -> "p",
      "Ubiquitination" -> "u"
  )

  def getPTMrepresentation(ptm: PTM): String = {
    // attempt to retrieve the abbreviated form of the label
    // if the key is missing from the LUT,
    // return the lowercase form of the first letter of the PTM's label
    PTMLUT.getOrElse(ptm.label, ptm.label.toLowerCase.head.toString)
  }
}