package edu.arizona.sista.assembly

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
  precededBy: Seq[IDPointer],
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
    val precedingEvents = precededBy.distinct.sorted.mkString(", ")
    val seenIn = docIDs.toSeq.sorted.mkString(", ")
    val examples = getTextualEvidence.mkString(" ++++ ")
    s"$input\t$output\t$controller\t$eventID\t$eventLabel\t$precedingEvents\t$negated\t$seen\t$examples\t$seenIn"
  }
}

class AssemblyExporter(val manager: AssemblyManager) {

  import AssemblyExporter._

  // distinct EntityEventRepresentations
  val distinctEERS = manager.distinctEEReprs

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

        val text = getText(entity)
       s"$text::${entity.grounding}$mutantForms$features"

    case complex: Complex =>
      complex.members.map(createInput).mkString(", ")

    case se: SimpleEvent =>
      se.input.values.flatten.map(createInput).mkString(", ")

    case reg: Regulation =>
      // get event IDs for each controlled
      reg.controlled.map(c => EERLUT.getOrElse(c.equivalenceHash, reportError(reg, c))).mkString(", ")
  }

  def reportError(reg: Regulation, c: EntityEventRepresentation): String = {
    println(s"$c is a controlled of a regulation but it is not a Complex or Event!")
    val labels = c.evidence.map(_.label)
    val evidence = reg.evidence.map(_.text)
    println(s"Evidence for reg: $evidence")
    println(s"Evidence for controlled: ${c.evidence.map(_.text)}")
    println(s"Labels assigned to evidence: $labels")
    "???"
  }
  def createOutput(repr: EntityEventRepresentation): String = repr match {

    case complex: Complex =>
      complex.members.map(createInput).mkString("{", ", ", "}")

    case se: SimpleEvent =>
      se.output.map{
        case binding: Complex => createOutput(binding)
        case other => createInput(other)
      }.mkString(", ")

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

  def writeTSV(outfile: String, rowFilter: Set[Row] => Set[Row]): Unit = {
    val f = new File(outfile)
    val header = s"INPUT\tOUTPUT\tCONTROLLER\tEVENT ID\tEVENT LABEL\tPRECEDED BY\tNEGATED?\tSEEN\tEVIDENCE\tSEEN IN\n"
    val rowsForOutput = rowFilter(getEventRows)
    val text =
      // only events
      rowsForOutput
      .toSeq
      .sortBy(r => (r.eventLabel, -r.docIDs.size, -r.seen))
      .map(_.toTSVrow)
      .mkString("\n")

    // make sure the output is valid
    val regs = rowsForOutput.filter(_.eventLabel == "Regulation")
    val problems = regs.filter(r => ! rowsForOutput.exists(_.eventID == r.input))
    require(problems.isEmpty, "Regulation input ID not found in EventIDs for rows!")
    // write the output to disk
    FileUtils.writeStringToFile(f, header + text)
  }

  def getEventLabel(e: EntityEventRepresentation): String = e match {
    case binding: Complex => "Binding"
    case reg: Regulation => "Regulation"
    case se: SimpleEvent => se.label
    case ptm: SimpleEntity if ptm.modifications.exists(_.isInstanceOf[PTM]) =>
      ptm.modifications.find(_.isInstanceOf[PTM]).get.asInstanceOf[PTM].label
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

  def getEventRows: Set[Row] = {
    // TODO: change to partition and filter so that its EventID can't be referenced by a Regulation
    getRows.filter(_.eventLabel != "entity")
  }
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

  def getPTMrepresentation(ptm: PTM): String = {
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
  def containsFamily(m: Mention): Boolean = {
    m match {
      case entity if entity matches "Entity" =>
        entity matches "Family"
      case site if site matches "Site" => false
      case event if event matches "Event" =>
        event.arguments.values.flatten.exists(containsFamily)
    }
  }

  /**
   * Applies MITRE requirements to assembled events
   * @param rows a Set[Row]
   * @return a filtered Set of [[Row]]
   */
  def MITREfilter(rows: Set[Row]): Set[Row] = {
    // 1a. A finding is to be reported only if it is supported by >= 3 different examples.
    rows.filter(_.seen >= 3)
      // 1b. Evidence come from at least 2 different sections.
      .filter(_.docIDs.toSet.size >= 2)
      // 2. Findings cannot include protein families.
      .filter(r => r.evidence.forall(e => ! containsFamily(e)))
      // 3. Findings should not have an unresolved (UAZ) grounding.
      // FIXME: probably this should operate over EERs, not their evidence
      .filter(r => r.evidence.forall(e => ! hasUAZgrounding(e)))
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