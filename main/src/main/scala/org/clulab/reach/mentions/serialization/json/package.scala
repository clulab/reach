package org.clulab.reach.mentions.serialization

import org.clulab.odin
import org.clulab.odin._
import org.clulab.serialization.json.{ MentionOps => JSONMentionOps, _ }
import org.clulab.reach.mentions.serialization.json.{ JSONSerializer => ReachJSONSerializer }
import org.clulab.reach.mentions.{ MentionOps => MOps, _ }
import org.clulab.reach.grounding.KBResolution
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.io.File
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native._


package object json {

  /** generate the appropriate AST according to Mention type */
  def mentionToJsonAST(m: Mention): JValue = m match {
    // NOTE: order matters due to inheritance
    case cm: CorefMention => CorefMentionOps(cm).jsonAST
    case bm: BioMention => BioMentionOps(bm).jsonAST
    case m: Mention => org.clulab.serialization.json.MentionOps(m).jsonAST
  }

  /** args -> coref representation -> json */
  private def argsAST(arguments: Map[String, Seq[Mention]]): JObject = {
    val args = arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(mentionToJsonAST).toList)
    }
    JObject(args.toList)
  }

  implicit val formats = org.json4s.DefaultFormats

  /** BioMention -> json */
  implicit class BioMentionOps(m: BioMention) extends JSONMentionOps(m) {

    override def jsonAST: JValue = m match {
      case tb: BioTextBoundMention => BioTextBoundMentionOps(tb).jsonAST
      case em: BioEventMention => BioEventMentionOps(em).jsonAST
      case rm: BioRelationMention => BioRelationMentionOps(rm).jsonAST
    }

    /**
      * Serialize mentions to json file
      */
    override def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), Seq[BioMention](m).json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    override def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  /** CorefMention -> json */
  implicit class CorefMentionOps(m: CorefMention) extends JSONMentionOps(m) {

    override def jsonAST: JValue = m match {
      case tb: CorefTextBoundMention => CorefTextBoundMentionOps(tb).jsonAST
      case em: CorefEventMention => CorefEventMentionOps(em).jsonAST
      case rm: CorefRelationMention => CorefRelationMentionOps(rm).jsonAST
    }

    /**
      * Serialize mentions to json file
      */
    override def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), Seq[CorefMention](m).json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    override def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  def pathsAST(paths: Map[String, Map[Mention, odin.SynPath]]): JValue = paths match {
    case gps if gps.nonEmpty => gps.jsonAST
    case _ => JNothing
  }

  implicit class BioTextBoundMentionOps(tb: BioTextBoundMention) extends TextBoundMentionOps(tb) {
    override def jsonAST: JValue = {

      val ast = TextBoundMentionOps(tb).jsonAST replace
        (List("type"), BioTextBoundMention.string) replace
        (List("id"), tb.id)

      ast merge (
        ("modifications" -> tb.modifications.jsonAST) ~
        // grounding is optional
        ("grounding" -> tb.grounding.map(_.jsonAST)) ~
        // context is optional
        ("context" -> tb.context.map(_.jsonAST)) ~
        // usually just labels.head...
        ("displayLabel" -> tb.displayLabel)
        )
    }
  }

  implicit class BioEventMentionOps(em: BioEventMention) extends EventMentionOps(em) {
    override def jsonAST: JValue = {

      val ast = EventMentionOps(em).jsonAST replace
        (List("type"), BioEventMention.string) replace
        (List("id"), em.id) replace
        (List("arguments"), argsAST(em.arguments))

      ast merge (
        ("modifications" -> em.modifications.jsonAST) ~
        // grounding is optional
        ("grounding" -> em.grounding.map(_.jsonAST)) ~
        // context is optional
        ("context" -> em.context.map(_.jsonAST)) ~
        // usually just labels.head...
        ("displayLabel" -> em.displayLabel) ~
        ("isDirect" -> em.isDirect)
        )
    }
  }

  implicit class BioRelationMentionOps(rm: BioRelationMention) extends RelationMentionOps(rm) {
    override def jsonAST: JValue = {

      val ast = RelationMentionOps(rm).jsonAST replace
        (List("type"), BioRelationMention.string) replace
        (List("id"), rm.id) replace
        (List("arguments"), argsAST(rm.arguments))

      ast merge (
        ("modifications" -> rm.modifications.jsonAST) ~
        // grounding is optional
        ("grounding" -> rm.grounding.map(_.jsonAST)) ~
        // context is optional
        ("context" -> rm.context.map(_.jsonAST)) ~
        // usually just labels.head...
        ("displayLabel" -> rm.displayLabel)
        )
    }
  }

  implicit class CorefTextBoundMentionOps(tb: CorefTextBoundMention) extends BioTextBoundMentionOps(tb) {
    override def jsonAST: JValue = {

      val ast = BioTextBoundMentionOps(tb).jsonAST replace
        (List("type"), CorefTextBoundMention.string) replace
        (List("id"), tb.id)

      ast merge (
        ("antecedents" -> tb.antecedents.jsonAST) ~
        ("sieves" -> tb.sieves.jsonAST)
        )
    }
  }

  implicit class CorefEventMentionOps(em: CorefEventMention) extends BioEventMentionOps(em) {
    override def jsonAST: JValue = {

      val ast = BioEventMentionOps(em).jsonAST replace
        (List("type"), CorefEventMention.string) replace
        (List("id"), em.id) replace
        (List("arguments"), argsAST(em.arguments))

      ast merge (
        ("antecedents" -> em.antecedents.jsonAST) ~
        ("sieves" -> em.sieves.jsonAST)
        )
    }
  }

  implicit class CorefRelationMentionOps(rm: CorefRelationMention) extends BioRelationMentionOps(rm) {
    override def jsonAST: JValue = {

      val ast = BioRelationMentionOps(rm).jsonAST replace
        (List("type"), CorefRelationMention.string) replace
        (List("id"), rm.id) replace
        (List("arguments"), argsAST(rm.arguments))

      ast merge (
        ("antecedents" -> rm.antecedents.jsonAST) ~
        ("sieves" -> rm.sieves.jsonAST)
        )
    }
  }

  /** For Seq[BioMention] */
  implicit class BioMentionSeq(biomentions: Seq[BioMention]) extends MentionSeq(biomentions) {

    override def jsonAST: JValue = ReachJSONSerializer.jsonAST(biomentions)

    /**
      * Serialize mentions to json file
      */
    override def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), biomentions.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    override def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  /** For Seq[CorefMention] */
  implicit class CorefMentionSeq(corefmentions: Seq[CorefMention]) extends BioMentionSeq(corefmentions) {

    override def jsonAST: JValue = ReachJSONSerializer.jsonAST(corefmentions)

    /**
      * Serialize mentions to json file
      */
    override def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), corefmentions.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    override def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  implicit class ModificationOps(mod: Modification) extends JSONSerialization {
    def jsonAST: JValue = mod match {
      case PTM(label, evidenceOp, siteOp, negated) =>
        ("modification-type" -> "PTM") ~
        ("label" -> label) ~
        // evidence is optional
        ("evidence" -> evidenceOp.map(mentionToJsonAST)) ~
        // site is optional
        ("site" -> siteOp.map(mentionToJsonAST)) ~
        ("negated" -> negated)
      case Mutant(evidence, foundBy) =>
        ("modification-type" -> "Mutant") ~
        ("evidence" -> mentionToJsonAST(evidence)) ~
        ("foundBy" -> foundBy)
      case EventSite(evidence) =>
        ("modification-type" -> "EventSite") ~
        ("site" -> mentionToJsonAST(evidence))
      case Negation(evidence) =>
        ("modification-type" -> "Negation") ~
        ("evidence" -> mentionToJsonAST(evidence))
      case Hypothesis(evidence) =>
        ("modification-type" -> "Hypothesis") ~
        ("evidence" -> mentionToJsonAST(evidence))
    }
  }

  implicit class ModificationsOps(mods: Set[Modification]) extends JSONSerialization {
    def jsonAST: JValue = mods match {
      case hasMods if hasMods.nonEmpty => hasMods.map(_.jsonAST).toList
      case _ => JNothing
    }
  }

  implicit class KBResolutionOps(kbr: KBResolution) extends JSONSerialization {
    def jsonAST: JValue = {
      // components needed to construct KBEntry (KBResolution.entry)
      ("text" -> kbr.entry.text) ~
      ("key" -> kbr.entry.key) ~
      ("namespace" -> kbr.entry.namespace) ~
      ("id" -> kbr.entry.id) ~
      ("species" -> kbr.entry.species)
    }
  }

  implicit class ContextOps(context: Map[String, Seq[String]]) extends JSONSerialization {
    def jsonAST: JValue = context match {
      case hasContext if hasContext.nonEmpty => hasContext
      case _ => JNothing
    }
  }

  implicit class AnaphoricOps(antecedents: Set[Anaphoric]) extends JSONSerialization {
    def jsonAST: JValue = antecedents match {
      case hasAntecedents if hasAntecedents.nonEmpty => hasAntecedents.map(m => m.asInstanceOf[CorefMention].jsonAST)
      case _ => JNothing
    }
  }

  implicit class StringSetOps(ss: Set[String]) extends JSONSerialization {
    def jsonAST: JValue = ss match {
      case contents if contents.nonEmpty => contents
      case _ => JNothing
    }
  }

  def prettify(json: JValue): String = prettyJson(renderJValue(json))

  object BioTextBoundMention {
    val string = "BioTextBoundMention"
    val shortString = "T"
  }

  object BioEventMention {
    val string = "BioEventMention"
    val shortString = "E"
  }

  object BioRelationMention {
    val string = "BioRelationMention"
    val shortString = "R"
  }

  object CorefTextBoundMention {
    val string = "CorefTextBoundMention"
    val shortString = "T"
  }

  object CorefEventMention {
    val string = "CorefEventMention"
    val shortString = "E"
  }

  object CorefRelationMention {
    val string = "CorefRelationMention"
    val shortString = "R"
  }
}