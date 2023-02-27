package org.clulab.reach.mentions.serialization

import org.clulab.odin
import org.clulab.odin._
import org.clulab.odin.serialization.json.{EventMention, EventMentionOps, OdinPathOps, RelationMentionOps, TextBoundMention, TextBoundMentionOps, MentionOps => OdinMentionOps}
import org.clulab.serialization.json.{Equivalency, JSONSerialization}
import org.clulab.reach.mentions.serialization.json.{JSONSerializer => ReachJSONSerializer}
import org.clulab.reach.mentions._
import org.clulab.reach.grounding.KBResolution
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson._

import scala.util.hashing.MurmurHash3._


package object json {

  implicit val formats = org.json4s.DefaultFormats

  /** generate the appropriate AST according to Mention type */
  private def mentionToJsonAST(m: Mention): JValue = m match {
    // NOTE: order matters due to inheritance
    case cm: CorefMention => CorefMentionOps(cm).jsonAST
    case bm: BioMention => BioMentionOps(bm).jsonAST
    case m: Mention => OdinMentionOps(m).jsonAST
  }

  implicit class MentionJSONOps(m: Mention) extends OdinMentionOps(m) {

    /** Without "documents" field **/
    override def jsonAST: JValue = mentionToJsonAST(m)

    /** Includes "documents" field for simple deserialization **/
    override def completeAST: JValue = REACHMentionSeq(Seq(m)).jsonAST

  }

  /** For Seq[BioMention], Seq[CorefMention], etc */
  implicit class REACHMentionSeq(mentions: Seq[Mention]) extends JSONSerialization {

    override def jsonAST: JValue = ReachJSONSerializer.jsonAST(mentions)

  }

  /** generate a json string from the given ast */
  def astToJSON(jsonast: JValue, pretty: Boolean): String = {
    val jsonDoc = renderJValue(jsonast)
    pretty match {
      case true => prettyJson(jsonDoc)
      case false => compactJson(jsonDoc)
    }
  }

  /** generate a json string from a mention <br>
    * Note that this is incomplete for deserialization purposes,
    * as only a reference to the Document is included
    * */
  def mentionToJSON(m: Mention, pretty: Boolean): String = astToJSON(mentionToJsonAST(m), pretty)

  /** args -> coref representation -> json */
  private def argsAST(arguments: Map[String, Seq[Mention]]): JObject = {
    val args = arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(mentionToJsonAST).toList)
    }
    JObject(args.toList)
  }

  /** BioMention -> json */
  implicit class BioMentionOps(m: BioMention) extends JSONSerialization {

    override def jsonAST: JValue = m match {
      case tb: BioTextBoundMention => BioTextBoundMentionOps(tb).jsonAST
      case em: BioEventMention => BioEventMentionOps(em).jsonAST
      case rm: BioRelationMention => BioRelationMentionOps(rm).jsonAST
    }
  }

  /** CorefMention -> json */
  implicit class CorefMentionOps(m: CorefMention) extends JSONSerialization {

    override def jsonAST: JValue = m match {
      case tb: CorefTextBoundMention => CorefTextBoundMentionOps(tb).jsonAST
      case em: CorefEventMention => CorefEventMentionOps(em).jsonAST
      case rm: CorefRelationMention => CorefRelationMentionOps(rm).jsonAST
    }
  }

  def pathsAST(paths: Map[String, Map[Mention, odin.SynPath]]): JValue = paths match {
    case gps if gps.nonEmpty => gps.jsonAST
    case _ => JNothing
  }

  implicit class BioTextBoundMentionOps(tb: BioTextBoundMention) extends TextBoundMentionOps(tb) {

    override def jsonAST: JValue = {
      val ast = super.jsonAST
          .replace(List("type"), BioTextBoundMention.string)

      ast merge (
        ("modifications" -> tb.modifications.jsonAST) ~
        // grounding is optional
        ("grounding" -> tb.grounding.map(_.jsonAST)) ~
        // context is optional
        ("context" -> tb.contextOpt.map(_.jsonAST)) ~
        // usually just labels.head...
        ("displayLabel" -> tb.displayLabel)
      )
    }
  }

  implicit class BioEventMentionOps(em: BioEventMention) extends EventMentionOps(em) {

    override def triggerJsonAST: JValue = mentionToJsonAST(em.trigger)

    override def argumentsJsonAST: JObject = argsAST(em.arguments)

    override def jsonAST: JValue = {
      val ast = super.jsonAST
          .replace(List("type"), BioEventMention.string)

      ast merge (
        ("modifications" -> em.modifications.jsonAST) ~
        // grounding is optional
        ("grounding" -> em.grounding.map(_.jsonAST)) ~
        // context is optional
        ("context" -> em.contextOpt.map(_.jsonAST)) ~
        // usually just labels.head...
        ("displayLabel" -> em.displayLabel) ~
        ("isDirect" -> em.isDirect)
      )
    }
  }

  implicit class BioRelationMentionOps(rm: BioRelationMention) extends RelationMentionOps(rm) {

    override def argumentsJsonAST: JObject = argsAST(rm.arguments)

    override def jsonAST: JValue = {
      val ast = super.jsonAST
          .replace(List("type"), BioRelationMention.string)

      ast merge (
        ("modifications" -> rm.modifications.jsonAST) ~
        // grounding is optional
        ("grounding" -> rm.grounding.map(_.jsonAST)) ~
        // context is optional
        ("context" -> rm.contextOpt.map(_.jsonAST)) ~
        // usually just labels.head...
        ("displayLabel" -> rm.displayLabel)
      )
    }
  }

  implicit class CorefTextBoundMentionOps(tb: CorefTextBoundMention) extends BioTextBoundMentionOps(tb) {

    override def jsonAST: JValue = {
      val ast = super.jsonAST
          .replace(List("type"), CorefTextBoundMention.string)

      ast merge (
        ("antecedents" -> tb.antecedents.jsonAST) ~
        ("sieves" -> tb.sieves.jsonAST)
      )
    }
  }

  implicit class CorefEventMentionOps(em: CorefEventMention) extends BioEventMentionOps(em) {

    override def jsonAST: JValue = {
      val ast = super.jsonAST
          .replace(List("type"), CorefEventMention.string)

      ast merge (
        ("antecedents" -> em.antecedents.jsonAST) ~
        ("sieves" -> em.sieves.jsonAST)
      )
    }
  }

  implicit class CorefRelationMentionOps(rm: CorefRelationMention) extends BioRelationMentionOps(rm) {

    override def jsonAST: JValue = {
      val ast = super.jsonAST
          .replace(List("type"), CorefRelationMention.string)

      ast merge (
        ("antecedents" -> rm.antecedents.jsonAST) ~
        ("sieves" -> rm.sieves.jsonAST)
      )
    }
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
      case eventSite: EventSite =>
        // This is an exception because of the "site" label.
        ("modification-type" -> eventSite.label) ~
        ("site" -> mentionToJsonAST(eventSite.site))
      case simpleModification: SimpleModification =>
        ("modification-type" -> simpleModification.label) ~
        ("evidence" -> mentionToJsonAST(simpleModification.mention))
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
      // components needed to construct KBResolution
      ("text" -> kbr.text) ~
      ("namespace" -> kbr.namespace) ~
      ("id" -> kbr.id) ~
      ("species" -> kbr.species)
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
