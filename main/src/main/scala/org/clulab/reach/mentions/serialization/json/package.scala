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


  implicit class LocalTextBoundMentionOps(tb: TextBoundMention) extends JSONSerialization with Equivalency {

    val stringCode = s"org.clulab.odin.${TextBoundMention.string}"

    def equivalenceHash: Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, tb.labels.hashCode)
      // interval.start
      val h2 = mix(h1, tb.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, tb.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, tb.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, 5) // tb.document.equivalenceHash) // cache these
      finalizeHash(h5, 5)
    }

    override def id: String = s"${TextBoundMention.shortString}:$equivalenceHash"

    def jsonAST: JValue = {
      ("type" -> TextBoundMention.string) ~
        // used for correspondence with paths map
        ("id" -> id) ~ // do not call tb.id because it will just created another of these
        ("text" -> tb.text) ~
        ("labels" -> tb.labels) ~
        ("tokenInterval" -> Map("start" -> tb.tokenInterval.start, "end" -> tb.tokenInterval.end)) ~
        ("characterStartOffset" -> tb.startOffset) ~
        ("characterEndOffset" -> tb.endOffset) ~
        ("sentence" -> tb.sentence) ~
        ("document" -> "keith") ~ // tb.document.equivalenceHash.toString) ~ // cache these
        ("keep" -> tb.keep) ~
        ("foundBy" -> tb.foundBy)
    }
  }

  implicit class BioTextBoundMentionOps(tb: BioTextBoundMention) extends TextBoundMentionOps(tb) {

//    override val stringCode = s"org.clulab.odin.${BioTextBoundMention.string}"
//    override def id: String = s"${BioTextBoundMention.shortString}:$equivalenceHash"

    override def jsonAST: JValue = {

      val ast = LocalTextBoundMentionOps(tb).jsonAST replace
        (List("type"), BioTextBoundMention.string) replace
        (List("id"), tb.id)

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

  implicit class LocalEventMentionOps(em: EventMention) extends JSONSerialization with Equivalency {

    private def argsHash(args: Map[String, Seq[Mention]]): Int = {
      import org.clulab.odin.serialization.json.MentionOps
      // TODO: fix this

      val argHashes = for {
        (role, mns) <- args
        bh = stringHash(s"role:$role")
        hs = mns.map( _ => 5) // TODO _.equivalenceHash)
      } yield mix(bh, unorderedHash(hs))
      val h0 = stringHash("org.clulab.odin.Mention.arguments")
      finalizeHash(h0, unorderedHash(argHashes))
    }

    val stringCode = s"org.clulab.odin.${EventMention.string}"

    def equivalenceHash: Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, em.labels.hashCode)
      // interval.start
      val h2 = mix(h1, em.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, em.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, em.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, em.document.equivalenceHash)
      // args
      val h6 = mix(h5, argsHash(em.arguments))
      // trigger
      val h7 = mix(h6, TextBoundMentionOps(em.trigger).equivalenceHash)
      finalizeHash(h7, 7)
    }

    override def id: String = s"${EventMention.shortString}:$equivalenceHash"

    def jsonAST: JValue = {
      ("type" -> EventMention.string) ~
        // used for paths map
        ("id" -> id) ~
        ("text" -> em.text) ~
        ("labels" -> em.labels) ~
        ("trigger" -> "dean") ~ // em.trigger.jsonAST) ~ // TODO
        ("arguments" -> argsAST(em.arguments)) ~
        // paths are encoded as (arg name -> (mentionID -> path))
        ("paths" -> pathsAST(em.paths)) ~
        ("tokenInterval" -> Map("start" -> em.tokenInterval.start, "end" -> em.tokenInterval.end)) ~
        ("characterStartOffset" -> em.startOffset) ~
        ("characterEndOffset" -> em.endOffset) ~
        ("sentence" -> em.sentence) ~
        ("document" -> "brad") ~ // em.document.equivalenceHash.toString) ~
        ("keep" -> em.keep) ~
        ("foundBy" -> em.foundBy)
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
        ("context" -> em.contextOpt.map(_.jsonAST)) ~
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
        ("context" -> rm.contextOpt.map(_.jsonAST)) ~
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
