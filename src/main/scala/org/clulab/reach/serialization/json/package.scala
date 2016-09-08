package org.clulab.reach.serialization

import org.clulab.odin
import org.clulab.odin._
import org.clulab.serialization.json.{ MentionOps => JSONMentionOps, _ }
import org.clulab.serialization.json.{ JSONSerializer => JSONSer }
import org.clulab.reach.mentions.{ MentionOps => MOps, _ }
import org.clulab.reach.grounding.KBResolution
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.io.File
import org.json4s.JsonDSL._
import org.json4s._


package object json {

  private def argsAST(arguments: Map[String, Seq[Mention]]): JObject = {
    val args = arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(_.toCorefMention.jsonAST).toList)
    }
    JObject(args.toList)
  }

  implicit val formats = org.json4s.DefaultFormats

  implicit class CorefMentionOps(m: CorefMention) extends JSONMentionOps(m) {

    override def jsonAST: JValue = m match {
      case tb: CorefTextBoundMention => CorefTextBoundMentionOps(tb).jsonAST
      case em: CorefEventMention => CorefEventMentionOps(em).jsonAST
      case rm: CorefRelationMention => CorefRelationMentionOps(rm).jsonAST
    }

    // A mention only only contains a pointer to a document, so
    // create a Seq[Mention] whose jsonAST includes
    // an accompanying json map of docEquivHash -> doc's json
    override def completeAST: JValue = Seq(m).jsonAST

    /**
      * Serialize mentions to json file
      */
    override def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), Seq(m).json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    override def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  def pathsAST(paths: Map[String, Map[Mention, odin.SynPath]]): JValue = paths match {
    case gps if gps.nonEmpty => gps.jsonAST
    case _ => JNothing
  }

  implicit class CorefTextBoundMentionOps(tb: CorefTextBoundMention) extends TextBoundMentionOps(tb) {
    override def jsonAST: JValue = {
      ("type" -> CorefTextBoundMention.string) ~
      // used for paths map
      ("id" -> tb.id) ~
      ("text" -> tb.text) ~
      ("labels" -> tb.labels) ~
      ("tokenInterval" -> Map("start" -> tb.tokenInterval.start, "end" -> tb.tokenInterval.end)) ~
      ("characterStartOffset" -> tb.startOffset) ~
      ("characterEndOffset" -> tb.endOffset) ~
      ("sentence" -> tb.sentence) ~
      ("document" -> tb.document.equivalenceHash.toString) ~
      ("keep" -> tb.keep) ~
      ("foundBy" -> tb.foundBy) ~
      ("modifications" -> tb.modifications.jsonAST) ~
      // grounding is optional
      ("grounding" -> tb.grounding.map(_.jsonAST)) ~
      // context is optional
      ("context" -> tb.context.map(_.jsonAST)) ~
      // usually just labels.head...
      ("displayLabel" -> tb.displayLabel) ~
      ("antecedents" -> tb.antecedents.jsonAST) ~
      ("sieves" -> tb.sieves)
    }
  }

  implicit class CorefEventMentionOps(em: CorefEventMention) extends EventMentionOps(em) {
    override def jsonAST: JValue = {
      ("type" -> CorefEventMention.string) ~
      // used for paths map
      ("id" -> em.id) ~
      ("text" -> em.text) ~
      ("labels" -> em.labels) ~
      ("trigger" -> em.trigger.asInstanceOf[CorefTextBoundMention].jsonAST) ~
      ("paths" -> pathsAST(em.paths)) ~
      ("arguments" -> argsAST(em.arguments)) ~
      ("tokenInterval" -> Map("start" -> em.tokenInterval.start, "end" -> em.tokenInterval.end)) ~
      ("characterStartOffset" -> em.startOffset) ~
      ("characterEndOffset" -> em.endOffset) ~
      ("sentence" -> em.sentence) ~
      ("document" -> em.document.equivalenceHash.toString) ~
      ("keep" -> em.keep) ~
      ("foundBy" -> em.foundBy) ~
      ("modifications" -> em.modifications.jsonAST) ~
      // grounding is optional
      ("grounding" -> em.grounding.map(_.jsonAST)) ~
      // context is optional
      ("context" -> em.context.map(_.jsonAST)) ~
      // usually just labels.head...
      ("displayLabel" -> em.displayLabel) ~
      ("isDirect" -> em.isDirect) ~
      ("antecedents" -> em.antecedents.jsonAST) ~
      ("sieves" -> em.sieves)
    }
  }

  implicit class CorefRelationMentionOps(rm: CorefRelationMention) extends RelationMentionOps(rm) {
    override def jsonAST: JValue = {
      ("type" -> CorefRelationMention.string) ~
      // used for paths map
      ("id" -> rm.id) ~
      ("text" -> rm.text) ~
      ("labels" -> rm.labels) ~
      ("arguments" -> argsAST(rm.arguments)) ~
      ("paths" -> pathsAST(rm.paths)) ~
      ("tokenInterval" -> Map("start" -> rm.tokenInterval.start, "end" -> rm.tokenInterval.end)) ~
      ("characterStartOffset" -> rm.startOffset) ~
      ("characterEndOffset" -> rm.endOffset) ~
      ("sentence" -> rm.sentence) ~
      ("document" -> rm.document.equivalenceHash.toString) ~
      ("keep" -> rm.keep) ~
      ("foundBy" -> rm.foundBy) ~
      ("modifications" -> rm.modifications.jsonAST) ~
      // grounding is optional
      ("grounding" -> rm.grounding.map(_.jsonAST)) ~
      // context is optional
      ("context" -> rm.context.map(_.jsonAST)) ~
      // usually just labels.head...
      ("displayLabel" -> rm.displayLabel) ~
      ("antecedents" -> rm.antecedents.jsonAST) ~
      ("sieves" -> rm.sieves)
    }
  }

  /** For sequences of biomentions */
  implicit class CorefMentionSeq(corefmentions: Seq[CorefMention]) extends MentionSeq(corefmentions) {

    override def jsonAST: JValue = JSONSer.jsonAST(corefmentions)

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
        ("type" -> "PTM") ~
        ("label" -> label) ~
        // evidence is optional
        ("evidence" -> evidenceOp.map(_.toCorefMention.jsonAST)) ~
        // site is optional
        ("site" -> siteOp.map(_.toCorefMention.jsonAST)) ~
        ("negated" -> negated)
      case Mutant(evidence, foundBy) =>
        ("type" -> "Mutant") ~
        ("evidence" -> evidence.toCorefMention.jsonAST) ~
        ("foundBy" -> foundBy)
      case EventSite(evidence) =>
        ("type" -> "EventSite") ~
        ("site" -> evidence.toCorefMention.jsonAST)
      case Negation(evidence) =>
        ("type" -> "Negation") ~
        ("evidence" -> evidence.toCorefMention.jsonAST)
      case Hypothesis(evidence) =>
        ("type" -> "Hypothesis") ~
        ("evidence" -> evidence.toCorefMention.jsonAST)
    }
  }

  implicit class ModificationsOps(mods: Set[Modification]) extends JSONSerialization {
    def jsonAST: JValue = mods.map(_.jsonAST).toList
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
    def jsonAST: JValue = context
  }

  implicit class AnaphoricOps(antecedents: Set[Anaphoric]) extends JSONSerialization {
    def jsonAST: JValue = antecedents.map(m => m.asInstanceOf[CorefMention].jsonAST)
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