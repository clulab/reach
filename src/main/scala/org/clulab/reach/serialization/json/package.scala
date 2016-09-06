package org.clulab.reach.serialization

import org.clulab.odin._
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions._
import org.clulab.serialization.json.JSONSerialization
import org.json4s._
import org.json4s.JsonDSL._


package object json {

  private def argsAST(arguments: Map[String, Seq[Mention]]): JObject = {
    val args = arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(_.toBioMention.jsonAST).toList)
    }
    JObject(args.toList)
  }

  implicit val formats = org.json4s.DefaultFormats

  implicit class BioMentionOps(m: BioMention) extends JSONSerialization {
    def jsonAST: JValue = m match {
      case tb: BioTextBoundMention => BioTextBoundMentionOps(tb).jsonAST
      case em: BioEventMention => BioEventMentionOps(em).jsonAST
      case rm: BioRelationMention => BioRelationMentionOps(rm).jsonAST
    }

    // A mention only only contains a pointer to a document, so
    // create a Seq[Mention] whose jsonAST includes
    // an accompanying json map of docEquivHash -> doc's json
    def completeAST: JValue = Seq(m).jsonAST

    /**
      * Serialize mentions to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), Seq(m).json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  implicit class BioTextBoundMentionOps(btb: BioTextBoundMention) extends JSONSerialization {
    def jsonAST: JValue = {
      ("type" -> "BioTextBoundMention") ~
      ("labels" -> btb.labels) ~
      ("tokenInterval" -> Map("start" -> btb.tokenInterval.start, "end" -> btb.tokenInterval.end)) ~
      ("characterStartOffset" -> btb.startOffset) ~
      ("characterEndOffset" -> btb.endOffset) ~
      ("sentence" -> btb.sentence) ~
      ("document" -> btb.document.equivalenceHash.toString) ~
      ("keep" -> btb.keep) ~
      ("foundBy" -> btb.foundBy) ~
      ("modifications" -> btb.modifications.jsonAST) ~
      // grounding is optional
      ("grounding" -> btb.grounding.map(_.jsonAST)) ~
      // context is optional
      ("context" -> btb.context.map(_.jsonAST))
    }
  }

  implicit class BioEventMentionOps(bem: BioEventMention) extends JSONSerialization {
    def jsonAST: JValue = {
      ("type" -> "BioEventMention") ~
      ("labels" -> bem.labels) ~
      ("trigger" -> bem.trigger.asInstanceOf[BioTextBoundMention].jsonAST) ~
      ("arguments" -> argsAST(bem.arguments)) ~
      ("tokenInterval" -> Map("start" -> bem.tokenInterval.start, "end" -> bem.tokenInterval.end)) ~
      ("characterStartOffset" -> bem.startOffset) ~
      ("characterEndOffset" -> bem.endOffset) ~
      ("sentence" -> bem.sentence) ~
      ("document" -> bem.document.equivalenceHash.toString) ~
      ("keep" -> bem.keep) ~
      ("foundBy" -> bem.foundBy) ~
      ("modifications" -> bem.modifications.jsonAST) ~
      // grounding is optional
      ("grounding" -> bem.grounding.map(_.jsonAST)) ~
      // context is optional
      ("context" -> bem.context.map(_.jsonAST))
    }
  }

  implicit class BioRelationMentionOps(brm: BioRelationMention) extends JSONSerialization {
    def jsonAST: JValue = {
      ("type" -> "BioRelationMention") ~
      ("labels" -> brm.labels) ~
      ("arguments" -> argsAST(brm.arguments)) ~
      ("tokenInterval" -> Map("start" -> brm.tokenInterval.start, "end" -> brm.tokenInterval.end)) ~
      ("characterStartOffset" -> brm.startOffset) ~
      ("characterEndOffset" -> brm.endOffset) ~
      ("sentence" -> brm.sentence) ~
      ("document" -> brm.document.equivalenceHash.toString) ~
      ("keep" -> brm.keep) ~
      ("foundBy" -> brm.foundBy) ~
      ("modifications" -> brm.modifications.jsonAST) ~
      // grounding is optional
      ("grounding" -> brm.grounding.map(_.jsonAST)) ~
      // context is optional
      ("context" -> brm.context.map(_.jsonAST))
    }
  }

  /** For sequences of biomentions */
  implicit class BioMentionSeq(biomentions: Seq[BioMention]) extends JSONSerialization {

    def jsonAST: JValue = JSONSerializer.jsonAST(biomentions)

    /**
      * Serialize mentions to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), biomentions.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  implicit class ModificationOps(mod: Modification) extends JSONSerialization {
    def jsonAST: JValue = mod match {
      case PTM(label, evidenceOp, siteOp, negated) =>
        ("type" -> "PTM") ~
        ("label" -> label) ~
        // evidence is optional
        ("evidence" -> evidenceOp.map(_.toBioMention.jsonAST)) ~
        // site is optional
        ("site" -> siteOp.map(_.toBioMention.jsonAST)) ~
        ("negated" -> negated)
      case Mutant(evidence, foundBy) =>
        ("type" -> "Mutant") ~
        ("evidence" -> evidence.toBioMention.jsonAST) ~
        ("foundBy" -> foundBy)
      case EventSite(evidence) =>
        ("type" -> "EventSite") ~
        ("site" -> evidence.toBioMention.jsonAST)
      case Negation(evidence) =>
        ("type" -> "Negation") ~
        ("evidence" -> evidence.toBioMention.jsonAST)
      case Hypothesis(evidence) =>
        ("type" -> "Hypothesis") ~
        ("evidence" -> evidence.toBioMention.jsonAST)
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
}