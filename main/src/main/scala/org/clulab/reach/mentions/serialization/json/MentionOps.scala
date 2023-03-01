package org.clulab.reach.mentions.serialization.json

import org.clulab.odin.Mention
import org.clulab.odin.serialization.json.{EventMentionOps, RelationMentionOps, TextBoundMentionOps}
import org.clulab.odin.serialization.json.{MentionOps => OdinMentionOps}
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions.{Anaphoric, CorefMention, EventSite, Modification, Mutant, PTM, SimpleModification}
import org.clulab.reach.mentions.{BioEventMention, BioRelationMention, BioTextBoundMention}
import org.clulab.reach.mentions.{CorefEventMention, CorefRelationMention, CorefTextBoundMention}
import org.clulab.serialization.json.JSONSerialization
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson._

import ReachImplicits._

object MentionOps {
  implicit val formats = org.json4s.DefaultFormats

  def apply(mention: Mention): OdinMentionOps = {
    mention match {
      // Corefs must be on top because they inherit from the Bio classes.
      case mention: CorefTextBoundMention => new CorefTextBoundMentionOps(mention)
      case mention: CorefEventMention => new CorefEventMentionOps(mention)
      case mention: CorefRelationMention => new CorefRelationMentionOps(mention)

      case mention: BioTextBoundMention => new BioTextBoundMentionOps(mention)
      case mention: BioEventMention => new BioEventMentionOps(mention)
      case mention: BioRelationMention => new BioRelationMentionOps(mention)

      case mention => OdinMentionOps(mention) // Let odin figure it out.
    }
  }
}

object MentionsOps {

  def apply(mentions: Seq[Mention]): JSONSerialization = new SeqMentionOps(mentions)
}

class BioTextBoundMentionOps(tb: BioTextBoundMention) extends TextBoundMentionOps(tb) {
  override def asMentionOps(mention: Mention): OdinMentionOps = MentionOps(mention)

  override def jsonAST: JValue = {
    val ast = super.jsonAST
        .replace(List("type"), BioTextBoundMentionOps.string)

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

class BioEventMentionOps(em: BioEventMention) extends EventMentionOps(em) {
  override def asMentionOps(mention: Mention): OdinMentionOps = MentionOps(mention)

  override def jsonAST: JValue = {
    if (em.text == "MEK activates K-RAS")
      println("Here it is!")
    val ast = super.jsonAST
        .replace(List("type"), BioEventMentionOps.string)

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

class BioRelationMentionOps(rm: BioRelationMention) extends RelationMentionOps(rm) {
  override def asMentionOps(mention: Mention): OdinMentionOps = MentionOps(mention)

  override def jsonAST: JValue = {
    val ast = super.jsonAST
        .replace(List("type"), BioRelationMentionOps.string)

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

class CorefTextBoundMentionOps(tb: CorefTextBoundMention) extends BioTextBoundMentionOps(tb) {

  override def jsonAST: JValue = {
    val ast = super.jsonAST
        .replace(List("type"), CorefTextBoundMentionOps.string)

    ast merge (
      ("antecedents" -> tb.antecedents.jsonAST) ~
      ("sieves" -> tb.sieves.jsonAST)
    )
  }
}

class CorefEventMentionOps(em: CorefEventMention) extends BioEventMentionOps(em) {

  override def jsonAST: JValue = {
    val ast = super.jsonAST
        .replace(List("type"), CorefEventMentionOps.string)

    ast merge (
      ("antecedents" -> em.antecedents.jsonAST) ~
      ("sieves" -> em.sieves.jsonAST)
    )
  }
}

class CorefRelationMentionOps(rm: CorefRelationMention) extends BioRelationMentionOps(rm) {

  override def jsonAST: JValue = {
    val ast = super.jsonAST
        .replace(List("type"), CorefRelationMentionOps.string)

    ast merge (
      ("antecedents" -> rm.antecedents.jsonAST) ~
      ("sieves" -> rm.sieves.jsonAST)
    )
  }
}

object ReachImplicits {

  implicit class SeqMentionOps(mentions: Seq[Mention]) extends JSONSerialization {

    def jsonAST: JValue = JSONSerializer.jsonAST(mentions)
  }

  implicit class ModificationOps(mod: Modification) extends JSONSerialization {
    def jsonAST: JValue = mod match {
      case PTM(label, evidenceOp, siteOp, negated) =>
        ("modification-type" -> "PTM") ~
        ("label" -> label) ~
        // evidence is optional
        ("evidence" -> evidenceOp.map(MentionOps(_).jsonAST)) ~
        // site is optional
        ("site" -> siteOp.map(MentionOps(_).jsonAST)) ~
        ("negated" -> negated)
      case Mutant(evidence, foundBy) =>
        ("modification-type" -> "Mutant") ~
        ("evidence" -> MentionOps(evidence).jsonAST) ~
        ("foundBy" -> foundBy)
      case eventSite: EventSite =>
        // This is an exception because of the "site" label.
        ("modification-type" -> eventSite.label) ~
        ("site" -> MentionOps(eventSite.site).jsonAST)
      case simpleModification: SimpleModification =>
        ("modification-type" -> simpleModification.label) ~
        ("evidence" -> MentionOps(simpleModification.mention).jsonAST)
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
      case hasAntecedents if hasAntecedents.nonEmpty => hasAntecedents.map(m => MentionOps(m.asInstanceOf[CorefMention]).jsonAST)
      case _ => JNothing
    }
  }

  implicit class StringSetOps(ss: Set[String]) extends JSONSerialization {
    def jsonAST: JValue = ss match {
      case contents if contents.nonEmpty => contents
      case _ => JNothing
    }
  }
}

object BioTextBoundMentionOps {
  val string = "BioTextBoundMention"
  val shortString = "T"
}

object BioEventMentionOps {
  val string = "BioEventMention"
  val shortString = "E"
}

object BioRelationMentionOps {
  val string = "BioRelationMention"
  val shortString = "R"
}

object CorefTextBoundMentionOps {
  val string = "CorefTextBoundMention"
  val shortString = "T"
}

object CorefEventMentionOps {
  val string = "CorefEventMention"
  val shortString = "E"
}

object CorefRelationMentionOps {
  val string = "CorefRelationMention"
  val shortString = "R"
}
