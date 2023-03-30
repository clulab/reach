package org.clulab.reach.mentions.serialization.json

import org.clulab.odin.Mention
import org.clulab.odin.serialization.json.{EventMentionOps, RelationMentionOps, TextBoundMentionOps}
import org.clulab.odin.serialization.json.{MentionOps => OdinMentionOps}
import org.clulab.reach.context.Context
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions.{Anaphoric, BioEventMention, BioRelationMention, BioTextBoundMention, CorefEventMention, CorefMention, CorefRelationMention, CorefTextBoundMention, Display, EventSite, Grounding, Modification, Modifications, Mutant, PTM, SimpleModification}
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

trait BioMentionOps {
  this: OdinMentionOps =>

  type BioMention = Modifications with Grounding with Display with Context

  // The mention is accessible through the OdinMentionOps which records it as a val.
  override lazy val documentEquivalenceHash: Int = EquivalenceHashes.get(mention.document)
  // If the equivalenceHash should ever need the ID of this subclass of MentionOps,
  // then this stringCode can be used to dynamically get the right value.
  // override val stringCode = s"org.clulab.odin.$longString"

  override def asMentionOps(mention: Mention): OdinMentionOps = MentionOps(mention)

  def bioJsonAST(bioMention: BioMention): JObject = {
    ("modifications" -> bioMention.modifications.jsonAST) ~
    // grounding is optional
    ("grounding" -> bioMention.grounding.map(_.jsonAST)) ~
    // context is optional
    ("context" -> bioMention.contextOpt.map(_.jsonAST)) ~
    // usually just labels.head...
    ("displayLabel" -> bioMention.displayLabel)
  }
}

class BioTextBoundMentionOps(tb: BioTextBoundMention) extends TextBoundMentionOps(tb) with BioMentionOps {

  override def longString: String = BioTextBoundMentionOps.string

  override def jsonAST: JValue = super.jsonAST.merge(bioJsonAST(tb))
}

class BioEventMentionOps(em: BioEventMention) extends EventMentionOps(em) with BioMentionOps {

  override def longString: String = BioEventMentionOps.string

  override def jsonAST: JValue = super.jsonAST
      .merge(bioJsonAST(em))
      .merge(JObject(List(JField("isDirect", em.isDirect))))
}

class BioRelationMentionOps(rm: BioRelationMention) extends RelationMentionOps(rm) with BioMentionOps {

  override def longString: String = BioRelationMentionOps.string

  override def jsonAST: JValue = super.jsonAST.merge(bioJsonAST(rm))
}

trait CorefMentionOps {
  type CorefMention = Modifications with Grounding with Display with Context with Anaphoric

  def corefJsonAST(corefMention: CorefMention): JObject = {
    ("antecedents" -> corefMention.antecedents.jsonAST) ~
    ("sieves" -> corefMention.sieves.jsonAST)
  }
}

class CorefTextBoundMentionOps(tb: CorefTextBoundMention) extends BioTextBoundMentionOps(tb) with CorefMentionOps {

  override def longString: String = CorefTextBoundMentionOps.string

  override def jsonAST: JValue = super.jsonAST.merge(corefJsonAST(tb))
}

class CorefEventMentionOps(em: CorefEventMention) extends BioEventMentionOps(em) with CorefMentionOps {

  override def longString: String = CorefEventMentionOps.string

  override def jsonAST: JValue = super.jsonAST.merge(corefJsonAST(em))
}

class CorefRelationMentionOps(rm: CorefRelationMention) extends BioRelationMentionOps(rm) with CorefMentionOps {

  override def longString: String = CorefRelationMentionOps.string

  override def jsonAST: JValue = super.jsonAST.merge(corefJsonAST(rm))
}

object ReachImplicits {

  implicit class SeqMentionOps(mentions: Seq[Mention]) extends JSONSerialization {

    def jsonAST: JValue = JSONSerializer.jsonAST(mentions.sorted(OdinMentionOps.mentionOrdering))
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
}

object BioEventMentionOps {
  val string = "BioEventMention"
}

object BioRelationMentionOps {
  val string = "BioRelationMention"
}

object CorefTextBoundMentionOps {
  val string = "CorefTextBoundMention"
}

object CorefEventMentionOps {
  val string = "CorefEventMention"
}

object CorefRelationMentionOps {
  val string = "CorefRelationMention"
}
