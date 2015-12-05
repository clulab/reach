package edu.arizona.sista.reach

import edu.arizona.sista.coref.AntecedentSelector
import edu.arizona.sista.reach.context.Context
import edu.arizona.sista.odin._

package object mentions {
  type BioMention = Mention with Modifications with Grounding with Display with Context
  type CorefMention = BioMention with Anaphoric
  type Link = (Seq[CorefMention], AntecedentSelector) => Seq[CorefMention]

  implicit class MentionOps(mention: Mention) {
    def toBioMention: BioMention = mention match {
      case m: BioMention => m

      case m: TextBoundMention =>
        new BioTextBoundMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy)

      case m: EventMention =>
        new BioEventMention(
          m.labels,
          m.trigger,
          convertArguments(m.arguments),
          m.sentence,
          m.document,
          m.keep,
          m.foundBy)

      case m: RelationMention =>
        new BioRelationMention(
          m.labels,
          convertArguments(m.arguments),
          m.sentence,
          m.document,
          m.keep,
          m.foundBy)
    }

    def toCorefMention: CorefMention = mention match {
      case m: CorefMention => m

      case m: BioTextBoundMention => {
        val tbm = new CorefTextBoundMention(
          m.labels,
          m.tokenInterval,
          m.sentence,
          m.document,
          m.keep,
          m.foundBy
        )
        tbm.modifications ++= m.modifications
        BioMention.copyAttachments(m, tbm)
        tbm
      }
      case m: BioEventMention => {
        val ev = new CorefEventMention(
          m.labels,
          m.trigger,
          corefArguments(m.arguments),
          m.sentence,
          m.document,
          m.keep,
          m.foundBy
        )
        ev.modifications ++= m.modifications
        BioMention.copyAttachments(m, ev)
        ev
      }

      case m: BioRelationMention => {
        val rel = new CorefRelationMention(
          m.labels,
          corefArguments(m.arguments),
          m.sentence,
          m.document,
          m.keep,
          m.foundBy
        )
        rel.modifications ++= m.modifications
        BioMention.copyAttachments(m, rel)
        rel
      }

      case m: Mention => m.toBioMention.toCorefMention
    }

    private def convertArguments(
      arguments: Map[String, Seq[Mention]]
    ): Map[String, Seq[BioMention]] = arguments.transform {
      case (k, v) => v.map(_.toBioMention)
    }

    private def corefArguments(
      arguments: Map[String, Seq[Mention]]
    ): Map[String,Seq[CorefMention]] = arguments.transform {
      case (k, v) => v.map(_.toCorefMention)
    }

  }

}
