package edu.arizona.sista.bionlp

import edu.arizona.sista.odin._

package object mentions {
  type BioMention = Mention with Modifications with Grounding with Display

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

    private def convertArguments(
      arguments: Map[String, Seq[Mention]]
    ): Map[String, Seq[BioMention]] = arguments.transform {
      case (k, v) => v.map(_.toBioMention)
    }
  }

}
