package edu.arizona.sista.bionlp.mentions

import edu.arizona.sista.odin._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

class BioTextBoundMention(
  labels: Seq[String],
  tokenInterval: Interval,
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display

class BioEventMention(
  labels: Seq[String],
  trigger: TextBoundMention,
  arguments: Map[String, Seq[Mention]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends EventMention(labels, trigger, arguments, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display

class BioRelationMention(
  labels: Seq[String],
  arguments: Map[String, Seq[Mention]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends RelationMention(labels, arguments, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display
