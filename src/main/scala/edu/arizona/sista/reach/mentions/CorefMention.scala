package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Interval

class CorefTextBoundMention(
                           labels: Seq[String],
                           tokenInterval: Interval,
                           sentence: Int,
                           document: Document,
                           keep: Boolean,
                           foundBy: String
                           ) extends BioTextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy) with Anaphoric {

  def isGeneric: Boolean = labels contains "Generic_entity"

  def number: Int = 1
}

class CorefEventMention(
                       labels: Seq[String],
                       trigger: TextBoundMention,
                       arguments: Map[String, Seq[Mention]],
                       sentence: Int,
                       document: Document,
                       keep: Boolean,
                       foundBy: String
                       ) extends BioEventMention(labels, trigger, arguments, sentence, document, keep, foundBy) with Anaphoric {

  def isGeneric: Boolean = labels contains "Generic_event"

  def number: Int = 1
}


class CorefRelationMention(
                          labels: Seq[String],
                          arguments: Map[String, Seq[Mention]],
                          sentence: Int,
                          document: Document,
                          keep: Boolean,
                          foundBy: String
                          ) extends BioRelationMention(labels, arguments, sentence, document, keep, foundBy) with Anaphoric {

  def isGeneric: Boolean = false

  def number: Int = 1
}

