package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.context.Context

class BioTextBoundMention(
  labels: Seq[String],
  tokenInterval: Interval,
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display {

  override def hashCode: Int = {
    val mutations = modifications.filter(_.isInstanceOf[Mutant])
    super.hashCode() * 42 + mutations.hashCode()
  }

}

class BioEventMention(
  labels: Seq[String],
  trigger: TextBoundMention,
  arguments: Map[String, Seq[Mention]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends EventMention(labels, trigger, arguments, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display with Context{

  // FIXME
  // sketchy code to mutate the mention's tokenInterval
  // we need this because a mention solved by coreference
  // may have arguments outside its sentence
  var mutableTokenInterval: Interval = super.tokenInterval
  override def tokenInterval: Interval = mutableTokenInterval

  override def hashCode: Int = {
    val mutations = modifications.filter(_.isInstanceOf[Mutant])
    super.hashCode() * 42 + mutations.hashCode()
  }

}

class BioRelationMention(
  labels: Seq[String],
  arguments: Map[String, Seq[Mention]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends RelationMention(labels, arguments, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display {

  // FIXME
  // sketchy code to mutate the mention's tokenInterval
  // we need this because a mention solved by coreference
  // may have arguments outside its sentence
  var mutableTokenInterval: Interval = super.tokenInterval
  override def tokenInterval: Interval = mutableTokenInterval

  override def hashCode: Int = {
    val mutations = modifications.filter(_.isInstanceOf[Mutant])
    super.hashCode() * 42 + mutations.hashCode()
  }

}
