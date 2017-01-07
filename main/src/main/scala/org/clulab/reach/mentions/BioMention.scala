package org.clulab.reach.mentions

import org.clulab.odin._
import org.clulab.struct.Interval
import org.clulab.processors.Document
import org.clulab.reach.context.Context

class BioTextBoundMention(
  labels: Seq[String],
  tokenInterval: Interval,
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display with Context{

  override def hashCode: Int = {
    val mutations = modifications.filter(_.isInstanceOf[Mutant])
    super.hashCode() * 42 + mutations.hashCode()
  }

  def this(m: Mention) = this(m.labels, m.tokenInterval, m.sentence, m.document, m.keep, m.foundBy)
}

class BioEventMention(
  labels: Seq[String],
  trigger: TextBoundMention,
  arguments: Map[String, Seq[Mention]],
  paths: Map[String, Map[Mention, SynPath]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String,
  val isDirect: Boolean = false
) extends EventMention(labels, mkTokenInterval(trigger, arguments), trigger, arguments, paths, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display with Context{

  override def hashCode: Int = {
    val mutations = modifications.filter(_.isInstanceOf[Mutant])
    super.hashCode() * 42 + mutations.hashCode()
  }

  def this(m: EventMention) =
    this(m.labels, m.trigger, m.arguments, m.paths, m.sentence, m.document, m.keep, m.foundBy)

  def this(m: EventMention, isDirect: Boolean) =
    this(m.labels, m.trigger, m.arguments, m.paths, m.sentence, m.document, m.keep, m.foundBy, isDirect = isDirect)
}

class BioRelationMention(
  labels: Seq[String],
  arguments: Map[String, Seq[Mention]],
  paths: Map[String, Map[Mention, SynPath]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends RelationMention(labels, mkTokenInterval(arguments), arguments, paths, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display with Context {

  override def hashCode: Int = {
    val mutations = modifications.filter(_.isInstanceOf[Mutant])
    super.hashCode() * 42 + mutations.hashCode()
  }

  def this(m: RelationMention) =
    this(m.labels, m.arguments, m.paths, m.sentence, m.document, m.keep, m.foundBy)
}

object BioMention{
    def copyAttachments(src:BioMention, dst:BioMention){
        dst.copyGroundingFrom(src)
        dst.context = src.context
        dst.modifications ++= src.modifications
    }
}
