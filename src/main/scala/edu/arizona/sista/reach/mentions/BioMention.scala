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
    with Modifications with Grounding with Display with Context{

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
  foundBy: String,
  val isDirect: Boolean = false
) extends EventMention(labels, trigger, arguments, sentence, document, keep, foundBy)
    with Modifications with Grounding with Display with Context{


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
    with Modifications with Grounding with Display with Context {

  override def hashCode: Int = {
    val mutations = modifications.filter(_.isInstanceOf[Mutant])
    super.hashCode() * 42 + mutations.hashCode()
  }

}

object BioMention{
    def copyAttachments(src:BioMention, dst:BioMention){
        dst.copyGroundingFrom(src)
        dst.context = src.context
        dst.modifications ++= src.modifications
    }
}
