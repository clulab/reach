package org.clulab.reach.mentions

import org.clulab.odin._
import org.clulab.struct.Interval
import org.clulab.processors.Document
import org.clulab.reach.context.{Context, ContextMap, ContextMetaData}

import scala.collection.mutable

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


//  override var contextOpt: Option[ContextMap] = None
//  override var contextMetaDataOpt: Option[ContextMetaData] = None
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

  // Copy constructor for EventMention
  def copyWithMods(
    labels: Seq[String] = this.labels,
    trigger: TextBoundMention = this.trigger,
    arguments: Map[String, Seq[Mention]] = this.arguments,
    paths: Map[String, Map[Mention, SynPath]] = this.paths,
    sentence: Int = this.sentence,
    document: Document = this.document,
    keep: Boolean = this.keep,
    foundBy: String = this.foundBy,
    modifications:Set[Modification] = this.modifications): BioEventMention = {
    val bem = new BioEventMention(labels, trigger, arguments, paths, sentence, document, keep, foundBy)
    val copyMods = new mutable.HashSet[Modification]()
    copyMods ++= modifications
    bem.modifications = copyMods.toSet
    bem
  }

//  override var contextOpt: Option[ContextMap] = None
//  override var contextMetaDataOpt: Option[ContextMetaData] = None
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

  // Copy constructor for RelationMention
  def copyWithMods(
    labels: Seq[String] = this.labels,
    tokenInterval: Interval = this.tokenInterval,
    arguments: Map[String, Seq[Mention]] = this.arguments,
    paths: Map[String, Map[Mention, SynPath]] = this.paths,
    sentence: Int = this.sentence,
    document: Document = this.document,
    keep: Boolean = this.keep,
    foundBy: String = this.foundBy,
    modifications:Set[Modification] = this.modifications): BioRelationMention = {
    val brm = new BioRelationMention(labels, arguments, paths, sentence, document, keep, foundBy)
    val copyMods = new mutable.HashSet[Modification]()
    copyMods ++= modifications
    brm.modifications = copyMods.toSet
    brm
  }

//  override var contextOpt: Option[ContextMap] = None
//  override var contextMetaDataOpt: Option[ContextMetaData] = None
}

object BioMention{
    def copyAttachments(src:BioMention, dst:BioMention){
        dst.copyGroundingFrom(src)
        dst.contextOpt = src.contextOpt
        dst.contextMetaDataOpt = src.contextMetaDataOpt
        dst.modifications ++= src.modifications
    }
}
