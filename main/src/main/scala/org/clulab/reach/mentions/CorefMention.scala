package org.clulab.reach.mentions

import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.reach.utils.DependencyUtils._
import org.clulab.struct.Interval
import org.clulab.coref.CorefUtils._

class CorefTextBoundMention(
  labels: Seq[String],
  tokenInterval: Interval,
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends BioTextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy) with Anaphoric {

  def isGeneric: Boolean = (labels contains "Generic_entity") || (labels contains "GenericMutant")

  def hasGenericMutation: Boolean = {
    this.mutants.exists(mut => mut.isGeneric) ||
    (this.isGeneric && this.text.toLowerCase.take(6) == "mutant") //FIXME: hack until mutant detection is better
  }

  def toSingletons: Seq[CorefTextBoundMention] = {
    if (this.nonGeneric && !this.hasGenericMutation) Seq(this)
    else {
      val ants = this.firstSpecific.filter(_.isComplete)
        .filterNot(_ == this)
      if (ants.isEmpty) {
        if (this.hasGenericMutation) Seq(this)
        else Nil
      }
      else if (ants.size == 1) {
        Seq(this)
      }
      else {
        for {
          ant <- ants
        } yield {
          val copy = new CorefTextBoundMention(
            this.labels,
            this.tokenInterval,
            this.sentence,
            this.document,
            this.keep,
            this.foundBy)
          CorefMention.copyAttachments(this,copy)
          copy.antecedents = Set(ant)
          copy.sieves = this.sieves
          copy
        }
      }
    }
  }

  def isComplete: Boolean = {
    this.nonGeneric ||
      (this.antecedent.nonEmpty && this.antecedent.get != this && this.antecedent.get.isComplete)
  }

  def isClosedClass: Boolean = {
    if (!this.isGeneric || this.words.length > 1) false
    else {
      val tag = this.tags.get.headOption.getOrElse("NA")
      val closedClass = Seq("DT", "PRP", "PRP$", "WDT", "WP", "WP$")
      closedClass contains tag
    }
  }

  /**
    * Determine the cardinality of a mention -- how many real-world entities or events does it refer to?
    */
  def number: Int = {
    // number is always 1 if mutants are known, because we have split them into singletons already
    if (this.mutants.nonEmpty && this.mutants.forall(mut => !mut.isGeneric)) return 1
    val sent = this.sentenceObj
    // Is it safe to assume that generic mutations will be adjacent to their protein? Unsure.
    val startFrom = if (mutants.nonEmpty && this.mutants.exists(_.isGeneric)) {
      val genMut = this.mutants.find(_.isGeneric).get.evidence
      Interval(math.min(genMut.tokenInterval.start, this.tokenInterval.start),
        math.max(genMut.tokenInterval.end, this.tokenInterval.end))
    } else this.tokenInterval
    val mhead = findHeadStrict(startFrom, sent).getOrElse(this.tokenInterval.start)
    val phrase = subgraph(startFrom, sent).getOrElse(this.tokenInterval)
    // use determiner ("the") or explicit number ("all six") if it exists and is useful
    val dc = detCardinality(sent.words.slice(phrase.start, phrase.end),
      sent.tags.get.slice(phrase.start, phrase.end))
    // use head if determiner is nonexistent or not useful
    val hc = headCardinality(sent.words(mhead), sent.tags.get(mhead))

    this match {
      case informativeDeterminer if dc != 0 => dc
      case informativeHead if dc == 0 & hc != 0 => hc
      case _ => 1
    }
  }

  def isGenericNounPhrase: Boolean = {
    this.isGeneric &&
      !this.isClosedClass &&
      Seq("NN", "NNS").contains(this.sentenceObj.tags.get(
          findHeadStrict(
            this.tokenInterval,
            this.sentenceObj
          ).getOrElse(
            this.tokenInterval.start
          )
        )
      )
  }
}

class CorefEventMention(
  labels: Seq[String],
  trigger: TextBoundMention,
  arguments: Map[String, Seq[Mention]],
  paths: Map[String, Map[Mention, SynPath]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String,
  isDirect: Boolean = false
) extends BioEventMention(labels, trigger, arguments, paths, sentence, document, keep, foundBy, isDirect) with Anaphoric {

  def isGeneric: Boolean = labels contains "Generic_event"

  def hasGenericMutation: Boolean = false

  def toSingletons: Seq[CorefEventMention] = {
    if (!this.isGeneric) Seq(this)
    else {
      val ants = this.firstSpecific.filterNot(_ == this).filter(_.isComplete)
      if (ants.isEmpty) Nil
      else if (ants.size == 1) Seq(this)
      else {
        for {
          ant <- ants
        } yield {
          val copy = new CorefEventMention(
            this.labels,
            this.trigger,
            this.arguments,
            this.paths,
            this.sentence,
            this.document,
            this.keep,
            this.foundBy)
          CorefMention.copyAttachments(this,copy)
          copy.antecedents = Set(ant)
          copy.sieves = this.sieves
          copy
        }
      }
    }
  }

  def isComplete: Boolean = {
    val toExamine = this.antecedent.getOrElse(this)
    toExamine match {
      case gnc if this.isGeneric => this.antecedent.nonEmpty
      case spc =>
        val completeArgs = for {
          (lbl, args) <- this.arguments
          newArgs = args.filter(_.toCorefMention.isComplete).map(_.toCorefMention)
          if newArgs.nonEmpty
        } yield lbl -> newArgs
        argsComplete(completeArgs, spc.asInstanceOf[CorefMention].labels)
    }
  }

  def isClosedClass: Boolean = false

  /**
    * Determine the cardinality of a mention -- how many real-world entities or events does it refer to?
    */
  def number: Int = {
    val sent = this.sentenceObj
    val mhead = findHeadStrict(this.tokenInterval, sent).getOrElse(this.tokenInterval.start)
    val phrase = subgraph(this.tokenInterval, sent)
    val dc = detCardinality(sent.words.slice(phrase.get.start, phrase.get.end),
      sent.tags.get.slice(phrase.get.start, phrase.get.end))
    val hc = headCardinality(sent.words(mhead), sent.tags.get(mhead))

    this match {
      case informativeDeterminer if dc != 0 => dc
      case informativeHead if dc == 0 & hc != 0 => hc
      case _ => 1
    }
  }

  // For now, only CorefTextBoundMention can be a generic noun phrase
  def isGenericNounPhrase: Boolean = false
}


class CorefRelationMention(
  labels: Seq[String],
  arguments: Map[String, Seq[Mention]],
  paths: Map[String, Map[Mention, SynPath]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String
) extends BioRelationMention(labels, arguments, paths, sentence, document, keep, foundBy) with Anaphoric {

  def isGeneric: Boolean = false

  def hasGenericMutation: Boolean = false

  def number: Int = 1

  def toSingletons: Seq[CorefRelationMention] = Seq(this)

  def isComplete: Boolean = {
    val completeArgs = for {
      (lbl, args) <- this.arguments
      newArgs = args.filter(_.toCorefMention.isComplete).map(_.toCorefMention)
      if newArgs.nonEmpty
    } yield lbl -> newArgs
    argsComplete(completeArgs, this.labels)
  }

  def isClosedClass: Boolean = false

  def isGenericNounPhrase: Boolean = false
}

object CorefMention {
  def copyAttachments(src:BioMention, dst:CorefMention){
    dst.copyGroundingFrom(src)
    dst.contextOpt = src.contextOpt
    dst.modifications ++= corefMods(src.modifications)
  }

  private def corefMods(modifications: Set[Modification]): Set[Modification] = {
    for {
      modification <- modifications
      corefMod = modification match {
        case mutation: Mutant => Mutant(mutation.evidence.toCorefMention, mutation.foundBy)
        case anythingElse: Modification => anythingElse
      }
    } yield corefMod
  }
}
