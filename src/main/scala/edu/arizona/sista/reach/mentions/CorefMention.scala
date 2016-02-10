package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.utils.DependencyUtils._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.coref.CorefUtils._

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
    (this.isGeneric && this.text.toLowerCase.take(6) == "mutant" && this.mutants.isEmpty)
  }

  def toSingletons: Seq[CorefTextBoundMention] = {
    if (!this.isGeneric && !this.mutants.exists(mut => mut.isGeneric)) Seq(this)
    else {
      val ants = this.firstSpecific.filter(_.isComplete)
        .filterNot(a => a == this || a.asInstanceOf[CorefMention].hasGenericMutation)
      if (ants.isEmpty) {
        if (this.mutants.exists(mut => mut.isGeneric)) Seq(this)
        else Nil
      }
      else if (ants.size == 1) Seq(this)
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

  def isComplete: Boolean = !this.isGeneric || (this.antecedent.nonEmpty && this.antecedent.get.isComplete)

  def isClosedClass: Boolean = {
    if (!this.isGeneric || this.words.length > 1) false
    else {
      val tag = this.tags.get.headOption.getOrElse("NA")
      val closedClass = Seq("DT", "PRP", "PRP$", "WDT", "WP", "WP$")
      closedClass contains tag
    }
  }

  // generic antecedent matching with number approximation
  private val detMap = Map("a" -> 1,
    "an" -> 1,
    "both" -> 2,
    "each" -> 2,
    "that" -> 1,
    "those" -> 2,
    "these" -> 2, // assume two for now...
    "this" -> 1,
    "few" -> 3,
    "some" -> 3, // assume three for now...
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10,
    "its" -> 1,
    "their" -> 2)

  private val headMap = Map("it" -> 1,
    "they" -> 2,
    "theirs" -> 1,
    "them" -> 2,
    "that" -> 1,
    "both" -> 2,
    "each" -> 2,
    "those" -> 2,
    "these" -> 2, // assume two for now...
    "this" -> 1,
    "some" -> 3, // assume three for now...
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10
  )

  /**
    * Return the cardinality of a phrase based on its determiners
    */
  private def detCardinality(words: Seq[String], tags: Seq[String]): Int = {
    require(words.length == tags.length)
    val somenum = words(tags
      .zipWithIndex
      .find(x => x._1 == "CD")
      .getOrElse(tags
        .zipWithIndex
        .find(x => Seq("DT", "PRP$")
          .contains(x._1))
        .getOrElse(return 0))
      ._2)
    def finalAttempt(num: String): Int = try {
      num.toInt
    } catch {
      case e: NumberFormatException => 0
    }
    detMap.getOrElse(somenum, finalAttempt(somenum))
  }

  /**
    * Return the cardinality of a phrase based on its head -- is it plural?
    */
  private def headCardinality(somenum: String, tag: String): Int = {
    tag match {
      case "PRP" | "PRP$" => headMap.getOrElse(somenum, 0)
      case "NNS" | "NNPS" => 2
      case "NN" | "NNP" => 1
      case _ => headMap.getOrElse(somenum, 1)
    }
  }

  /**
    * Determine the cardinality of a mention -- how many real-world entities or events does it refer to?
    */
  def number: Int = {
    if (this.mutants.nonEmpty) return 1
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
                       sentence: Int,
                       document: Document,
                       keep: Boolean,
                       foundBy: String
                       ) extends BioEventMention(labels, trigger, arguments, sentence, document, keep, foundBy) with Anaphoric {

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

  // generic antecedent matching with number approximation
  private val detMap = Map("a" -> 1,
    "an" -> 1,
    "both" -> 2,
    "that" -> 1,
    "those" -> 2,
    "these" -> 2, // assume two for now...
    "this" -> 1,
    "few" -> 3,
    "some" -> 3, // assume three for now...
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10,
    "its" -> 1,
    "their" -> 2)

  private val headMap = Map("it" -> 1,
    "they" -> 2,
    "theirs" -> 1,
    "them" -> 2,
    "that" -> 1,
    "both" -> 2,
    "those" -> 2,
    "these" -> 2, // assume two for now...
    "this" -> 1,
    "some" -> 3, // assume three for now...
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10
  )

  /**
    * Return the cardinality of a phrase based on its determiners
    */
  private def detCardinality(words: Seq[String], tags: Seq[String]): Int = {
    require(words.length == tags.length)
    //val somenum = words(tags.zipWithIndex.find(x => Seq("DT", "CD", "PRP$").contains(x._1)).getOrElse(return 0)._2)
    val somenum = words(tags
      .zipWithIndex
      .find(x => x._1 == "CD")
      .getOrElse(tags
        .zipWithIndex
        .find(x => Seq("DT", "PRP$")
          .contains(x._1))
        .getOrElse(return 0))
      ._2)
    def finalAttempt(num: String): Int = try {
      num.toInt
    } catch {
      case e: NumberFormatException => 0
    }
    detMap.getOrElse(somenum, finalAttempt(somenum))
  }

  /**
    * Return the cardinality of a phrase based on its head -- is it plural?
    */
  private def headCardinality(somenum: String, tag: String): Int = {
    tag match {
      case "PRP" | "PRP$" => headMap.getOrElse(somenum, 0)
      case "NNS" | "NNPS" => 2
      case "NN" | "NNP" => 1
      case _ => headMap.getOrElse(somenum, 1)
    }
  }

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
                          sentence: Int,
                          document: Document,
                          keep: Boolean,
                          foundBy: String
                          ) extends BioRelationMention(labels, arguments, sentence, document, keep, foundBy) with Anaphoric {

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
    dst.xref = src.xref
    dst.context = src.context
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
