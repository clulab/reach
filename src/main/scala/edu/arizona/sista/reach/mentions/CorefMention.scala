package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
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

  def isGeneric: Boolean = labels contains "Generic_entity"

  def number: Int = 1

  def toSingletons: Seq[CorefTextBoundMention] = {
    if (!this.isGeneric) Seq(this)
    else {
      val ants = this.firstSpecific.filterNot(_ == this).filter(_.isComplete)
      if (ants.isEmpty) Nil
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
          copy.antecedents = Set(ant)
          copy.sieves = this.sieves
          copy
        }
      }
    }
  }

  def isComplete: Boolean = !this.isGeneric || this.antecedent.nonEmpty
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

  def toSingletons: Seq[CorefEventMention] = {
    if (!this.isGeneric) Seq(this)
    else {
      val ants = this.firstSpecific.filterNot(_ == this).filter(_.isComplete)
      if (ants.isEmpty) Nil
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

  def toSingletons: Seq[CorefRelationMention] = Seq(this)

  def isComplete: Boolean = {
    val completeArgs = for {
      (lbl, args) <- this.arguments
      newArgs = args.filter(_.toCorefMention.isComplete).map(_.toCorefMention)
      if newArgs.nonEmpty
    } yield lbl -> newArgs
    argsComplete(completeArgs, this.labels)
  }

}

