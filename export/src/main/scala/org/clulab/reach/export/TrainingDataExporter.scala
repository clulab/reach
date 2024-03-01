package org.clulab.reach.`export`

import org.clulab.odin.{EventMention, Mention, TextBoundMention}
import org.clulab.struct.Interval
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.annotation.tailrec

abstract class BaseDatum{
  def json: JValue
}
case class EmptyDatum(words:Seq[String]) extends BaseDatum {
  def json: JValue = ("sentence_tokens" -> words.toList)
}

case class RegulationDatum(
                words: Seq[String],
                eventIndices: Range,
                type_ : String,
                polarity: Boolean,
                controllerIndices: Range,
                controlledIndices: Range,
                triggerIndices:Range,
                ruleName:Option[String],
                rule:Option[String]
                ) extends BaseDatum {
  val json: JValue =
    ("sentence_tokens" -> words.toList) ~
      ("event_indices" -> eventIndices) ~
      ("type" -> type_) ~
      ("polarity" -> polarity) ~
      ("controller_indices" -> controllerIndices) ~
      ("controlled_indices" -> controlledIndices) ~
      ("trigger_indices" -> triggerIndices) ~
      ("rule_name" -> ruleName.orNull) ~
      ("rule" -> rule.orNull)
}


case class NoRegulationDatum(
              words: Seq[String],
              entitiesIndices: Seq[Range],
            ) extends BaseDatum {

  val json: JValue =
    ("sentence_tokens" -> words.toList) ~
      ("entities_indices" -> entitiesIndices.toList)
}

object TrainingDataExporter {

  def getPolarity(label:String): Boolean = {
    val l = label.toLowerCase
    if(l contains "positive")
      true
    else if (l contains "negative")
      false
    else
      true
  }

  @tailrec
  def getIndices(e:Mention, arg:Option[String] = None): Range = {
    e match {
      case _: EventMention if arg.nonEmpty && e.arguments.contains(arg.get) => getIndices(e.arguments(arg.get).head, arg)
      case _ => e.start to e.end
    }
  }

  def jsonOutput(mentions: Seq[Mention],
                 allowedLabels:Option[Set[String]] = None,    // If not specified, every event will be returned
                 includeRule:Boolean = false,
                 rulesDictionary:Option[Map[String, String]] = None): String = {

    def filterCriteria(e:EventMention): Boolean = {
      if(allowedLabels.isEmpty || allowedLabels.get.contains(e.label))
        e.arguments.contains("controller") && e.arguments.contains("controlled")
      else
        false
    }
    // Iterate only through events
    val events:Seq[EventMention] =
      mentions collect  {
        case e:EventMention if filterCriteria(e) => e
      }

    val sentencesWithRegulationIndices = events.map(_.sentence).toSet

    // Get the TB mentions
    val tbMentions:Seq[TextBoundMention] =
      mentions collect {
        case tb:TextBoundMention if tb.matches("BioEntity")=> tb
      }

    // Get all the sentences with no event associated to it
    val emptyValues =
      if(events.nonEmpty){
        val sentences = events.head.document.sentences
        (for{
          (s, ix) <- sentences.zipWithIndex
          if !sentencesWithRegulationIndices.contains(ix)
        } yield EmptyDatum(words = s.words)).toSeq
      }
      else
        Seq.empty[RegulationDatum]

    // Get the sentences with mentions but w/o regulations
    val sentenceIxToMentions = tbMentions.groupBy(_.sentence)

    val mentionsWORegulationValues =
      for {
        (sIx, tbs) <- sentenceIxToMentions
        if !(sentencesWithRegulationIndices contains sIx)
      } yield {
        val words = tbs.head.sentenceObj.words
        NoRegulationDatum(
          words = words, entitiesIndices = tbs.map(tb => getIndices(tb, None))
        )
      }

    val (regulationValues, hardNoRegulationValues) =
      (events map {
        e =>
          val trigger = e.trigger

          val controllerIndices = getIndices(e, Some("controller"))
          val controlledIndices = getIndices(e, Some("controlled"))

          val words = e.sentenceObj.words

          // The positive instance
          val datum =
            RegulationDatum(
              words,
              getIndices(e),
              e.label,
              getPolarity(e.label),
              controllerIndices,
              controlledIndices,
              getIndices(trigger),
              Some(e.foundBy),
              if(includeRule && rulesDictionary.isDefined) {
                Some(rulesDictionary.get.getOrElse(e.foundBy, "MISSING VAL"))
              } else
                None

           )

          // The "hard" negative instance
          // Get all the tb mentions in the same sentence
          val sentenceTbMentions = sentenceIxToMentions.getOrElse(e.sentence, Seq.empty)
          // Get all the eligible tb mentions (those that aren't participant in the event
          val iController = Interval(controllerIndices.start, controllerIndices.end)
          val iControlled = Interval(controlledIndices.start, controlledIndices.end)
          val otherMentions = sentenceTbMentions.map(tb => getIndices(tb)).filter(ix => {
            val iTb = Interval(ix.start, ix.end)
            !((iTb overlaps iController) || (iTb overlaps iControlled))
          })

          // Now do the cross product of all the mentions that aren't participant
          val hardNegative = if(otherMentions.nonEmpty) Some(NoRegulationDatum(words, otherMentions)) else None

          (datum, hardNegative)
      }).unzip

//    val allDatums:Seq[BaseDatum] = regulationValues ++ hardNoRegulationValues ++ emptyValues ++ mentionsWORegulationValues

    val ret = ("regulations" -> regulationValues.map(_.json)) ~
      ("hardInstances" -> hardNoRegulationValues.collect{case Some(d) => d}.map(_.json)) ~
      ("withoutRegulations" -> mentionsWORegulationValues.map(_.json)) ~
      ("emptySentences" -> emptyValues.map(_.json))


    pretty(render(ret))
  }


}
