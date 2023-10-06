package org.clulab.reach.`export`

import de.heikoseeberger.akkahttpjson4s.Json4sSupport.ShouldWritePretty.False
import org.clulab.odin.{CrossSentenceMention, EventMention, Mention, RelationMention, TextBoundMention}
import org.json4s.JsonAST.JArray
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import scala.annotation.tailrec

case class Datum(
                words: Seq[String],
                eventIndices: Range,
                type_ : String,
                polarity: Boolean,
                controllerIndices: Range,
                controlledIndices: Range,
                triggerIndices:Range,
                ruleName:Option[String],
                rule:Option[String]
                ) {
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

    val values =
      events map {
        e =>
          val trigger = e.trigger

          Datum(
            e.sentenceObj.words,
            getIndices(e),
            e.label,
            getPolarity(e.label),
            getIndices(e, Some("controller")),
            getIndices(e, Some("controlled")),
            getIndices(trigger),
            Some(e.foundBy),
            if(includeRule && rulesDictionary.isDefined) {
              val x = Some(rulesDictionary.get.getOrElse(e.foundBy, "MISSING VAL"))
              x
            } else
              None
          )
      }

    pretty(render(JArray(values.map(_.json).toList)))
  }


}
