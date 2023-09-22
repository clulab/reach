package org.clulab.reach.`export`

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
                triggerIndices:Range
                ) {
  val json: JValue =
    ("sentence_tokens" -> words.toList) ~
      ("event_indices" -> eventIndices.toList) ~
      ("type" -> type_) ~
      ("polarity" -> polarity) ~
      ("controller_indices" -> controllerIndices.toList) ~
      ("controlled_indices" -> controlledIndices.toList) ~
      ("trigger_indices" -> triggerIndices.toList)
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

  def jsonOutput(mentions: Seq[Mention]): String = {

    def filterCriteria(e:EventMention): Boolean = {
      if((e.label.toLowerCase contains "activation") || (e.label.toLowerCase contains "regulation"))
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
          )
      }

    pretty(render(JArray(values.map(_.json).toList)))
  }


}
