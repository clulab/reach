package org.clulab.reach.`export`

import org.clulab.odin.{EventMention, Mention, TextBoundMention}
import org.clulab.reach.mentions.BioTextBoundMention
import org.json4s.JValue
import org.json4s.JsonAST.JArray
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import scala.annotation.tailrec

case class RelationDatum(
                          // Information about the event
                          controller: String,
                          controllerId: String,
                          controlled: Option[String],
                          controlledId: Option[String],
                          sentenceTokens: Seq[String],
                          eventIndices: Tuple2[Int, Int],
                          label: String,
                          polarity: Option[Boolean], // True for positive, False for negative, None for undefined
                          controllerIndices: Tuple2[Int, Int],
                          controlledIndices: Option[Tuple2[Int, Int]],
                          triggerIndices: Option[Tuple2[Int, Int]],
                          // Information about the rule that detected the extractioh
                          ruleName: Option[String],
                          // Information about the context, to aid transformers
                          contextLeft: Option[Seq[String]],
                          contextRight: Option[Seq[String]]
                        ) {
  val json: JValue = {
    ("controller" -> controller)~
      ("controller_id" -> controllerId) ~
      ("controlled" -> controlled.orNull) ~
      ("controller_id" -> controlledId.orNull) ~
      ("sentence_tokens" -> sentenceTokens) ~
      ("event_indices" -> List(eventIndices._1, eventIndices._2)) ~
      ("label" -> label) ~
      ("polarity" -> polarity) ~
      ("controller_indices" -> List(controllerIndices._1, controllerIndices._2)) ~
      ("controlled_indices" -> (controlledIndices match {
        case Some(i) => Some(List(i._1, i._2))
        case _ => None
      })) ~
      ("trigger_indices" -> (triggerIndices match {
        case Some(i) => Some(List(i._1, i._2))
        case _ => None
      })) ~
      ("rule_name" -> ruleName.orNull) ~
      ("context_left" -> contextLeft.orNull) ~
      ("context_right" -> contextRight.orNull)
  }
}

object RelationDatum{

  @tailrec
  private def getArgument(m:Mention, argumentName:String):Option[Mention] = {
    if(m.isInstanceOf[TextBoundMention])
      Some(m)
    else {
      m.arguments.get(argumentName) match {
        case Some(s:Seq[Mention]) => getArgument(s.head, argumentName)
        case _ => m.arguments.get("theme") match {
          case Some(b) => Some(b.head)
          case _ => None
        }

      }
    }
  }

  private def getPolarity(label: String): Option[Boolean] = {
    val l = label.toLowerCase
    if (l contains "positive")
      Some(true)
    else if (l contains "negative")
      Some(false)
    else
      None
  }

  def fromEventMention(evt:EventMention): RelationDatum = {
    val trigger = evt.trigger
    val controller = getArgument(evt, "controller").get
    val controlled = getArgument(evt, "controlled")

    val contextLeft =
      if(evt.sentence > 0){
        Some(evt.document.sentences(evt.sentence - 1).words.toSeq)
      }
      else None

    val contextRight =
      if (evt.sentence < (evt.document.sentences.length - 1)) {
        Some(evt.document.sentences(evt.sentence + 1).words.toSeq)
      }
      else None

    RelationDatum(
      controller = controller.text,
      controllerId = controller.asInstanceOf[BioTextBoundMention].grounding().get.id,
      controlled = controlled match {
        case Some(c) => Some(c.text)
        case _ => None
      },
      controlledId = controlled match {
        case Some(c) => Some(c.asInstanceOf[BioTextBoundMention].grounding().get.id)
        case _ => None
      },
      sentenceTokens = evt.sentenceObj.words,
      eventIndices = (evt.start, evt.end),
      label = evt.label,
      polarity = getPolarity(evt.label),
      controllerIndices = (controller.start, controller.end),
      controlledIndices = controlled match {
        case Some(c) => Some((c.start, c.end))
        case _ => None
      },
      triggerIndices = Some((trigger.start, trigger.end)),
      ruleName = Some(evt.foundBy),
      contextLeft = contextLeft,
      contextRight = contextRight
    )
  }
}

object VisualAnalyticsDataExporter {

  def jsonOutput(mentions:Seq[Mention]):String = {
    // Only activations, regulations and associations
    val filteredMentions =
      mentions.filter{
        m =>
          val label = m.label.toLowerCase()
          m.isInstanceOf[EventMention] && Seq("activation", "regulation", "association").map(label.contains).exists(identity)
      }
//    val eventMentions = mentions.filter(m => m.arguments.contains("controller") && m.isInstanceOf[EventMention])
    val data = filteredMentions.map(e => RelationDatum.fromEventMention(e.asInstanceOf[EventMention]))
    pretty(render(JArray(data.map(_.json).toList)))
  }


}
