package org.clulab.reach.`export`

import org.clulab.odin.{EventMention, Mention, TextBoundMention}
import org.clulab.reach.FriesEntry
import org.clulab.reach.mentions.BioTextBoundMention
import org.json4s.{JObject, JValue}
import org.json4s.JsonAST.JArray
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import scala.annotation.tailrec

case class RelationDatum(
                          // Information about the event
                          controller: String,
                          controllerId: Seq[String],
                          controlled: Option[String],
                          controlledId: Option[Seq[String]],
                          sentenceTokens: Seq[String],
                          eventIndices: (Int, Int),
                          eventCharSpan: (Int, Int),
                          label: String,
                          polarity: Option[Boolean], // True for positive, False for negative, None for undefined
                          controllerIndices: (Int, Int),
                          controllerCharSpan: (Int, Int),
                          controlledIndices: Option[(Int, Int)],
                          controlledCharSpan: Option[(Int, Int)],
                          triggerIndices: Option[(Int, Int)],
                          triggerCharSpan: Option[(Int, Int)],
                          // Information about the rule that detected the extraction
                          ruleName: Option[String],
                          // Information about the context, to aid transformers
                          contextLeft: Option[Seq[String]],
                          contextRight: Option[Seq[String]]
                        ) {
  val json: JValue = {
    ("controller" -> controller)~
      ("controller_id" -> controllerId) ~
      ("controlled" -> controlled.orNull) ~
      ("controlled_id" -> controlledId.orNull) ~
      ("sentence_tokens" -> sentenceTokens) ~
      ("event_indices" -> List(eventIndices._1, eventIndices._2)) ~
      ("event_char_span" -> List(eventCharSpan._1, eventCharSpan._2)) ~
      ("label" -> label) ~
      ("polarity" -> polarity) ~
      ("controller_indices" -> List(controllerIndices._1, controllerIndices._2)) ~
      ("controlled_indices" -> controlledIndices.map(i => List(i._1, i._2))) ~
      ("trigger_indices" -> triggerIndices.map(i => List(i._1, i._2))) ~
      ("controller_char_span" -> List(controllerCharSpan._1, controllerCharSpan._2)) ~
      ("controlled_char_span" -> controlledCharSpan.map(i => List(i._1, i._2))) ~
      ("trigger_char_span" -> triggerCharSpan.map(i => List(i._1, i._2))) ~
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

  private def getCharSpan(mention:Mention): (Int, Int) = {
    val sent = mention.sentenceObj
    val (firstToken, lastToken) = (mention.tokenInterval.start, mention.tokenInterval.end-1)
    (sent.startOffsets(firstToken), sent.endOffsets(lastToken))
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
      controllerId = {
        val controllerGrounding = controller.asInstanceOf[BioTextBoundMention].grounding().get
        Seq(controllerGrounding.namespace,  controllerGrounding.id)
      },
      controlled = controlled match {
        case Some(c) => Some(c.text)
        case _ => None
      },
      controlledId = controlled match {
        case Some(c) =>
          val controlledGrounding = c.asInstanceOf[BioTextBoundMention].grounding().get
          Some(Seq(controlledGrounding.namespace, controlledGrounding.id))
        case _ => None
      },
      sentenceTokens = evt.sentenceObj.words,
      eventIndices = (evt.start, evt.end),
      eventCharSpan = getCharSpan(evt),
      label = evt.label,
      polarity = getPolarity(evt.label),
      controllerIndices = (controller.start, controller.end),
      controllerCharSpan = getCharSpan(controller),
      controlledIndices = controlled.map(c =>(c.start, c.end)),
      controlledCharSpan = controlled.map(getCharSpan),
      triggerIndices = Some((trigger.start, trigger.end)),
      triggerCharSpan = Some(getCharSpan(trigger)),
      ruleName = Some(evt.foundBy),
      contextLeft = contextLeft,
      contextRight = contextRight
    )
  }
}

object VisualAnalyticsDataExporter {

  def jsonOutput(mentions:Seq[Mention], entry:Option[FriesEntry]):String = {
    // Only activations, regulations and associations
    val filteredMentions =
      mentions.filter{
        m =>
          val label = m.label.toLowerCase()
          m.isInstanceOf[EventMention] && Seq("activation", "regulation", "association").map(label.contains).exists(identity)
      }

    val data = filteredMentions.map(e => RelationDatum.fromEventMention(e.asInstanceOf[EventMention]))
    val entryText = entry map (_.text)
    val json = JObject("mentions" -> JArray(data.map(_.json).toList))

    pretty(render(
      entryText match {
      case Some(txt) => json ~ ("text" -> txt)
      case None => json
    }))
  }

}
