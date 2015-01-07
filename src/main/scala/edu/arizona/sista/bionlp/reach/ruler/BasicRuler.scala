package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.brat.Brat
import edu.arizona.sista.matcher.{ExtractorEngine, Actions, Mention, EventMention}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.bionlp.reach.core.RelationMention

class BasicRuler(val rules: String, val actions: Actions) {
  val engine = new ExtractorEngine(rules, actions)

  def extractFrom(doc: Document): Seq[Mention] = postprocess(engine.extractFrom(doc))

  def postprocess(mentions: Seq[Mention]): Seq[Mention] = mentions flatMap { mention =>
    mention match {
      case m: EventMention if m.label == "Phosphorylation" && m.arguments.contains("Cause") =>
        val controller = m.arguments("Cause")
        val phospho = new EventMention("Phosphorylation", m.trigger, m.arguments - "Cause", m.sentence, m.document, m.foundBy)
        val args = Map("Controller" -> controller, "Controlled" -> Seq(phospho))
        val upreg = new RelationMention("UpRegulation", args, m.sentence, m.document, m.foundBy)
        Seq(upreg, phospho)
      case m => Seq(m)
    }
  }
}

object BasicRuler {
  def readRules: String = readEntityRules + "\n\n" + readEventRules

  def readEntityRules: String = {
    val dir = "/edu/arizona/sista/bionlp/extractors"
    val files = Seq(s"$dir/default_entities.yml", s"$dir/DARPA_entities.yml" )
    (files map readFile).mkString("\n\n")
  }

  def readEventRules: String = {
    val dir = "/edu/arizona/sista/bionlp/extractors"
    val files = Seq(s"$dir/phospho_events.yml",
                    s"$dir/ubiq_events.yml",
                    s"$dir/hydrox_events.yml",
                    s"$dir/hydrolysis_events.yml",
                    s"$dir/bind_events.yml",
                    s"$dir/exchange_events.yml",
                    s"$dir/degrad_events.yml",
                    s"$dir/transcription_events.yml",
                    s"$dir/regulation_events.yml",
                    s"$dir/down_reg_events.yml",
                    s"$dir/up_reg_events.yml",
                    s"$dir/transport_events.yml")
    (files map readFile).mkString("\n\n")
  }

  def readFile(filename: String) = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }
}
