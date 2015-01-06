package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.brat.Brat
import edu.arizona.sista.matcher.{ExtractorEngine, Actions, Mention}
import edu.arizona.sista.processors.Document

class BasicRuler(val rules: String, val actions: Actions) {
  val engine = new ExtractorEngine(rules, actions)

  def extractFrom(doc: Document): Seq[Mention] = postprocess(engine.extractFrom(doc))

  // TODO this method should inspect the mentions and return a new sequence
  // that may include new mentions or discard some
  // NOTE now it returns the same sequence
  def postprocess(mentions: Seq[Mention]): Seq[Mention] = mentions
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
                    s"$dir/down_reg_events.yml",
                    s"$dir/up_reg_events.yml")
    (files map readFile).mkString("\n\n")
  }

  def readFile(filename: String) = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }
}
