package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.matcher.{Actions, Mention, EventMention, TextBoundMention, State}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Interval

class DarpaActions extends Actions {
  // NOTE these are example actions that should be adapted for the darpa evaluation

  def mkTextBoundMention(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    Seq(new TextBoundMention(label, mention("--GLOBAL--").head, sent, doc, ruleName))
  }

  def mkBannerMention(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val allMentions = state.allMentions.filter(_.sentence == sent).map(_.tokenInterval)
    // make sure each interval doesn't intersect with existing Gene_or_gene_product mentions previously found
    for (m <- mention("--GLOBAL--") if allMentions.forall(!_.intersects(m))) yield new TextBoundMention(label, m, sent, doc, ruleName)
  }

  def mkConversion(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val theme = state.mentionsFor(sent, mention("theme").head.start, "Protein").head
    val cause = if (mention contains "cause") state.mentionsFor(sent, mention("cause").head.start, "Protein").headOption else None
    val args = if (cause.isDefined) Map("Theme" -> Seq(theme), "Cause" -> Seq(cause.get)) else Map("Theme" -> Seq(theme))
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkRegulation(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val cause = state.mentionsFor(sent, mention("cause").head.start, "Protein").head
    val theme = state.mentionsFor(sent, mention("theme").head.start, Seq("Phosphorylation", "Ubiquitination")).find(_.isInstanceOf[EventMention]).get
    val args = Map("Theme" -> Seq(theme), "Cause" -> Seq(cause))
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkBindingEvent(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val themes = mention("theme") flatMap (m => state.mentionsFor(sent, m.start, "Gene_or_gene_product"))
    val args = Map("Theme" -> themes)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }
}
