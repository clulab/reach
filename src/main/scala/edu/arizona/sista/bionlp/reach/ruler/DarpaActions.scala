package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.core.RelationMention
import edu.arizona.sista.matcher.{Actions, Mention, EventMention, TextBoundMention, State}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Interval

class DarpaActions extends Actions {
  // NOTE these are example actions that should be adapted for the darpa evaluation

  //
  val proteinLabels = Seq("Simple_chemical", "Complex", "Protein", "Protein_with_site", "Gene_or_gene_product", "GENE")
  val simpleProteinLabels = Seq("Protein", "Gene_or_gene_product")
  val siteLabels = Seq("Site", "Protein_with_site")

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
    val theme = state.mentionsFor(sent, mention("theme").head.start, simpleProteinLabels).head
    val cause = if (mention contains "cause") state.mentionsFor(sent, mention("cause").head.start, simpleProteinLabels).headOption else None
    val args = if (cause.isDefined) Map("Theme" -> Seq(theme), "Cause" -> Seq(cause.get)) else Map("Theme" -> Seq(theme))
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkComplexEntity(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    // construct an event mention from a complex entity like "Protein_with_site"
    //mention("protein").foreach(interval => println(doc.sentences(sent).words.slice(interval.start, interval.end).mkString(" ")))
    val protein = state.mentionsFor(sent, mention("protein").head.start, simpleProteinLabels).head
    val site = state.mentionsFor(sent, mention("site").head.start, Seq("Site")).head
    val event = new RelationMention(label, Map("Protein" -> Seq(protein), "Site" -> Seq(site)), sent, doc, ruleName)

    Seq(event)
  }

  def mkMultiSite(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    // construct an event mention from a complex entity like "Protein_with_site"

    // Will this be a problem?  Protein_with_site mentions are actually EventMentions
    val parent = state.mentionsFor(sent, mention("parent").head.start, Seq("Site")).head.asInstanceOf[TextBoundMention]

    val site = new TextBoundMention(label, mention("site").head, sent, doc, ruleName)
    val event = new RelationMention(label,  Map("Parent" -> Seq(parent), "Site" -> Seq(site)), sent, doc, ruleName)

    Seq(event)
  }

  def mkSimpleEvent(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    // Don't change this, but feel free to make a new action based on this one.

    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)

    def getMentions(argName: String, validLabels: Seq[String]): Seq[Mention] = mention.getOrElse(argName, Nil) match {
      case Nil => Seq();
      case someArgs => state.mentionsFor(sent, someArgs.map(_.start), validLabels).distinct;
    }

    val themes = getMentions("theme",proteinLabels)
    // Only propagate EventMentions containing a theme
    if (themes.isEmpty) return Nil

    val sites = getMentions("site", siteLabels)

    val causes = getMentions("cause", proteinLabels)

    val mentions = trigger match {
      case hasCauseHasThemeHasSite if causes.nonEmpty && themes.nonEmpty && sites.nonEmpty => for (cause <- causes; site <- sites; theme <- themes) yield new EventMention(label, trigger, Map("Theme" -> Seq(theme), "Site" -> Seq(site), "Cause" -> Seq(cause)), sent, doc, ruleName)
      case hasCauseHasThemeNoSite if causes.nonEmpty && themes.nonEmpty && sites.isEmpty => for (cause <- causes; theme <- themes) yield new EventMention(label, trigger, Map("Theme" -> Seq(theme), "Cause" -> Seq(cause)), sent, doc, ruleName)
      case noCauseHasThemeHasSite if causes.isEmpty && sites.nonEmpty && themes.nonEmpty => for (site <- sites; theme <- themes) yield new EventMention(label, trigger, Map("Theme" -> Seq(theme), "Site" -> Seq(site)), sent, doc, ruleName)
      case noCauseNoSiteHasTheme if causes.isEmpty && sites.isEmpty && themes.nonEmpty => for (theme <- themes) yield new EventMention(label, trigger, Map("Theme" -> Seq(theme)), sent, doc, ruleName)
      case _ => Seq()
    }
    if (mentions.nonEmpty) trigger +: mentions else Nil
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
    val themes = mention("theme") flatMap (m => state.mentionsFor(sent, m.start, simpleProteinLabels))
    // we may get the same entity several times but we want distinct entities only
    val args = Map("Theme" -> themes.distinct)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkExchange(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val theme1 = mention("theme1") flatMap (m => state.mentionsFor(sent, m.start, "Simple_chemical"))
    val theme2 = mention("theme2") flatMap (m => state.mentionsFor(sent, m.start, "Simple_chemical"))
    val goal = mention("goal") flatMap (m => state.mentionsFor(sent, m.start, simpleProteinLabels))
    val cause = mention("cause") flatMap (m => state.mentionsFor(sent, m.start, simpleProteinLabels))
    val args = Map("Theme1" -> theme1, "Theme2" -> theme2, "Goal" -> goal, "Cause" -> cause)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkDegradation(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val theme = mention("theme") flatMap (m => state.mentionsFor(sent, m.start, simpleProteinLabels))
    val cause = mention("cause") flatMap (m => state.mentionsFor(sent, m.start, simpleProteinLabels))
    val args = Map("Theme" -> theme, "Cause" -> cause)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }
}
