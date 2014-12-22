package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.core.RelationMention
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
    val theme = state.mentionsFor(sent, mention("theme").head.start, "Gene_or_gene_product").head
    val cause = if (mention contains "cause") state.mentionsFor(sent, mention("cause").head.start, "Gene_or_gene_product").headOption else None
    val args = if (cause.isDefined) Map("Theme" -> Seq(theme), "Cause" -> Seq(cause.get)) else Map("Theme" -> Seq(theme))
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkComplexEntity(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    // construct an event mention from a complex entity like "Protein_with_site"
    val protein = state.mentionsFor(sent, mention("protein").head.start, Seq("Protein", "Gene_or_gene_product")).head
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
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    // not sure if this should be simple chemical...
    // TODO: don't just check mentions, see if matched word is all uppercase, etc. (use "dumb" tricks to detect a likely entity)

    def getMentions(argName: String, validLabels: Seq[String]): Seq[Mention] = mention.getOrElse(argName, Nil) match {
      case Nil => Seq();
      case someArgs => state.mentionsFor(sent, someArgs.map(_.start), validLabels).distinct;
    }

    val themes = getMentions("theme", Seq("Simple_chemical", "Complex", "Protein_with_site", "Gene_or_gene_product", "GENE"))
    val theme1s = getMentions("theme1", Seq("Simple_chemical", "Complex", "Protein_with_site", "Gene_or_gene_product", "GENE"))
    val theme2s = getMentions("theme2", Seq("Simple_chemical", "Complex", "Protein_with_site", "Gene_or_gene_product", "GENE"))
    // Only propagate EventMentions containing a theme
    if (themes.isEmpty & (theme1s.isEmpty | theme2s.isEmpty)) return Nil

    val sites = getMentions("site", Seq("Site", "Protein_with_site"))

    val causes = getMentions("cause", Seq("Simple_chemical", "Complex", "Gene_or_gene_product", "GENE"))

    val goals = getMentions("goal", Seq("Complex", "Gene_or_gene_product", "GENE"))

    val mentions = trigger match {
      case hasCauseHas2ThemesNoSiteHasGoal if causes.nonEmpty && theme1s.nonEmpty && goals.nonEmpty => for (cause <- causes; goal <- goals; theme1 <- theme1s; theme2 <- theme2s) yield new EventMention(label, trigger, Map("Theme1" -> Seq(theme1), "Theme2" -> Seq(theme2), "Goal" -> Seq(goal), "Cause" -> Seq(cause)), sent, doc, ruleName)
      case hasCauseHas2ThemesNoSiteNoGoal if causes.nonEmpty && theme1s.nonEmpty && goals.isEmpty => for (cause <- causes; theme1 <- theme1s; theme2 <- theme2s) yield new EventMention(label, trigger, Map("Theme1" -> Seq(theme1), "Theme2" -> Seq(theme2), "Cause" -> Seq(cause)), sent, doc, ruleName)
      case noCauseHas2ThemesNoSiteHasGoal if causes.isEmpty && theme1s.nonEmpty && goals.nonEmpty => for (goal <- goals; theme1 <- theme1s; theme2 <- theme2s) yield new EventMention(label, trigger, Map("Theme1" -> Seq(theme1), "Theme2" -> Seq(theme2), "Goal" -> Seq(goal)), sent, doc, ruleName)
      case noCauseHas2ThemesNoSiteNoGoal if causes.isEmpty && theme1s.nonEmpty && goals.isEmpty => for (theme1 <- theme1s; theme2 <- theme2s) yield new EventMention(label, trigger, Map("Theme1" -> Seq(theme1), "Theme2" -> Seq(theme2)), sent, doc, ruleName)
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
    val themes = mention("theme") flatMap (m => state.mentionsFor(sent, m.start, "Gene_or_gene_product"))
    val args = Map("Theme" -> themes)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkExchange(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val theme1 = mention("theme1") flatMap (m => state.mentionsFor(sent, m.start, "Simple_chemical"))
    val theme2 = mention("theme2") flatMap (m => state.mentionsFor(sent, m.start, "Simple_chemical"))
    val goal = mention("goal") flatMap (m => state.mentionsFor(sent, m.start, "Gene_or_gene_product"))
    val cause = mention("cause") flatMap (m => state.mentionsFor(sent, m.start, "Gene_or_gene_product"))
    val args = Map("Theme1" -> theme1, "Theme2" -> theme2, "Goal" -> goal, "Cause" -> cause)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkDegradation(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val theme = mention("theme") flatMap (m => state.mentionsFor(sent, m.start, "Gene_or_gene_product"))
    val cause = mention("cause") flatMap (m => state.mentionsFor(sent, m.start, "Gene_or_gene_product"))
    val args = Map("Theme" -> theme, "Cause" -> cause)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }
}
