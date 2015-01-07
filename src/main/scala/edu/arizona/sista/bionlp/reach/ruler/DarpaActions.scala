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
  val eventLabels = Seq("Phosphorylation", "Exchange", "Hydroxylation", "Ubiquitination", "Binding", "Degradation", "Hydrolysis", "Transcription", "Up_regulation", "Down_regulation", "Transport")

  def mkTextBoundMention(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    //mention("--GLOBAL--").foreach(interval => println(doc.sentences(sent).words.slice(interval.start, interval.end).mkString(" ")))
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

    val proteins = state.mentionsFor(sent, mention("protein").flatMap(_.toSeq), simpleProteinLabels).distinct
    val sites = state.mentionsFor(sent, mention("site").flatMap(_.toSeq), Seq("Site")).distinct
    val events = for (protein <- proteins; site <- sites) yield new RelationMention(label, Map("Protein" -> Seq(protein), "Site" -> Seq(site)), sent, doc, ruleName)
    events
  }

  def mkProteinWithSiteSyntax(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    // construct an event mention from a complex entity like "Protein_with_site"
    val trigger = state.mentionsFor(sent, mention("trigger").map(_.start))
    val proteins = if (mention contains "protein") state.mentionsFor(sent, mention("protein").map(_.start), simpleProteinLabels) else trigger
    val sites = if (mention contains "site") state.mentionsFor(sent, mention("site").map(_.start), Seq("Site")) else trigger

    val events = for (protein <- proteins; site <- sites) yield new RelationMention(label, Map("Protein" -> Seq(protein), "Site" -> Seq(site)), sent, doc, ruleName)
    events
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
    // println(s"args for $ruleName: ${mention.keys.flatMap(k => mention(k).flatMap(m => doc.sentences(sent).words.slice(m.start, m.end))).mkString(", ")}")

    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)

    def getMentions(argName: String, validLabels: Seq[String]): Seq[Mention] = mention.getOrElse(argName, Nil) match {
      case Nil => Seq();
      case someArgs => state.mentionsFor(sent, someArgs.map(_.start), validLabels).distinct;
    }

    def filterRelationMentions(mentions:Seq[Mention], argNames:Seq[String]):Seq[Mention]= {

      // unpack RelationMention arguments
      val relationMentions =
        mentions
        .filter(_.isInstanceOf[RelationMention])
        .map(_.asInstanceOf[RelationMention])

      // check all arguments
      (for (arg <- argNames; rel <- relationMentions) yield rel.arguments.getOrElse(arg, Seq()))
        .flatten
        .distinct
    }

    // We want to unpack relation mentions...
    val allThemes = getMentions("theme", proteinLabels)

    // unpack any RelationMentions and keep only mention matching the set of valid TextBound Entity labels
    val themes = allThemes.filter(!_.isInstanceOf[RelationMention]) ++
      filterRelationMentions(allThemes, proteinLabels.filter(_!= "Protein_with_site"))

    // Only propagate EventMentions containing a theme
    if (themes.isEmpty) return Nil

    // unpack any RelationMentions
    val sites = getMentions("site", siteLabels) ++ filterRelationMentions(allThemes, Seq("Site"))

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

  def mkComplexEvent(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    // Don't change this, but feel free to make a new action based on this one.

    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)

    val themes = state.mentionsFor(sent, mention("theme").map(_.start))
    // Only propagate EventMentions containing a theme
    if (themes.isEmpty) return Nil

    val causes = state.mentionsFor(sent, mention("cause").map(_.start))

    val mentions = trigger match {
      case hasCauseHasTheme if causes.nonEmpty && themes.nonEmpty => for (cause <- causes; theme <- themes) yield new EventMention(label, trigger, Map("Theme" -> Seq(theme), "Cause" -> Seq(cause)), sent, doc, ruleName)
      case noCauseHasTheme if causes.isEmpty && themes.nonEmpty => for (theme <- themes) yield new EventMention(label, trigger, Map("Theme" -> Seq(theme)), sent, doc, ruleName)
      case _ => Seq()
    }
    if (mentions.nonEmpty) trigger +: mentions else Nil
  }

  def mkRegulation(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val controller = for {
      m <- mention("controller")
      c <- state.mentionsFor(sent, m.toSeq, "Protein").distinct
    } yield c
    val controlled = for {
      m <- mention("controlled")
      c <- state.mentionsFor(sent, m.toSeq, "Phosphorylation").distinct
      if c.isInstanceOf[EventMention]
    } yield c
    val args = Map("Controller" -> controller, "Controlled" -> controlled)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkBindingEvent(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    //println(s"args for $ruleName: ${mention.keys.flatMap(k => mention(k).flatMap(m => doc.sentences(sent).words.slice(m.start, m.end))).mkString(", ")}")
    val themes = for {
      name <- mention.keys
      if name startsWith "theme"
      m <- mention(name)
      theme <- state.mentionsFor(sent, m.start, "Simple_chemical" +: simpleProteinLabels)
    } yield theme
    val args = Map("Theme" -> themes.toSeq.distinct)
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

  def mkHydrolysis(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val theme = mention("theme") flatMap (m => state.mentionsFor(sent, m.start, proteinLabels))
    //val cause = mention("cause") flatMap (m => state.mentionsFor(sent, m.start, proteinLabels))
    //val args = Map("Theme" -> theme, "Cause" -> cause)
    val args = Map("Theme" -> theme)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }
  
  def mkTransport(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val theme = state.mentionsFor(sent, mention("theme").head.start, Seq("Protein", "Gene_or_gene_product", "Small_molecule"))
    val src = state.mentionsFor(sent, mention("source").head.start, Seq("Cellular_component"))

    val dst = mention.getOrElse("destination", Nil) flatMap (m => state.mentionsFor(sent, m.start, Seq("Cellular_component")))
    
    val args = Map("Theme" -> theme, "Source" -> src, "Destination" -> dst)
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

}
