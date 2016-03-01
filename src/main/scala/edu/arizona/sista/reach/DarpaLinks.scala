package edu.arizona.sista.reach

import edu.arizona.sista.coref.{AntecedentSelector, LinearSelector, Links}
import edu.arizona.sista.coref.CorefUtils._
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.utils.DependencyUtils._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.struct.Interval

class DarpaLinks(doc: Document) extends Links {

  val debug: Boolean = false
  val verbose: Boolean = debug
  val defaultSelector: AntecedentSelector = new LinearSelector

  /**
    * Link a mention to the closest prior mention with exactly the same string, excluding generic mentions (e.g. 'it'
    * won't match with 'it'). This probably doesn't do anything to help event recall, but it shouldn't hurt, either.
    *
    * @param mentions All mentions
    * @param selector Rule for selecting the best antecedent from candidates
    * @return The same mentions but with new links (antecedents) added
    */
  def exactStringMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Exact entity string matching=====")
    val sameText = mentions
      .filter(x => x.isInstanceOf[CorefTextBoundMention] &&
        !x.asInstanceOf[CorefTextBoundMention].isGeneric &&
        !x.asInstanceOf[CorefTextBoundMention].hasGenericMutation)
      .groupBy(m => m.text.toLowerCase + "(" + m.mutants.map(_.text).mkString("/") + ")")
      .filter(_._2.toSeq.length > 1)
    sameText.foreach {
      case (ent, ms) =>
        ms.foldLeft(Set.empty: Set[CorefMention])((prev, curr) => {
          if (curr.antecedents.isEmpty && !curr.isGeneric) {
            if (debug) println(s"${curr.text} matches ${prev.map(_.text).mkString(", ")}")
            curr.antecedents ++= prev
          }
          Set(curr)
        })
    }
    mentions
  }

  /**
    * Link a mention to the closest prior mention with exactly the same grounding ID, excluding potentially generic
    * mentions (e.g. 'it' won't match with 'it'). This probably doesn't do anything to help event recall, but it shouldn't hurt, either.
    *
    * @param mentions All mentions
    * @param selector Rule for selecting the best antecedent from candidates
    * @return The same mentions but with new links (antecedents) added
    */
  def groundingMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Entity grounding matching=====")
    // exact grounding
    val sameGrounding = mentions
      .filter(x => x.isInstanceOf[CorefTextBoundMention] && !x.asInstanceOf[CorefTextBoundMention].isGeneric)
      .filter(x => x.asInstanceOf[CorefTextBoundMention].isGrounded)
      .groupBy(m => m.asInstanceOf[CorefTextBoundMention].grounding.get.id)
    sameGrounding.foreach {
      case (gr, ms) =>
        ms.foldLeft(Set.empty: Set[CorefMention])((prev, curr) => {
          if (curr.antecedents.isEmpty && !curr.isGeneric) {
            if (debug) println(s"${curr.text} matches ${prev.map(_.text).mkString(", ")}")
            curr.antecedents ++= prev
          }
          Set(curr)
        })
    }
    mentions
  }

  def mutantProteinMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Mutant protein matching=====")

    val tbms = mentions.filter(_.isInstanceOf[CorefTextBoundMention])
    val gms = tbms.filter(m => m.isInstanceOf[CorefTextBoundMention] && m.mutants.exists(mut => mut.isGeneric))

    gms.foreach {
      case gm =>
        if (verbose) println(s"Searching for ${gm.number.toString} antecedents to '${gm.text} ${gm.mutants.find(_.isGeneric).get.text}'")

        val cands = tbms.filter { m =>
          m.precedes(gm) &&
            !m.isGeneric &&
            m.isGrounded &&
            (m.grounding.get.equals(gm.grounding.get) | gm.isGeneric) &&
            (m.mutants.nonEmpty | gm.tags.get.head.takeRight(1) == "$") &&
            m.mutants.forall(mut => !mut.isGeneric)
        }

        if (verbose) println(s"Candidates are '${cands.map(c => c.text + c.mutants.map(_.text).mkString(" ", " ", "")).mkString("', '")}'")

        val ants = selector(gm, cands diff Seq(gm), gm.number)
        if (debug) ants.foreach { ant =>
          println(s"${gm.text} ${gm.mutants.find(mut => mut.isGeneric).get.text} links " +
            s"to ${ant.text} ${ant.mutants.map(_.text).mkString("/")}")
        }

        gm.antecedents ++= ants
        gm.sieves += "mutantProteinMatch"
    }
    mentions
  }

  /**
    * Match two mentions, at least one generic, in which the later mention's head is in the earlier one and the later
    * mention's words are a subset of the earlier one.
    *
    * @param mentions All mentions
    * @param selector Rule for selecting the best antecedent from candidates
    * @return The same mentions but with new links (antecedents) added.
    */
  def strictHeadMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Strict head matching=====")
    // split TBMs from other mentions -- we'll only be working on TBMs
    val (tbms, hasArgs) = mentions.partition(m => m.isInstanceOf[CorefTextBoundMention])
    // split generic mentions from specific mentions -- we'll only be working on generics
    val (gnrc, spcfc) = tbms.partition(_.isGeneric)
    for {
      g <- gnrc
      // save pronominals for later -- only noun phrases are of interest here
      if !g.isClosedClass & g.antecedents.isEmpty
    } {
      // expand the mention so we can find its head
      val gExpanded = expand(g)
      // get the head of the noun phrase the mention is in
      val hd = doc.sentences(g.sentence)
        .words(findHeadStrict(gExpanded, doc.sentences(g.sentence)).getOrElse(g.tokenInterval.end - 1)).toLowerCase
      if (verbose) println(s"Searching for ${g.number.toString} antecedents to '${g.text}${g.mutants.map(_.text).mkString(" ", " ", "")}' expanded to " +
        s"'${doc.sentences(g.sentence).words.slice(gExpanded.start, gExpanded.end).mkString(" ")}' with head '$hd'")
      // what other tbms have this same head?
      val cands = tbms.filter { m =>
        val mExpanded = expand(m)
        val wdsExpanded = m.sentenceObj.words.slice(mExpanded.start, mExpanded.end).toSeq
        m.precedes(g) &&
          !g.isGeneric &&
          wdsExpanded.contains(hd) &&
          m.labels == g.labels &&
          !nested(gExpanded, mExpanded, doc.sentences(g.sentence), doc.sentences(m.sentence))
      }
      // use the selector to say which of the candidates is best
      val ants = selector(g, cands, g.number)
      if (debug) ants.foreach { ant =>
        println(s"${g.text}${g.mutants.map(_.text).mkString(" ", " ", "")} links to" +
          s" ${ant.text}${ant.mutants.map(_.text).mkString(" ", " ", "")}")
      }
      g.antecedents ++= ants
      g.sieves += "strictHeadMatch"
    }
    mentions
  }

  /**
    * Match two mentions where the latter mention is a closed-class anaphor, matching number
    *
    * @param mentions All mentions
    * @param selector Rule for selecting the best antecedent from candidates
    * @return The same mentions but with new links (antecedents) added.
    */
  def pronominalMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Pronominal matching=====")
    // separate out TBMs, so we can look only at arguments of events -- others are irrelevant
    val (tbms, hasArgs) = mentions.partition(m => m.isInstanceOf[CorefTextBoundMention])
    hasArgs.filter(_.antecedents.isEmpty).foreach {
      case pronominal if pronominal.arguments.values.flatten.exists(arg => arg.toCorefMention.isClosedClass) => {
        // separate the arguments into pronominal and non-pronominal
        val proMap = pronominal.arguments.map(args => args._1 -> args._2.partition(arg => arg.toCorefMention.isClosedClass))
        // exclude all the arguments of this event, plus this event itself,
        // plus all the arguments of any events that have this event as an argument
        var excludeThese = Seq(pronominal)

        if (verbose) proMap.foreach(kv =>
          println(s"${kv._1} has pronominal args (${kv._2._1.map(_.text).mkString(", ")}) and non-pronominals (${kv._2._2.map(_.text).mkString(", ")})"))

        // look at each matching generic argument in turn, in textual order
        proMap.map(pm => pm._2._1.map(v => (pm._1, v))).flatten.toSeq.sortBy(a => a._2).foreach { kv =>
          val (lbl, g) = (kv._1, kv._2.toCorefMention)
          if (verbose) println(s"Searching for ${g.number.toString} antecedents to '${g.text}' excluding ${excludeThese.map(_.text).mkString("'", "', '", "'")}")

          val gTag = g.tags.get.headOption

          if(gTag.isEmpty || gTag.head != "PRP$") {
            excludeThese ++= pronominal.arguments.values.flatten.toSeq.map(_.toCorefMention) ++
              hasArgs.filter(m => m.arguments.values.flatten.toSeq.contains(pronominal)).flatMap(_.arguments.values).flatten.map(_.toCorefMention)
          }

          val cands = lbl match {
            // controlled and controller can be EventMentions; other argument types must be TextBoundMentions
            case "controlled" => mentions.filter { m =>
              m.precedes(g) &&
                g.sentence - m.sentence < 2 &&
                m.getClass == g.getClass &&
                !m.isGeneric &&
                !excludeThese.contains(m) &&
                (m.matches("PossibleController") || m.isInstanceOf[CorefEventMention])
            }
            case "controller" => mentions.filter { m =>
              m.precedes(g) &&
                g.sentence - m.sentence < 2 &&
                m.getClass == g.getClass &&
                !m.isGeneric &&
                !excludeThese.contains(m) &&
                (m.matches("PossibleController") || m.isInstanceOf[CorefEventMention])
            }
            case _ => tbms.filter { m =>
              m.precedes(g) &&
                g.sentence - m.sentence < 2 &&
                m.getClass == g.getClass &&
                !m.isGeneric &&
                !excludeThese.contains(m) &&
                m.matches("PossibleController")
            }
          }
          if (verbose) println(s"Candidates are '${cands.map(c => c.text + c.mutants.map(_.text).mkString(" ", " ", "")).mkString("', '")}'")

          // apply selector to candidates
          val ants = selector(g.asInstanceOf[CorefTextBoundMention], cands, g.toCorefMention.number)
          if (verbose) println(s"matched '${ants.map(a => a.text + a.mutants.map(_.text).mkString(" ", " ", "")).mkString(", ")}'")

          // We must check for the anaphor mention in the state, because if it's not, we'll get an error upon
          // trying to link the two
          val gInState = mentions.find(m => g == m)
          if (gInState.isDefined) {
            // Mark the anaphor as having the selected antecedent
            gInState.get.antecedents ++= ants
            // Make sure we don't link to the same antecedent for other arguments to this event
            excludeThese ++= ants
            pronominal.sieves += "pronominalMatch"
          }
        }
      }
      case _ => ()
    }
    mentions
  }

  /**
    * Match two mentions where the latter mention is one of a specific set of generic mentions with a known class, e.g.
    * 'this protein' is known to have the label 'Protein'
    *
    * @param mentions All mentions
    * @param selector Rule for selecting the best antecedent from candidates
    * @return The same mentions but with new links (antecedents) added.
    */
  def nounPhraseMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Noun phrase matching=====")

    // only apply this matcher to arguments to events -- others are irrelevant
    val (tbms, hasArgs) = mentions.partition(m => m.isInstanceOf[CorefTextBoundMention])

    hasArgs.filter(_.antecedents.isEmpty).foreach {
      case np if np.arguments.values.flatten.exists(arg =>
        arg.isInstanceOf[TextBoundMention] &&
          arg.toCorefMention.isGenericNounPhrase &&
          arg.toCorefMention.antecedents.isEmpty) => {
        // separate the arguments into generic noun phrases and others
        val npMap = np.arguments.map(args =>
          args._1 -> args._2.partition(arg =>
            arg.isInstanceOf[TextBoundMention] &&
              arg.toCorefMention.isGenericNounPhrase &&
              arg.toCorefMention.antecedents.isEmpty
          )
        )

        // exclude all the arguments of this event, plus this event itself,
        // plus all the arguments of any events that have this event as an argument
        var excludeThese = np.arguments.values.flatten.toSeq ++
          Seq(np) ++
          hasArgs.filter(m => m.arguments.values.flatten.toSeq.contains(np)).flatMap(_.arguments.values).flatten

        // look at each matching generic argument in turn, in textual order
        // Note: Do *not* turn this into flatMap
        npMap.map(npm => npm._2._1.map(v => (npm._1, v))).flatten.toSeq.sortBy(x => x._2).foreach { kv =>
          val (lbl, g) = (kv._1, kv._2.toCorefMention)
          if (verbose) println(s"Searching for ${g.number.toString} antecedents to '${g.text}${g.mutants.map(_.text).mkString(" ", " ", "")}' " +
            s"excluding ${excludeThese.map(_.text).mkString("'", "', '", "'")}")

          val cands = lbl match {
            // controlled and controller can be EventMentions; other argument types must be TextBoundMentions
            case "controlled" => mentions.filter { m =>
              m.precedes(g) &&
                g.sentence - m.sentence < 2 &&
                !m.isGeneric &&
                !excludeThese.contains(m) &&
                (m.matches("PossibleController") || m.isInstanceOf[CorefEventMention]) &&
                compatibleMutants(m, g) &&
                g.labels.filter(l => l != "Generic_entity" && l != "Generic_event").forall(x => m.labels.contains(x))
            }
            case "controller" => mentions.filter { m =>
              m.precedes(g) &&
                g.sentence - m.sentence < 2 &&
                !m.isGeneric &&
                !excludeThese.contains(m) &&
                (m.matches("PossibleController") || m.isInstanceOf[CorefEventMention]) &&
                compatibleMutants(m, g) &&
                g.labels.filter(l => l != "Generic_entity" && l != "Generic_event").forall(x => m.labels.contains(x))
            }
            case _ => tbms.filter { m =>
              m.precedes(g) &&
                g.sentence - m.sentence < 2 &&
                !m.isGeneric &&
                !excludeThese.contains(m) &&
                m.matches("PossibleController") &&
                compatibleMutants(m, g) &&
                g.labels.filter(l => l != "Generic_entity" && l != "Generic_event").forall(x => m.labels.contains(x))
            }
          }
          if (verbose) println(s"Candidates are '${cands.map(c => c.text + c.mutants.map(_.text).mkString(" ", " ", "")).mkString("', '")}'")

          // apply selector to candidates
          val ants = selector(g.asInstanceOf[CorefTextBoundMention], cands, g.toCorefMention.number)
          if (verbose) println(s"matched '${ants.map(a => a.text + a.mutants.map(_.text).mkString(" ", " ", "")).mkString(", ")}'")

          // We must check for the anaphor mention in the state, because if it's not, we'll get an error upon
          // trying to link the two
          val gInState = mentions.find(m => g == m)
          if (gInState.isDefined) {
            // Mark the anaphor as having the selected antecedent
            gInState.get.antecedents ++= ants
            // Make sure we don't link to the same antecedent for other arguments to this event
            excludeThese ++= ants
            np.sieves += "nounPhraseMatch"
          }
        }
      }
      case _ => ()
    }

    mentions
  }

  /**
    * Examine complex events with generic simple events as arguments, searching for the best match of the same label,
    * e.g. "ASPP1 promotes this phosphorylation." will search for phosphorylations before this sentence.
    *
    * @param mentions All mentions
    * @param selector Rule for selecting the best antecedent from candidates
    * @return The same mentions but with new links (antecedents) added.
    */
  def simpleEventMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Simple event matching=====\n")

    val seLabels = Set(
      "Phosphorylation",
      "Ubiquitination",
      "Hydroxylation",
      "Sumoylation",
      "Glycosylation",
      "Acetylation",
      "Farnesylation",
      "Ribosylation",
      "Methylation",
      "Hydrolysis",
      "Translocation",
      "Binding")

    // We're only looking for generic simple events that are arguments of complex events
    val (complex, others) = mentions.partition(m => m matches "ComplexEvent")
    val (simplex, _) = others.partition(m => m matches "SimpleEvent")
    // We need to have the specific event mentions ready to match our anaphors with
    val (_, specifics) = simplex.partition(m => m matches "Generic_event")

    // ComplexEvent mentions one by one. Ignore ComplexEvents with no generic SimpleEvent arguments, and arguments that
    // are merely triggers of more complete SimpleEvents
    complex.filter(_.antecedents.isEmpty).foreach { case cx if cx.arguments.values.flatten.exists(arg => arg.matches("Generic_event") &&
      !specifics.filter(_.isInstanceOf[EventMention]).exists(sfc =>
        sfc.asInstanceOf[CorefEventMention].trigger == arg.asInstanceOf[CorefEventMention].trigger)) =>

      // Create a map just like m.arguments but with the values partitioned into generic and non-generic
      val argMap = cx.arguments.map(args => args._1 -> args._2.partition(arg => arg matches "Generic_event"))

      // These are the mentions excluded from matching with generic arguments: mentions participating in this event
      // already, and mentions participating in some parent event (that has this ComplexEvent as an argument). It's a
      // var because we'll be adding found antecedents as we go.
      var excludeThese = cx.arguments.values.flatten.toSeq ++
        specifics.filter(sfc =>
          cx.arguments.values.flatten.toSeq.filter(_.isInstanceOf[EventMention])
            .map(_.asInstanceOf[EventMention].trigger).contains(sfc.asInstanceOf[EventMention].trigger))

      // Looking just at the generic arguments, since we don't need to find antecedents to full mentions.
      // Note: Do *not* turn this into flatMap
      argMap.map(arg => arg._2._1.map(v => (arg._1, v))).flatten.toSeq.sortBy(x => x._2).foreach { kv =>
        // the label for the argument type (e.g. "theme") and the generic argument we need an antecedent for.
        val (lbl, g) = kv

        val gInState = mentions.find(m => g == m)

        // first check if g already has had an antecedent found during a different ComplexEvent's search
        if (gInState.isDefined && gInState.get.antecedents.isEmpty) {

          if (verbose) println(s"Searching for antecedents to '${g.text}' excluding ${excludeThese.map(_.text).mkString("'", "', '", "'")}")

          // candidates for the generic mention g's antecedents are full SimpleEvent mentions that precede g by 0-1
          // sentences, which haven't been found as an antecedent to another argument yet, and which has matching labels
          val cands = specifics.filter(s => (s precedes g) && g.sentence - s.sentence < 2 && !excludeThese.contains(s) &&
            (s matches g.labels.toSet.intersect(seLabels).toSeq.headOption.getOrElse("")))

          if (verbose) println(s"Candidates are '${cands.map(_.text).mkString("', '")}'")

          // use the selector to choose which among the candidates is best. Assume 1 antecedent for now.
          val ant = selector(g.asInstanceOf[CorefMention], cands, 1)

          if (verbose) println(s"matched '${ant.map(_.text).mkString(", ")}'")

          gInState.get.antecedents ++= ant
          excludeThese ++= ant
          cx.sieves += "simpleEventMatch"
        }
      }
    case _ => ()
    }

    mentions
  }
}
