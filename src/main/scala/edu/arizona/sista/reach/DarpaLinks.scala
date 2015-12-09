package edu.arizona.sista.reach

import edu.arizona.sista.coref.{AntecedentSelector, LinearSelector, Links}
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.utils.DependencyUtils._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.struct.Interval

class DarpaLinks(doc: Document) extends Links {

  val debug: Boolean = false
  val verbose: Boolean = false
  val defaultSelector: AntecedentSelector = new LinearSelector

  /**
   * Link a mention to the closest prior mention with exactly the same string, excluding generic mentions (e.g. 'it'
   * won't match with 'it'). This probably doesn't do anything to help event recall, but it shouldn't hurt, either.
   * @param mentions All mentions
   * @param selector Rule for selecting the best antecedent from candidates
   * @return The same mentions but with new links (antecedents) added
   */
  def exactStringMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Exact entity string matching=====")
    val sameText = mentions
      .filter(x => x.isInstanceOf[CorefTextBoundMention] && !x.asInstanceOf[CorefTextBoundMention].isGeneric)
      .groupBy(m => m.text.toLowerCase)
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
   * @param mentions All mentions
   * @param selector Rule for selecting the best antecedent from candidates
   * @return The same mentions but with new links (antecedents) added
   */
  def groundingMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Exact entity grounding matching=====")
    // exact grounding
    val sameGrounding = mentions
      .filter(x => x.isInstanceOf[CorefTextBoundMention] && !x.asInstanceOf[CorefTextBoundMention].isGeneric)
      .filter(x => x.asInstanceOf[CorefTextBoundMention].isGrounded)
      .groupBy(m => m.asInstanceOf[CorefTextBoundMention].xref.get.id)
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

  /**
   * Match two mentions, at least one generic, in which the later mention's head is in the earlier one and the later
   * mention's words are a subset of the earlier one.
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
      if !isPronominal(g) & g.antecedents.isEmpty
    } {
      // expand the mention so we can find its head
      val gExpanded = expand(g)
      // get the head of the noun phrase the mention is in
      val hd = doc.sentences(g.sentence)
        .words(findHeadStrict(gExpanded, doc.sentences(g.sentence)).getOrElse(g.tokenInterval.end - 1)).toLowerCase
      if (verbose) println(s"Searching for antecedents to '${g.text}' expanded to " +
        s"'${doc.sentences(g.sentence).words.slice(gExpanded.start, gExpanded.end).mkString(" ")}' with head '$hd'")
      // what other tbms have this same head?
      val cands = tbms.filter { m =>
        val mExpanded = expand(m)
        val wdsExpanded = m.sentenceObj.words.slice(mExpanded.start, mExpanded.end).toSeq
        !g.isGeneric &&
          wdsExpanded.contains(hd) &&
          !nested(gExpanded, mExpanded, doc.sentences(g.sentence), doc.sentences(m.sentence)) &&
          m.precedes(g)
      }
      // use the selector to say which of the candidates is best
      val ants = selector(g, cands, g.number)
      if (debug) ants.foreach { ant => println(s"${g.text} links to ${ant.text}") }
      g.antecedents ++= ants
      g.sieves += "strictHeadMatch"
    }
    mentions
  }

  /**
   * Match two mentions where the latter mention is a closed-class anaphor, matching number
   * @param mentions All mentions
   * @param selector Rule for selecting the best antecedent from candidates
   * @return The same mentions but with new links (antecedents) added.
   */
  def pronominalMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Pronominal matching=====")
    val state = State(mentions)
    // separate out TBMs, so we can look only at arguments of events -- others are irrelevant
    val (tbms, hasArgs) = mentions.partition(m => m.isInstanceOf[CorefTextBoundMention])
    hasArgs.filter(_.antecedents.isEmpty).foreach {
      case pronominal if pronominal.arguments.values.flatten.exists(arg => isPronominal(arg)) => {
        // separate the arguments into pronominal and non-pronominal
        val proMap = pronominal.arguments.map(args => args._1 -> args._2.partition(arg => isPronominal(arg)))
        // exclude all the arguments of this event, plus this event itself,
        // plus all the arguments of any events that have this event as an argument
        var excludeThese = pronominal.arguments.values.flatten.toSeq ++
          Seq(pronominal) ++
          hasArgs.filter(m => m.arguments.values.flatten.toSeq.contains(pronominal)).flatMap(_.arguments.values).flatten
        if (verbose) proMap.foreach(kv =>
          println(s"${kv._1} has pronominal args (${kv._2._1.map(_.text).mkString(", ")}) and non-pronominals (${kv._2._2.map(_.text).mkString(", ")})"))

        // look at each matching generic argument in turn, in textual order
        proMap.map(pm => pm._2._1.map(v => (pm._1, v))).flatten.toSeq.sortBy(a => a._2).foreach { kv =>
          val (lbl, g) = kv
          if (verbose) println(s"Searching for antecedents to '${g.text}' excluding ${excludeThese.map(_.text).mkString("'", "', '", "'")}")

          // look for the right number of antecedents
          val card = cardinality(g)
          val cands = lbl match {
            // controlled and controller can be EventMentions; other argument types must be TextBoundMentions
            case "controlled" => mentions.filter { m =>
              m.getClass == g.getClass && !m.isGeneric && m.precedes(g) && g.sentence - m.sentence < 2 && !excludeThese.contains(m)
            }
            case "controller" => mentions.filter { m =>
              m.getClass == g.getClass && !m.isGeneric && m.precedes(g) && g.sentence - m.sentence < 2 && !excludeThese.contains(m)
            }
            case _ => tbms.filter { m =>
              m.getClass == g.getClass && !m.isGeneric && m.precedes(g) && g.sentence - m.sentence < 2 && !excludeThese.contains(m)
            }
          }
          if (verbose) println(s"Candidates are '${cands.map(_.text).mkString("', '")}'")

          // apply selector to candidates
          val ants = selector(g.asInstanceOf[CorefTextBoundMention], cands, card)
          if (verbose) println(s"matched '${ants.map(_.text).mkString(", ")}'")

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
   * @param mentions All mentions
   * @param selector Rule for selecting the best antecedent from candidates
   * @return The same mentions but with new links (antecedents) added.
   */
  def nounPhraseMatch(mentions: Seq[CorefMention], selector: AntecedentSelector = defaultSelector): Seq[CorefMention] = {
    if (debug) println("\n=====Noun phrase matching=====")

    // only apply this matcher to arguments to events -- others are irrelevant
    val (tbms, hasArgs) = mentions.partition(m => m.isInstanceOf[CorefTextBoundMention])

    hasArgs.filter(_.antecedents.isEmpty).foreach {
      case np if np.arguments.values.flatten.exists(arg => isGenericNounPhrase(arg)) => {

        // separate the arguments into generic noun phrases and others
        val npMap = np.arguments.map(args => args._1 -> args._2.partition(arg => isGenericNounPhrase(arg)))

        // exclude all the arguments of this event, plus this event itself,
        // plus all the arguments of any events that have this event as an argument
        var excludeThese = np.arguments.values.flatten.toSeq ++
          Seq(np) ++
          hasArgs.filter(m => m.arguments.values.flatten.toSeq.contains(np)).flatMap(_.arguments.values).flatten

        // look at each matching generic argument in turn, in textual order
        npMap.map(npm => npm._2._1.map(v => (npm._1, v))).flatten.toSeq.sortBy(x => x._2).foreach { kv =>
          val (lbl, g) = kv
          if (verbose) println(s"Searching for antecedents to '${g.text}' excluding ${excludeThese.map(_.text).mkString("'", "', '", "'")}")

          // Look for the right number of antecedents
          val card = cardinality(g)
          val cands = lbl match {
            // controlled and controller can be EventMentions; other argument types must be TextBoundMentions
            case "controlled" => mentions.filter { m =>
              !m.isGeneric && m.precedes(g) && g.sentence - m.sentence < 2 && !excludeThese.contains(m) &&
                g.labels.filter(l => l != "Generic_entity" && l != "Generic_event").forall(x => m.labels.contains(x))
            }
            case "controller" => mentions.filter { m =>
              !m.isGeneric && m.precedes(g) && g.sentence - m.sentence < 2 && !excludeThese.contains(m) &&
                g.labels.filter(l => l != "Generic_entity" && l != "Generic_event").forall(x => m.labels.contains(x))
            }
            case _ => tbms.filter { m =>
              !m.isGeneric && m.precedes(g) && g.sentence - m.sentence < 2 && !excludeThese.contains(m) &&
                g.labels.filter(l => l != "Generic_entity" && l != "Generic_event").forall(x => m.labels.contains(x))
            }
          }
          if (verbose) println(s"Candidates are '${cands.map(_.text).mkString("', '")}'")

          // apply selector to candidates
          val ants = selector(g.asInstanceOf[CorefTextBoundMention], cands, card)
          if (verbose) println(s"matched '${ants.map(_.text).mkString(", ")}'")

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
    val (sevents, ignore) = others.partition(m => m matches "SimpleEvent")
    // We need to have the specific event mentions ready to match our anaphors with
    val (generics, specifics) = sevents.partition(m => m matches "Generic_event")

    complex.filter(_.antecedents.isEmpty).foreach{ case cx if cx.arguments.values.flatten.exists(arg => arg.matches("Generic_event") &&
      !specifics.filter(_.isInstanceOf[EventMention]).exists(sfc =>
        sfc.asInstanceOf[CorefEventMention].trigger == arg.asInstanceOf[CorefEventMention].trigger)) =>
      val argMap = cx.arguments.map(args => args._1 -> args._2.partition(arg => arg matches "Generic_event"))

      var excludeThese = cx.arguments.values.flatten.toSeq ++
        specifics.filter(sfc =>
          cx.arguments.values.flatten.toSeq.filter(_.isInstanceOf[EventMention])
            .map(_.asInstanceOf[EventMention].trigger).contains(sfc.asInstanceOf[EventMention].trigger))

      argMap.map(arg => arg._2._1.map(v => (arg._1, v))).flatten.toSeq.sortBy(x => x._2).foreach { kv =>
        val (lbl, g) = kv

        if (verbose) println(s"Searching for antecedents to '${g.text}' excluding ${excludeThese.map(_.text).mkString("'", "', '", "'")}")

        val cands = specifics.filter(s => (s precedes g) && g.sentence - s.sentence < 2 && !excludeThese.contains(s) &&
          (s matches g.labels.toSet.intersect(seLabels).toSeq.headOption.getOrElse("")))

        if (verbose) println(s"Candidates are '${cands.map(_.text).mkString("', '")}'")

        val ant = selector(g.asInstanceOf[CorefMention], cands, 1)

        if (verbose) println(s"matched '${ant.map(_.text).mkString(", ")}'")

        val gInState = mentions.find(m => g == m)
        if (gInState.isDefined) {
          gInState.get.antecedents ++= ant
          excludeThese ++= ant
          cx.sieves += "simpleEventMatch"
        }
      }
    case _ => ()
    }

    mentions
  }

  /**
   * Are all the words (minus stopwords) in the obj mention also in the subj mention?
   */
  def includes(subj: CorefMention, obj: CorefMention): Boolean = {
    val stopWords = Set(
      "the",
      "a"
    )

    (obj.words.toSet - stopWords - subj.words.toSet).isEmpty
  }

  /**
   * From a mention, use the dependency graph to expand the interval to the noun phrase the mention is a part of
   */
  def expand(mention: Mention): Interval = {
    val sent = doc.sentences(mention.sentence)
    val graph = sent.dependencies.getOrElse(return mention.tokenInterval)

    val localHead = findHeadStrict(mention.tokenInterval, sent).getOrElse(mention.tokenInterval.end - 1)

    var npHead = localHead

    var searchingHead = true

    // keep traversing incomingEdges until you reach the head of the NP
    while (searchingHead) {
      val newHead = try {
        graph.getIncomingEdges(npHead).find(edge => edge._2 == "nn")
      } catch {
        case e: Throwable => None
      }
      if (newHead.isDefined) npHead = newHead.get._1
      else searchingHead = false
    }

    subgraph(Interval(npHead), sent).getOrElse(mention.tokenInterval)
  }

  /**
   * Is this mention a pronoun or other closed-class anaphor?
   */
  def isPronominal(mention: Mention): Boolean = {
    (detMap.contains(mention.text) || headMap.contains(mention.text)) && mention.isInstanceOf[CorefTextBoundMention]
  }

  /**
   * Is this mention a generic noun phrase that isn't a pronoun or other closed-class anaphor?
   */
  def isGenericNounPhrase(mention: Mention): Boolean = {
    mention.isInstanceOf[CorefTextBoundMention] && mention.asInstanceOf[CorefMention].isGeneric && !isPronominal(mention)
  }


  final val themeMap = Map(
    "Binding" -> 2,
    "Ubiquitination" -> 1,
    "Phosphorylation" -> 1,
    "Hydroxylation" -> 1,
    "Acetylation" -> 1,
    "Farnesylation" -> 1,
    "Glycosylation" -> 1,
    "Methylation" -> 1,
    "Ribosylation" -> 1,
    "Sumoylation" -> 1,
    "Hydrolysis" -> 1,
    "Degradation" -> 1,
    "Exchange" -> 2,
    "Transcription" -> 1,
    "Transportation" -> 1,
    "Translocation" -> 1
  )

  // crucial: pass lemma so plurality isn't a concern
  def themeCardinality(eventLemma: String): Int = {
    themeMap.getOrElse(eventLemma, 1)
  }

  // generic antecedent matching with number approximation
  val detMap = Map("a" -> 1,
    "an" -> 1,
    //"the" -> 1, // assume one for now...
    "both" -> 2,
    "that" -> 1,
    "those" -> 2,
    "these" -> 2, // assume two for now...
    "this" -> 1,
    "few" -> 3,
    "some" -> 3, // assume three for now...
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "its" -> 1,
    "their" -> 2)

  val headMap = Map("it" -> 1,
    "they" -> 2,
    "theirs" -> 1,
    "them" -> 2,
    "that" -> 1,
    "both" -> 2,
    "those" -> 2,
    "these" -> 2, // assume two for now...
    "this" -> 1,
    "some" -> 3, // assume three for now...
    "one" -> 1,
    "two" -> 2,
    "three" -> 3
  )

  /**
   * Return the cardinality of a phrase based on its determiners
   */
  def detCardinality(words: Seq[String], tags: Seq[String]): Int = {
    require(words.length == tags.length)
    val somenum = words(tags.zipWithIndex.find(x => Seq("DT", "CD", "PRP$").contains(x._1)).getOrElse(return 0)._2)
    def finalAttempt(num: String): Int = try {
      num.toInt
    } catch {
      case e: NumberFormatException => 0
    }
    detMap.getOrElse(somenum, finalAttempt(somenum))
  }

  /**
   * Return the cardinality of a phrase based on its head -- is it plural?
   */
  def headCardinality(somenum: String, tag: String): Int = {
    tag match {
      case "PRP" | "PRP$" => headMap.getOrElse(somenum, 0)
      case "NNS" | "NNPS" => 2
      case "NN" | "NNP" => 1
      case _ => headMap.getOrElse(somenum, 1)
    }
  }

  /**
   * Determine the cardinality of a mention -- how many real-world entities or events does it refer to?
   */
  def cardinality(m: Mention): Int = {
    val sent = doc.sentences(m.sentence)
    val mhead = findHeadStrict(m.tokenInterval, sent).getOrElse(m.tokenInterval.start)
    val phrase = subgraph(m.tokenInterval, sent)
    val dc = detCardinality(sent.words.slice(phrase.get.start, phrase.get.end), sent.tags.get.slice(phrase.get.start, phrase.get.end))
    val hc = headCardinality(sent.words(mhead), sent.tags.get(mhead))

    m match {
      case informativeDeterminer if dc != 0 => dc
      case informativeHead if dc == 0 & hc != 0 => hc
      case _ => 1
    }
  }

  /**
   * Do we have exactly 1 unique grounding id for this Sequence of Mentions?
   * @param mentions A Sequence of mentions to compare
   * @return boolean True if all mentions share a single grounding id
   */
  def sameEntityID(mentions: Seq[Mention]): Boolean = {
    val groundings =
      mentions
        .map(_.toBioMention)
        // only look at grounded Mentions
        .filter(_.xref.isDefined)
        .map(_.xref.get)
        .toSet
    // should be 1 if all are the same entity
    groundings.size == 1
  }

}
