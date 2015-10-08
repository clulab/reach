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
   * @return The same mentions but with new links (antecedents) added
   */
  def exactStringMatch (mentions: Seq[CorefMention]): Seq[CorefMention] = {
    if (debug) println("\n=====Exact entity string matching=====")
    val sameText = mentions
      .filter(x => x.isInstanceOf[CorefTextBoundMention] && !x.asInstanceOf[CorefTextBoundMention].isGeneric)
      .groupBy(m => m.text.toLowerCase)
      .filter(_._2.toSeq.length > 1)
    sameText.foreach {
      case (ent, ms) =>
        ms.foldLeft(Set.empty: Set[Mention])((prev,curr) => {
          curr.asInstanceOf[CorefMention].antecedents = curr.asInstanceOf[CorefMention].antecedents ++ prev
          Set(curr)
        })
    }
    mentions
  }

  /**
   * Link a mention to the closest prior mention with exactly the same grounding ID, excluding potentially generic
   * mentions (e.g. 'it' won't match with 'it'). This probably doesn't do anything to help event recall, but it shouldn't hurt, either.
   * @param mentions All mentions
   * @return The same mentions but with new links (antecedents) added
   */
  def groundingMatch(mentions: Seq[CorefMention]): Seq[CorefMention] = {
    if (debug) println("\n=====Exact entity grounding matching=====")
    // exact grounding
    val sameGrounding = mentions
      .filter(x => x.isInstanceOf[CorefTextBoundMention] && !x.asInstanceOf[CorefTextBoundMention].isGeneric)
      .filter(x => x.asInstanceOf[CorefTextBoundMention].isGrounded)
      .groupBy(m => m.asInstanceOf[CorefTextBoundMention].xref.get.id)
    sameGrounding.foreach {
      case (gr, ms) =>
        ms.foldLeft(Set.empty: Set[Mention])((prev,curr) => {
          curr.asInstanceOf[CorefMention].antecedents = curr.asInstanceOf[CorefMention].antecedents ++ prev
          Set(curr)
        })
    }
    mentions
  }

  /**
   * Match two mentions, at least one generic, in which the later mention's head is in the earlier one and the later
   * mention's words are a subset of the earlier one.
   * @param mentions All mentions
   * @return The same mentions but with new links (antecedents) added.
   */
  def strictHeadMatch(mentions: Seq[CorefMention]): Seq[CorefMention] = {
    // FIXME: selector should be an input to the function
    val selector = defaultSelector

    if (debug) println("\n=====Strict head matching=====")
    // split TBMs from other mentions -- we'll only be working on TBMs
    val (tbms, hasArgs) = mentions.partition(m => m.isInstanceOf[CorefTextBoundMention])
    // split generic mentions from specific mentions -- we'll only be working on generics
    val (gnrc, spcfc) = tbms.partition(_.isGeneric)
    for {
      g <- gnrc
      // save pronominals for later -- only noun phrases are of interest here
      if !isPronominal(g)
    } {
      // expand the mention so we can find its head
      val gExpanded = expand(g)
      // get the head of the noun phrase the mention is in
      val hd = doc.sentences(g.sentence)
        .words(findHeadStrict(gExpanded, doc.sentences(g.sentence)).getOrElse(g.tokenInterval.end - 1)).toLowerCase
      if (verbose) println(s"Searching for antecedents to '${g.text}' expanded to " +
        s"'${doc.sentences(g.sentence).words.slice(gExpanded.start,gExpanded.end).mkString(" ")}' with head '$hd'")
      // what other tbms have this same head?
      val cands = tbms.filter{m =>
        val mExpanded = expand(m)
        val wdsExpanded = m.sentenceObj.words.slice(mExpanded.start,mExpanded.end).toSeq
        wdsExpanded.contains(hd) &&
          !nested(gExpanded, mExpanded, doc.sentences(g.sentence), doc.sentences(m.sentence)) &&
          m.precedes(g)}
      // use the selector to say which of the candidates is best
      val ants = selector(g, cands, g.number)
      if (debug) ants.foreach{ant => println(s"${g.text} links to ${ant.text}")}
      g.antecedents = g.antecedents ++ ants
      g.sieves = g.sieves + "strictHeadMatch"
    }
    mentions
  }

  def pronominalMatch(mentions: Seq[CorefMention]): Seq[CorefMention] = {
    // FIXME: selector should be an input to the function
    val selector = defaultSelector

    if (debug) println("\n=====Pronominal matching=====")
    // separate out TBMs, so we can look only at arguments of events -- others are irrelevant
    val (tbms, hasArgs) = mentions.partition(m => m.isInstanceOf[CorefTextBoundMention])
    hasArgs.foreach {
        case pronominal if pronominal.arguments.values.flatten.exists(arg => isPronominal(arg)) => {
          // separate the arguments into pronominal and non-pronominal
          val proMap = pronominal.arguments.map(args => args._1 -> args._2.partition(arg => isPronominal(arg)))
          // exclude all the arguments of this event, plus this event itself,
          // plus all the arguments of any events that have this event as an argument
          var excludeThese = pronominal.arguments.values.flatten.toSeq ++
            Seq(pronominal) ++
            hasArgs.filter(m => m.arguments.values.flatten.toSeq.contains(pronominal)).flatMap(_.arguments.values).flatten
          proMap.flatMap(pm => pm._2._1.map(v => (pm._1, v))).foreach{ kv =>
            val (lbl, g) = kv
            if (verbose) println(s"Searching for antecedents to '${g.text}' excluding '${excludeThese.map(_.text).mkString("', '")}'")
            val card = cardinality(g)
            val cands = lbl match {
              case "controlled" => hasArgs.filter { m =>
                !m.isGeneric && m.precedes(g) && g.sentence - m.sentence < 2 && !excludeThese.contains(m)
              }
              case _ => tbms.filter { m =>
                !m.isGeneric && m.precedes(g) && g.sentence - m.sentence < 2 && !excludeThese.contains(m)
              }
            }
            val ants = selector(g.asInstanceOf[CorefTextBoundMention], cands, card)
            if (verbose) println(s"matched '${ants.map(_.text).mkString(", ,")}'")
            val gInState = mentions.find(m => g == m)
            if (gInState.isDefined) {
              gInState.get.asInstanceOf[CorefTextBoundMention].antecedents = gInState.get.asInstanceOf[CorefTextBoundMention].antecedents ++ ants
              excludeThese = excludeThese ++ ants
              gInState.get.asInstanceOf[CorefTextBoundMention].sieves = gInState.get.asInstanceOf[CorefTextBoundMention].sieves + "pronominalMatch"
            }
          }
        }
        case _ => ()
      }
    mentions
  }

  def includes(subj: CorefMention, obj: CorefMention): Boolean = {
    val stopWords = Set(
      "the",
      "a"
    )

    (obj.words.toSet - stopWords - subj.words.toSet).isEmpty
  }

  def expand(mention: Mention): Interval = {
    val sent = doc.sentences(mention.sentence)
    val graph = sent.dependencies.getOrElse(return mention.tokenInterval)

    val localHead = findHeadStrict(mention.tokenInterval, sent).getOrElse(mention.tokenInterval.end - 1)

    var npHead = localHead

    var searchingHead = true

    // keep traversing incomingEdges until you reach the head of the NP
    while(searchingHead) {
      val newHead = try {
        graph.getIncomingEdges(npHead).find(edge => edge._2 == "nn")
      } catch {
        case e: Exception => None
      }
      if (newHead.isDefined) npHead = newHead.get._1
      else searchingHead = false
    }

    subgraph(Interval(npHead),sent).getOrElse(mention.tokenInterval)
  }

  def isPronominal(mention: Mention): Boolean = detMap.contains(mention.text) || headMap.contains(mention.text)

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
    themeMap.getOrElse(eventLemma,1)
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

  def headCardinality(somenum: String, tag: String): Int = {
    tag match {
      case "PRP" | "PRP$" => headMap.getOrElse(somenum,0)
      case "NNS" | "NNPS" => 2
      case "NN" | "NNP" => 1
      case _ => headMap.getOrElse(somenum,1)
    }
  }

  //displayMentions(orderedMentions, doc)

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
  def sameEntityID(mentions:Seq[Mention]): Boolean = {
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

/**
  if (debug) println("\n=====Match entities by determiner/pronoun anaphor cardinality=====")
  // travel in order through the event mentions in the document, trying to match the right number of arguments to each
  for(mention <- orderedMentions) {
    createByCardinality(mention, Seq("PRP","DT"))
  }

  if (keepAll) {
    results = results :+ (for {
      m <- orderedMentions
    } yield resolve(m)).flatten.sorted
  }

  if (debug) println("\n=====Match entities by noun phrase anaphor cardinality=====")
  // travel in order through the event mentions in the document, trying to match the right number of arguments to each
  for(mention <- orderedMentions) {
    createByCardinality(mention, Seq("NP"))
  }

  if (keepAll) {
    results = results :+ (for {
      m <- orderedMentions
    } yield resolve(m)).flatten.sorted
  }


  def createByCardinality(mention: Mention, labelsToExamine: Seq[String] = Seq("PRP")): Seq[Mention] = {

    if (alreadySplit contains mention) return alreadySplit(mention)

    mention match {

      // bindings need two themes each, but sometimes more than two are mentioned, in which case we
      // need an exhaustive combination of all the bindings from theme1(s) to theme2(s)
      case binding: BioEventMention if binding.matches("Binding") && unresolvedInside(binding) =>

        if (debug) println(s"Checking numerosity of binding BioEventMention '${binding.text}' themes...")

        val firstBindingArgWithThisTrigger = initialMentions
          .filter(m => m matches "Binding")
          .groupBy(m => m.asInstanceOf[BioEventMention].trigger)(binding.trigger)
          .flatMap(m => lookInside(Seq(m)))
          .distinct
          .sorted
          .head

        // this gnarly thing returns BioChemicalEntities that are grounded, aren't already arguments in this event,
        // and aren't controllers of a regulation that has this event as a controlled
        val priors = orderedMentions.slice(0, orderedMentions.indexOf(firstBindingArgWithThisTrigger))
          .filter(x => x.isInstanceOf[BioTextBoundMention] &&
          (x.sentence == binding.sentence || binding.sentence - x.sentence == 1) &&
          !x.matches("Unresolved") &&
          x.matches("BioChemicalEntity") &&
          doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
          !binding.arguments.getOrElse("theme1", Nil).contains(x) &&
          !binding.arguments.getOrElse("theme2", Nil).contains(x) &&
          !chains.keys.exists(y => y.matches("ComplexEvent") &&
            y.arguments.getOrElse("controller", Nil).contains(x) &&
            y.arguments.getOrElse("controlled", Nil).contains(binding)))

        if (debug) {
          print("PRIORS: ")
          for (p <- priors) print(s"${p.text}, ")
          println
        }

        val theme1s: Seq[Mention] = binding.arguments.getOrElse("theme1", Nil)
        val theme2s: Seq[Mention] = binding.arguments.getOrElse("theme2", Nil)

        if (theme1s.isEmpty && theme2s.isEmpty) return Nil

        // keeps track of antecedents found so far, so they won't be reused
        var antecedents: Seq[Mention] = Nil

        // look right to left through previous BioChemicalEntities; start with theme2(s) since we assume these appear
        // later than theme1(s)
        // FIXME: 'them' should prefer closely associated antecedents for plurals, e.g. "Ras and Mek", so we don't make an error on "Even more than Ras and Mek, ASPP2 is common, and so is its binding to them."
        val t2Ants: Seq[Mention] = (for {
          m <- theme2s
        } yield {
            val themes = m match {
              case unres if unres.matches("Unresolved") =>
                // exclude previously used antecedents
                val validPriors = priors.filter(x => !antecedents.contains(x))
                val num = cardinality(unres)

                if (debug) println(s"Theme2 ${m.text} cardinality: $num")

                validPriors match {
                  case noneFound if noneFound.isEmpty => Nil
                  case _ => validPriors.foldRight[(Int, Seq[Mention])]((0, Nil))((a, foundThemes) => {
                    val numToAdd = cardinality(a)

                    foundThemes match {
                      case stillRoom if foundThemes._1 + numToAdd <= num => (foundThemes._1 + numToAdd, foundThemes._2 :+ a)
                      case _ => foundThemes
                    }
                  })._2
                }
              case _ => Seq(m)
            }
            antecedents ++= themes
            themes
          }).flatten

        val t1Ants = (for {
          m <- theme1s
        } yield {
            val themes = m match {
              case unres if unres.matches("Unresolved") =>
                // exclude previously used antecedents
                val validPriors = priors.filter(x => !antecedents.contains(x))
                val num = cardinality(unres)

                if (debug) println(s"Theme2 ${m.text} cardinality: $num")

                validPriors match {
                  case noneFound if noneFound.isEmpty => Nil
                  case _ => validPriors.foldRight[(Int, Seq[Mention])]((0, Nil))((a, foundThemes) => {
                    val numToAdd = cardinality(a)
                    foundThemes match {
                      case stillRoom if foundThemes._1 + numToAdd <= num => (foundThemes._1 + numToAdd, foundThemes._2 :+ a)
                      case _ => foundThemes
                    }
                  }
                  )._2
                }

              case _ => Seq(m)
            }
            antecedents ++= themes
            themes
          }).flatten

        if (debug) {
          print("Theme1 antecedents: ")
          t1Ants.foreach(m => print(s"${m.text},"))
          println
          print("Theme2 antecedents: ")
          t2Ants.foreach(m => print(s"${m.text},"))
          println
        }

        // Basically the same as mkBinding in DarpaActions
        val newBindings: Seq[Mention] = (t1Ants, t2Ants) match {
          case (t1s, t2s) if t1Ants.isEmpty && t2Ants.isEmpty => Nil
          case (t1s, t2s) if theme1s.isEmpty || theme2s.isEmpty =>
            val mergedThemes = (t1s ++ t2s)
            (for {pair <- mergedThemes.combinations(2)} yield {
              val splitBinding = new BioEventMention(
                binding.labels, binding.trigger, Map("theme" -> Seq(pair.head, pair.last)), binding.sentence, doc, binding.keep, binding.foundBy + ",corefCardinality"
              )
              splitBinding.modifications ++= binding.asInstanceOf[BioEventMention].modifications
              splitBinding.mutableTokenInterval = binding.tokenInterval
              splitBinding
            }).toSeq
          case _ => {
            for {
              theme1 <- t1Ants
              theme2 <- t2Ants
              if !sameEntityID(Seq(theme1, theme2))
            } yield {
              if (theme1.text.toLowerCase == "ubiquitin") {
                val args = Map("theme" -> Seq(theme2))
                val ubiq = new BioEventMention(
                  "Ubiquitination" +: binding.labels.filter(_ != "Binding"), binding.trigger, args, binding.sentence, doc, binding.keep, binding.foundBy + ",corefCardinality")
                ubiq.modifications ++= mention.asInstanceOf[BioEventMention].modifications
                ubiq.mutableTokenInterval = mention.tokenInterval
                ubiq
              } else if (theme2.text.toLowerCase == "ubiquitin") {
                val args = Map("theme" -> Seq(theme1))
                val ubiq = new BioEventMention(
                  "Ubiquitination" +: binding.labels.filter(_ != "Binding"), binding.trigger, args, binding.sentence, doc, binding.keep, binding.foundBy + ",corefCardinality")
                ubiq.modifications ++= mention.asInstanceOf[BioEventMention].modifications
                ubiq.mutableTokenInterval = mention.tokenInterval
                ubiq
              }
              else {
                val args = Map("theme" -> Seq(theme1, theme2))
                val splitBinding = new BioEventMention(
                  binding.labels, binding.trigger, args, binding.sentence, doc, binding.keep, binding.foundBy + ",corefCardinalityBinding")
                splitBinding.modifications ++= binding.asInstanceOf[BioEventMention].modifications
                splitBinding.mutableTokenInterval = binding.tokenInterval
                splitBinding
              }
            }
          }
        }

        if (debug) println(s"Number of new bindings: ${newBindings.toSeq.length}")

        val distinctBindings = newBindings.filter(x => !chains.keys.exists(y => y.arguments == x.arguments &&
          y.labels == x.labels &&
          y.asInstanceOf[EventMention].trigger == x.asInstanceOf[EventMention].trigger))

        if (newBindings.isEmpty && chains.contains(binding)) {
          orderedMentions -= binding
          chains(binding).foreach(link => chains(link) = chains(link).filter(_ != binding))
          chains -= binding
        } else if (!distinctBindings.isEmpty) {
          // replace current binding with new bindings in the ordered mentions and in the chain map
          orderedMentions = (orderedMentions - binding ++ distinctBindings).sorted
          val newChain = (chains(binding).filter(_ != binding) ++ distinctBindings).sorted
          for (link <- chains(binding)) {
            chains(link) = newChain
          }
          for (link <- distinctBindings) {
            chains(link) = newChain
          }
          if (chains contains binding) chains -= binding
        }

        alreadySplit += (binding -> distinctBindings)

        distinctBindings

      case reg: BioEventMention if reg.matches("ComplexEvent") && unresolvedInside(reg) =>

        // Non-cause regulations (that is, regulations with a trigger) with multiple antecedents are invalid
        //if (reg.isInstanceOf[BioEventMention] &&
        //  reg.arguments.flatMap(_._2).filter(m => m.matches("Unresolved")).map(m => cardinality(m)).sum > 1) return Nil

        if (debug) println(s"Checking numerosity of ComplexEvent '${reg.text}' arguments...")

        val firstRegArgWithThisTrigger = initialMentions
          .filter(m => m matches "ComplexEvent")
          .groupBy(m => m.asInstanceOf[BioEventMention].trigger)(reg.trigger)
          .flatMap(m => lookInside(Seq(m)))
          .distinct
          .sorted
          .head

        var antecedents: Seq[Mention] = Nil

        val foundControlleds = reg.arguments.getOrElse("controlled", Nil) match {
          case unres if unres.exists(m => m matches "Unresolved") =>

            // this gnarly thing ensures that we only look at possible controllers that don't themselves control this
            // regulation in this or the prior sentence
            val priors =
              orderedMentions.slice(0, orderedMentions.indexOf(firstRegArgWithThisTrigger)) filter (x => !x.matches("Unresolved") &&
                (reg.sentence == x.sentence || (reg.sentence - x.sentence == 1)) &&
                doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
                !antecedents.contains(x) &&
                x.asInstanceOf[BioMention].matches("PossibleControlled") &&
                !reg.arguments.getOrElse("controller", Nil).contains(x))

            if (debug) println(s"Controlled priors: $priors")

            if (priors.isEmpty) {
              Nil
            } else {
              val ants = (for {
                p <- priors.takeRight(math.min(cardinality(unres.head), priors.length))
              } yield createByCardinality(p)).flatten
              antecedents ++= ants
              ants
            }
          case res => (for (m <- res) yield createByCardinality(m)).flatten
        }

        val foundControllers = reg.arguments.getOrElse("controller", Nil) match {
          case unres if unres.exists(m => m matches "Unresolved") =>
            // this gnarly thing ensures that we only look at possible controllers that don't themselves control this
            // regulation in this or the prior sentence
            val priors =
              orderedMentions.slice(0, orderedMentions.indexOf(firstRegArgWithThisTrigger)) filter (x => !x.matches("Unresolved") &&
                (reg.sentence == x.sentence || (reg.sentence - x.sentence == 1)) &&
                doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
                !antecedents.contains(x) &&
                x.asInstanceOf[BioMention].matches("PossibleController") &&
                !reg.arguments.getOrElse("controlled", Nil).contains(x))

            if (debug) println(s"Controller priors: $priors")

            if (priors.isEmpty) Nil
            else {
              val ants = (for {
                p <- priors.takeRight(math.min(cardinality(unres.head), priors.length))
              } yield createByCardinality(p)).flatten
              antecedents ++= ants
              ants
            }

          case res => (for (m <- res) yield createByCardinality(m)).flatten
        }

        if (debug) {
          println(s"Number of controllers: ${foundControllers.length}")
          println(s"Number of controlleds: ${foundControlleds.length}")
        }

        val newRegs = foundControllers.length match {
          case some if some > 0 =>
            for {
              d <- foundControlleds
            } yield {
              val newReg = reg match {
                case reg: BioEventMention =>
                  val ev = new BioEventMention(reg.labels, reg.asInstanceOf[BioEventMention].trigger,
                    Map("controlled" -> Seq(d)), reg.sentence, doc, reg.keep, reg.foundBy + ",corefCardinality")
                  ev.modifications ++= reg.modifications
                  ev.mutableTokenInterval = reg.tokenInterval
                  ev
              }
              newReg
            }
          case _ =>
            for {
              r <- foundControllers
              d <- foundControlleds
              if r != d
            } yield {
              val newReg = reg match {
                case reg: BioEventMention =>
                  val ev = new BioEventMention(reg.labels, reg.asInstanceOf[BioEventMention].trigger,
                    Map("controller" -> Seq(r), "controlled" -> Seq(d)), reg.sentence, doc, reg.keep, reg.foundBy + ",corefCardinalityReg")
                  ev.modifications ++= reg.modifications
                  ev.mutableTokenInterval = reg.tokenInterval
                  ev
              }
              newReg
            }
        }

        if (debug) println(s"Length of newRegs: ${newRegs.length}")

        if (newRegs.isEmpty && chains.contains(reg)) {
          orderedMentions -= reg
          chains(reg).foreach(link => chains(link) = chains(link).filter(_ != reg))
          chains -= reg
        } else if (!newRegs.forall(m => chains.contains(m))) {
          // replace current reg with new regs in the ordered mentions and in the chain map
          orderedMentions = (orderedMentions - reg ++ newRegs).sorted
          val newChain = (if (chains contains reg) {
            val chain = (chains(reg).filter(_ != reg) ++ newRegs).sorted
            chains -= reg
            chain
          } else {
            newRegs
          }).sorted
          for (link <- newChain) {
            chains(link) = newChain
          }
        }

        alreadySplit += (reg -> newRegs)

        newRegs

      case ev: BioEventMention if (ev.arguments.getOrElse("cause", Nil) ++ ev.arguments.getOrElse("theme", Nil))
        .exists(arg => arg matches "Unresolved") =>

        if (debug) println(s"Checking numerosity of non-binding BioEventMention '${ev.text}' themes and causes...")

        val firstEvArgWithThisTrigger = initialMentions
          .filter(m => m matches ev.label)
          .groupBy(m => m.asInstanceOf[BioEventMention].trigger)(ev.trigger)
          .flatMap(m => lookInside(Seq(m)))
          .distinct
          .sorted
          .head

        if (debug) println(s"First event argument with this trigger: ${displayMention(firstEvArgWithThisTrigger)}")

        var antecedents: Seq[Mention] = Nil
        val foundThemes = (for {
          m <- ev.arguments("theme").filter(thm => thm.matches("Unresolved"))
          priors = (orderedMentions.slice(0, orderedMentions.indexOf(firstEvArgWithThisTrigger))
            filter (x => x.isInstanceOf[BioTextBoundMention] &&
            (x.sentence == m.sentence || m.sentence - x.sentence == 1) &&
            !x.matches("Unresolved") &&
            x.matches(m.labels(1)) &&
            doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
            !antecedents.contains(x) &&
            !chains.keys.exists(y => y.matches("ComplexEvent") &&
              y.arguments.getOrElse("controller", Nil).contains(x) &&
              y.arguments.getOrElse("controlled", Nil).contains(ev)))).distinct

          brk1 = breakable {
            if (priors.isEmpty) break()
          }

          num = cardinality(m)

          themes = priors.takeRight(math.min(num, priors.length))
          if themes.combinations(2).forall(pair => !sameEntityID(pair))
        } yield {
            antecedents ++= themes
            themes
          }).flatten ++ ev.arguments("theme").filter(thm => !thm.matches("Unresolved"))

        val foundCauses = (for {
          m <- ev.arguments.getOrElse("cause", Nil).filter(cau => cau.matches("Unresolved"))
          priors = (orderedMentions.slice(0, orderedMentions.indexOf(firstEvArgWithThisTrigger))
            filter (x => x.isInstanceOf[BioTextBoundMention] &&
            (x.sentence == m.sentence || m.sentence - x.sentence == 1) &&
            !x.matches("Unresolved") &&
            x.matches(m.labels(1)) &&
            doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
            !antecedents.contains(x) &&
            !chains.keys.exists(y => y.matches("ComplexEvent") &&
              y.arguments.getOrElse("controller", Nil).contains(x) &&
              y.arguments.getOrElse("controlled", Nil).contains(ev)))).distinct

          brk1 = breakable {
            if (priors.isEmpty) break()
          }

          num = cardinality(m)

          causes = priors.takeRight(math.min(num, priors.length))
          if causes.combinations(2).forall(pair => !sameEntityID(pair))
        } yield {
            antecedents ++= causes
            causes
          }).flatten ++ ev.arguments.getOrElse("cause",Nil).filter(cau => !cau.matches("Unresolved"))

        val newEvs: Seq[BioEventMention] = (for {
          t <- foundThemes
          c <- if (foundCauses.nonEmpty) foundCauses else Seq(ev.trigger)
        } yield {
            c match {
              case ev.trigger =>
                val newEv =
                  new BioEventMention(
                    ev.labels,
                    ev.trigger,
                    ev.arguments - "theme" + ("theme" -> Seq(t)),
                    ev.sentence,
                    doc,
                    ev.keep,
                    ev.foundBy + ",corefCardinalitySimpleEvent")
                newEv.modifications ++= ev.modifications
                newEv.mutableTokenInterval = ev.mutableTokenInterval
                newEv
              case _ =>
                val newEv =
                  new BioEventMention(
                    ev.labels,
                    ev.trigger,
                    ((ev.arguments - "theme") - "cause") ++ Map("theme" -> Seq(t), "cause" -> Seq(c)),
                    ev.sentence,
                    doc,
                    ev.keep,
                    ev.foundBy + ",corefCardinalitySimpleEvent")
                newEv.modifications ++= ev.modifications
                newEv.mutableTokenInterval = ev.mutableTokenInterval
                newEv
            }

          }).distinct

        if (newEvs.isEmpty && chains.contains(ev)) {
          orderedMentions -= ev
          chains(ev).foreach(link => chains(link) = chains(link).filter(_ != ev))
          chains -= ev
        } else if (!newEvs.forall(m => chains.contains(m))) {
          orderedMentions = (orderedMentions - ev ++ newEvs).sorted
          val sumChain = (if (chains contains ev) {
            val chain = (chains(ev).filter(x => !(x == ev)) ++ newEvs).sorted
            chains -= ev
            chain
          } else {
            newEvs
          }).sorted
          sumChain.foreach(link => chains(link) = sumChain)
        }

        alreadySplit += (ev -> newEvs)

        newEvs


      case m => Seq(m)
    }

  }

  /*

      val classMap = Map(
        "protein" -> "Gene_or_gene_product",
        "gene" -> "Gene_or_gene_product",
        "cistron" -> "Gene_or_gene_product",
        "sequence" -> "Gene_or_gene_product",
        "exon" -> "Gene_or_gene_product",
        "intron" -> "Gene_or_gene_product",
        "mutant" -> "Gene_or_gene_product",
        "cytokine" -> "Gene_or_gene_product",
        "smad" -> "Gene_or_gene_product"
      )

      if (debug) println("\n=====Match unresolved entities to prior resolved entity of same class=====")
      for (mention <- orderedMentions) {

      }
  */

  if (debug) println("\n=====Match unresolved events to closest of the same kind=====")
  // event anaphora match closest previous event of same type
  for((m,i) <- orderedMentions.zipWithIndex) {
    if(m.isInstanceOf[BioEventMention] & m.matches("Unresolved")) {
      val priorRx = orderedMentions.slice(0,i) filter (x => x.isInstanceOf[BioEventMention] && !x.matches("Unresolved") && x.matches(m.labels(1)))
      if(priorRx.nonEmpty) {
        val sumChain = (chains(priorRx.last) ++ chains(m)).distinct.sorted
        if (debug) println(s"${m.text} matched " + sumChain.map(_.text).mkString(","))
        chains(priorRx.last).foreach(link => chains(link) = sumChain)
        chains(m).foreach(link => chains(link) = sumChain)
      }
    }
  }

  if (debug) println
*/
}