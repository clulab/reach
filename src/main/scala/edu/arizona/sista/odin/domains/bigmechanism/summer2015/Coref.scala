package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.bionlp.display._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
import scala.collection.mutable.ListBuffer
import util.control.Breaks._
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.DependencyUtils._
import scala.collection.mutable

class Coref extends DarpaFlow {

  val debug: Boolean = true

  def apply(mentions: Seq[Mention], state: State): Seq[BioMention] = applyAll(mentions).lastOption.getOrElse(Seq())

  def applyAll(mentions: Seq[Mention], keepAll: Boolean = false): Seq[Seq[BioMention]] = {

    val doc: Document = mentions.headOption.getOrElse(return Seq()).document

    if (debug) {
      println("BEFORE COREF")
      displayMentions(mentions,doc)
    }

    val chains = new mutable.HashMap[Mention,Seq[Mention]]

    val toSplit = new mutable.HashMap[Mention,Seq[Seq[Mention]]]

    val alreadySplit = new mutable.HashMap[Mention,Seq[Mention]]

    def lookInside (m: Mention): Seq[Mention] = {
      (for {
        (k, v) <- m.arguments
        a <- v
        n = a match {
          case found if mentions.contains(a) => lookInside(a)
          case _ => a +: lookInside(a)
        }
      } yield n).flatten.toSeq
    }

    def unresolvedInside (m: Mention): Boolean = {
      if (m matches "Unresolved") true
      else if (m.arguments.isEmpty) false
      else m.arguments.flatMap(x => x._2).exists(x => unresolvedInside(x))
    }

    val themeMap = Map(
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

    def getChildren (m: Mention): Seq[Mention] = {
      def getInnerChildren(m: Mention): Seq[Mention] = {
        m match {
          case t: BioTextBoundMention => Seq(t)
          case e: BioEventMention =>
            e +: (for {
              (k, v) <- e.arguments
              a <- v
            } yield {
                a match {
                  case en: BioTextBoundMention => Seq(en)
                  case ev: BioEventMention => ev +: getInnerChildren(ev)
                  case rm: BioRelationMention => rm +: getInnerChildren(rm)
                }
              }).flatten.toSeq
          case r: BioRelationMention =>
            r +: (for {
              (k, v) <- r.arguments
              a <- v
            } yield {
                a match {
                  case en: BioTextBoundMention => Seq(en)
                  case ev: BioEventMention => ev +: getInnerChildren(ev)
                  case rm: BioRelationMention => rm +: getInnerChildren(rm)
                }
              }).flatten.toSeq
        }
      }

      m match {
        case ut: BioTextBoundMention if ut.matches("Unresolved") => Seq()
        case t: BioTextBoundMention if !t.matches("Unresolved") => Seq(t)
        case e: BioEventMention =>
          e +: (for {
            (k, v) <- e.arguments
            a <- v
          } yield {
              a match {
                case en: BioTextBoundMention => Seq(en)
                case ev: BioEventMention => ev +: getInnerChildren(ev)
                case rm: BioRelationMention => rm +: getInnerChildren(rm)
              }
            }).flatten.toSeq
        case r: BioRelationMention =>
          r +: (for {
            (k, v) <- r.arguments
            a <- v
          } yield {
              a match {
                case en: BioTextBoundMention => Seq(en)
                case ev: BioEventMention => ev +: getInnerChildren(ev)
                case rm: BioRelationMention => rm +: getInnerChildren(rm)
              }
            }).flatten.toSeq
      }
    }

    def resolve(mention: Mention): Seq[Mention] = {
      mention match {
        case mention: BioTextBoundMention if !mention.matches("Unresolved") => Seq(mention)

        case mention: BioTextBoundMention if mention.matches("Unresolved") => {
          if (debug) print(s"Resolving unresolved BioTextBoundMention ${mention.text} => ")

          val resolved = chains.getOrElse(mention, {
            println("Mention not used in coreference: " + mention.label + ": " + mention.text)
            Seq(mention)
          })
            .filter(x => !x.matches("Unresolved") &&
            (x.sentence == mention.sentence || mention.sentence - x.sentence == 1) &&
            doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval,doc.sentences(x.sentence)).get).drop(1) == "N"
            )
            .span(m => m.precedes(mention))._1.lastOption.getOrElse({if (debug) println; return Seq()})
          // resolved.asInstanceOf[BioTextBoundMention].modifications ++= mention.asInstanceOf[BioTextBoundMention].modifications

          if (debug) println(resolved.text)

          Seq(resolved)
        }

        case mention: BioRelationMention => {
          if (debug) print(s"Resolving BioRelationMention ${mention.text} => ")

          val args = (for {
            (argType, argMentions) <- mention.arguments
          } yield argType -> argMentions.map(a => resolve(a)).flatten).filter(_._2.nonEmpty)
          args match {
            case regMissingArg if (mention.matches("ComplexEvent") ||
              mention.matches("Negative_regulation") ||
              mention.matches("Positive_activation") ||
              mention.matches("Negative_activation")) &&
              args.size < 2 =>
              if (debug) println
              Seq()

            case stillUnresolved if args.size < 1 =>
              if (debug) println
              Seq()

            case _ =>
              val resolved = new BioRelationMention(mention.labels, args, mention.sentence, doc, mention.keep, mention.foundBy)
              resolved.modifications ++= mention.asInstanceOf[BioRelationMention].modifications
              resolved.mutableTokenInterval = mention.tokenInterval
              if (debug) println(resolved.text)
              Seq(resolved)
          }
        }

        case mention: BioEventMention if mention.matches("Unresolved") => {
          if (debug) print(s"Resolving unresolved BioEventMention ${mention.text} => ")

          val resolved = chains.getOrElse(mention, {
            println("Mention not used in coreference: " + mention.label + ": " + mention.text)
            Seq(mention)
          })
            .filter(x => !x.matches("Unresolved") && (x.sentence == mention.sentence || mention.sentence - x.sentence == 1))
            .span(m => m.precedes(mention))._1.lastOption.getOrElse({if (debug) println; return Seq()})
          resolved.asInstanceOf[BioEventMention].modifications ++= mention.asInstanceOf[BioEventMention].modifications
          resolved.asInstanceOf[BioEventMention].mutableTokenInterval = mention.tokenInterval
          if (debug) println(resolved.text)
          Seq(resolved)
        }

        case mention: BioEventMention if !mention.matches("Unresolved") & toSplit.contains(mention) =>

          if (debug) print(s"Resolving splittable BioEventMention ${mention.text} => ")

            val themeSets = combination(toSplit(mention), themeCardinality(mention.label))
            //println("Number of sets: " + themeSets.length)
            //themeSets.foreach(s => println(s"(${for (m<-s) yield m.text + ","})"))

            for (themeSet <- themeSets) yield {
              val resolved = new BioEventMention(mention.labels,
              mention.trigger,
              mention.arguments - "theme" + ("theme" -> themeSet),
              mention.sentence,
              doc,
              mention.keep,
              "corefResolveSplitter")
              resolved.modifications ++= mention.asInstanceOf[BioEventMention].modifications
              resolved.mutableTokenInterval = mention.tokenInterval
              if (debug) println(resolved.text)

              resolved
            }

        case mention: BioEventMention if !mention.matches("Unresolved") & !toSplit.contains(mention) =>

          if (debug) print(s"Resolving non-splitting BioEventMention ${mention.text} => ")

            val args = (for {
              (argType, argMentions) <- mention.arguments
            } yield argType -> argMentions.map(a => resolve(a)).flatten.distinct).filter(_._2.nonEmpty)

            args match {
              case regMissingArg if (mention.matches("ComplexEvent") || mention.matches("ActivationEvent")) &&
                args.size < 2 =>
                if (debug) println
                Seq()
              case bindingMissingTheme if mention.matches("Binding") && mention.arguments.getOrElse("theme",Seq()).length < 2 =>
                if (debug) println
                Seq()
              case stillUnresolved if args.size < 1 =>
                if (debug) println
                Seq()
              case _ =>
                val resolved = new BioEventMention(mention.labels, mention.trigger, args, mention.sentence, mention.document, mention.keep, mention.foundBy)
                resolved.modifications ++= mention.asInstanceOf[BioEventMention].modifications
                resolved.mutableTokenInterval = mention.tokenInterval
                if (debug) println(resolved.text)
                Seq(resolved)
            }

        case m => Seq(m)
      }
    }

    // http://oldfashionedsoftware.com/2009/07/30/lots-and-lots-of-foldleft-examples/
    def group[A](list: List[A], size: Int): List[List[A]] =
      list.foldLeft( (List[List[A]](),0) ) { (r,c) => r match {
        case (head :: tail, num) =>
          if (num < size)  ( (c :: head) :: tail , num + 1 )
          else             ( List(c) :: head :: tail , 1 )
        case (Nil, num) => (List(List(c)), 1)
      }
      }._1.foldLeft(List[List[A]]())( (r,c) => c.reverse :: r)

    def combination[A](ms: Seq[Seq[A]], size: Int): Seq[Seq[A]] = {
      require(size > 0)
      if (ms.flatten.length <= size) return Seq(ms.flatten)

      ms.foldLeft[Seq[Seq[A]]](Seq())((args,thm) => args ++ (for {
        ant <- thm
        nxt <- if (size - 1 > 0) group(ms.span(ms.indexOf(_) <= ms.indexOf(thm))._2.flatten.toList,size-1).toSeq else Seq(Seq())
      } yield Seq(Seq(ant), nxt.toSeq).flatten))
    }

    def mapMerger[A,B](maps: Seq[Map[A,Seq[B]]]): Map[A,Seq[B]] = {
      val concat = new mutable.HashMap[A,Seq[B]]
      for {
        m <- maps
        (k,v) <- m
      } concat.update(k,concat.getOrElse(k,Seq()) ++ v)
      concat.toMap
    }

    def mentionMerger(mentions: Seq[Mention]): Seq[Mention] = {
      val evs = mentions collect {case ev: EventMention => ev.asInstanceOf[EventMention]}
      val mergedMentions = for {
        (rxn,rxnMentions) <- evs.groupBy(_.labels)
        (trig,trigMentions) <- rxnMentions.groupBy(_.trigger)
      } yield {
          trigMentions match {
            case singleton if singleton.length == 1 => singleton.head
            case _ =>
              val base = trigMentions.head
              val newMap = mapMerger[String,Mention](trigMentions.map(_.arguments))
              new BioEventMention(
              trigMentions.flatMap(_.labels).distinct,base.trigger,mapMerger(trigMentions.map(_.arguments)),base.sentence,doc,base.keep,"argMerger")
          }
        }
      mentions diff evs ++ mergedMentions
    }

    val hiddenMentions = (for {
      m <- mentions
    } yield lookInside(m)).flatten

    var orderedMentions: ListBuffer[Mention] = (for {
      m <- (mentions ++ hiddenMentions).distinct
    } yield getChildren(m)).flatten.distinct.sorted.to[ListBuffer]

    orderedMentions.foreach(m => chains += m -> Seq(m))

    var results: Seq[Seq[Mention]] = Seq()

    // exact string matching
    val sameText = chains.keys
      .filter(x => x.isInstanceOf[BioTextBoundMention] && !x.matches("Unresolved"))
      .groupBy (m => m.text.toLowerCase)
      .filter(_._2.toSeq.length > 1)
    sameText.foreach {
      case (ent, ms) =>
        val newChain = ms.flatMap(m => chains(m)).toSeq.distinct.sorted
        sameText(ent).foreach(link => chains(link) = newChain)
    }

    if (keepAll) {
      results = results :+ (for {
        m <- orderedMentions
      } yield resolve(m)).flatten.sorted
    }

    // exact grounding
    val sameGrounding = chains.keys
      .filter(x => x.isInstanceOf[BioTextBoundMention])
      .filter(x => x.asInstanceOf[BioTextBoundMention].isGrounded)
      .groupBy(m => m.asInstanceOf[BioTextBoundMention].xref.get.id)
    sameGrounding.foreach {
      case (gr, ms) =>
        val newChain = ms.flatMap(m => chains(m)).toSeq.distinct.sorted
        sameGrounding(gr).foreach(link => chains(link) = newChain)
    }

    if (keepAll) {
      results = results :+ (for {
        m <- orderedMentions
      } yield resolve(m)).flatten.sorted
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
     * @param mentions A Sequence of odin-style mentions
     * @return boolean
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


    // travel in order through the event mentions in the document, trying to match the right number of arguments to each
    for(mention <- orderedMentions) {
      createByCardinality(mention)
    }

    def createByCardinality(mention: Mention): Seq[Mention] = {

      if (alreadySplit contains mention) return alreadySplit(mention)

      mention match {

        // bindings need two themes each, but sometimes more than two are mentioned, in which case we
        // need an exhaustive combination of all the bindings from theme1(s) to theme2(s)
        case binding: BioEventMention if binding.matches("Binding") && unresolvedInside(binding) =>

          if (debug) println(s"Checking numerosity of binding BioEventMention '${binding.text}' themes...")

          // this gnarly thing returns BioChemicalEntities that are grounded, aren't already arguments in this event,
          // and aren't controllers of a regulation that has this event as a controlled
          val priors = orderedMentions.slice(0, orderedMentions.indexOf(binding.arguments.flatMap(_._2).toSeq.sorted.lastOption.getOrElse(binding))) filter (x => x.isInstanceOf[BioTextBoundMention] &&
            (x.sentence == binding.sentence || binding.sentence - x.sentence == 1) &&
            !x.matches("Unresolved") &&
            x.matches("BioChemicalEntity") &&
            doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
            !binding.arguments.getOrElse("theme1", Seq()).contains(x) &&
            !binding.arguments.getOrElse("theme2", Seq()).contains(x) &&
            !chains.keys.exists(y => y.matches("ComplexEvent") &&
              y.arguments.getOrElse("controller", Seq()).contains(x) &&
              y.arguments.getOrElse("controlled", Seq()).contains(binding)))

          if (debug) {
            print("PRIORS: ")
            for (p <- priors) print(s"${p.text}, ")
            println
          }

          val theme1s: Seq[Mention] = binding.arguments.getOrElse("theme1", Seq())
          val theme2s: Seq[Mention] = binding.arguments.getOrElse("theme2", Seq())

          if (theme1s.isEmpty && theme2s.isEmpty) return Seq()

          // keeps track of antecedents found so far, so they won't be reused
          var antecedents: Seq[Mention] = Seq()

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
                    case _ => validPriors.foldRight[(Int, Seq[Mention])]((0, Seq()))((a, foundThemes) => {
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
                    case _ => validPriors.foldRight[(Int, Seq[Mention])]((0, Seq()))((a, foundThemes) => {
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
            case (t1s, t2s) if t1Ants.isEmpty && t2Ants.isEmpty => Seq()
            case (t1s, t2s) if theme1s.isEmpty || theme2s.isEmpty =>
              val mergedThemes = (t1s ++ t2s)
              (for {pair <- mergedThemes.combinations(2)} yield {
                val splitBinding = new BioEventMention(
                  binding.labels, binding.trigger, Map("theme" -> Seq(pair.head, pair.last)), binding.sentence, doc, binding.keep, "corefCardinality"
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
                    "Ubiquitination" +: binding.labels.filter(_ != "Binding"), binding.trigger, args, binding.sentence, doc, binding.keep, "corefCardinality")
                  ubiq.modifications ++= mention.asInstanceOf[BioEventMention].modifications
                  ubiq.mutableTokenInterval = mention.tokenInterval
                  ubiq
                } else if (theme2.text.toLowerCase == "ubiquitin") {
                  val args = Map("theme" -> Seq(theme1))
                  val ubiq = new BioEventMention(
                    "Ubiquitination" +: binding.labels.filter(_ != "Binding"), binding.trigger, args, binding.sentence, doc, binding.keep, "corefCardinality")
                  ubiq.modifications ++= mention.asInstanceOf[BioEventMention].modifications
                  ubiq.mutableTokenInterval = mention.tokenInterval
                  ubiq
                }
                else {
                  val args = Map("theme" -> Seq(theme1, theme2))
                  val splitBinding = new BioEventMention(
                    binding.labels, binding.trigger, args, binding.sentence, doc, binding.keep, "corefCardinality")
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


        case reg if reg.matches("ComplexEvent") && unresolvedInside(reg) =>

          // Non-cause regulations (that is, regulations with a trigger) with multiple antecedents are invalid
          if (reg.isInstanceOf[BioEventMention] &&
            reg.arguments.flatMap(_._2).filter(m => m.matches("Unresolved")).map(m => cardinality(m)).sum > 1) return Seq()

          if (debug) println(s"Checking numerosity of ComplexEvent '${reg.text}' arguments...")

          var antecedents: Seq[Mention] = Seq()

          val foundControlleds = reg.arguments.getOrElse("controlled", Seq()) match {
            case unres if unres.exists(m => m matches "Unresolved") =>

              // this gnarly thing ensures that we only look at possible controllers that don't themselves control this
              // regulation in this or the prior sentence
              val priors = orderedMentions.slice(0, orderedMentions.indexOf(unres.sorted.head)) filter (x => !x.matches("Unresolved") &&
                (reg.sentence == x.sentence || (reg.sentence - x.sentence == 1)) &&
                doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
                !antecedents.contains(x) &&
                x.asInstanceOf[BioMention].matches("SimpleEvent") &&
                !reg.arguments.getOrElse("controller", Seq()).contains(x))

              if (debug) println(s"Controlled priors: $priors")

              if (priors.isEmpty) {
                Seq()
              } else {
                val ants = (for {
                  p <- priors.takeRight(math.min(cardinality(unres.head), priors.length))
                } yield createByCardinality(p)).flatten
                antecedents ++= ants
                ants
              }
            case res => (for (m <- res) yield createByCardinality(m)).flatten
          }

          val foundControllers = reg.arguments.getOrElse("controller", Seq()) match {
            case unres if unres.exists(m => m matches "Unresolved") =>
              // this gnarly thing ensures that we only look at possible controllers that don't themselves control this
              // regulation in this or the prior sentence
              val priors = orderedMentions.slice(0, orderedMentions.indexOf(unres.sorted.head)) filter (x => !x.matches("Unresolved") &&
                (reg.sentence == x.sentence || (reg.sentence - x.sentence == 1)) &&
                doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
                !antecedents.contains(x) &&
                x.asInstanceOf[BioMention].matches("PossibleController") &&
                !reg.arguments.getOrElse("controlled", Seq()).contains(x))

              if (debug) println(s"Controller priors: $priors")

              if (priors.isEmpty) {
                Seq()
              } else {
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
            println(s"Number of controllers: ${foundControlleds.length}")
          }

          val newRegs = for {
            r <- foundControllers
            d <- foundControlleds
            if r != d
          } yield {
              val newReg = reg match {
                case reg: BioEventMention =>
                  val ev = new BioEventMention(reg.labels, reg.asInstanceOf[BioEventMention].trigger,
                    Map("controller" -> Seq(r), "controlled" -> Seq(d)), reg.sentence, doc, reg.keep, "corefCardinality")
                  ev.modifications ++= reg.modifications
                  ev.mutableTokenInterval = reg.tokenInterval
                  ev
                case _ =>
                  val rel = new BioRelationMention(reg.labels, Map("controller" -> Seq(r), "controlled" -> Seq(d)),
                    reg.sentence, doc, reg.keep, "corefCardinality")
                  rel.modifications ++= reg.asInstanceOf[BioMention].modifications
                  rel.mutableTokenInterval = reg.tokenInterval
                  rel
              }
              newReg
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


        case act if act.matches("ActivationEvent") &&
          act.arguments.exists(kv => kv._2.exists(m => m matches "Unresolved")) =>

          // Activations with multiple antecedents are invalid
          if (act.arguments.flatMap(_._2).filter(m => m.matches("Unresolved")).map(m => cardinality(m)).sum > 1) return Seq()

          if (debug) println(s"Checking numerosity of ComplexEvent '${act.text}' arguments...")

          var antecedents: Seq[Mention] = Seq()

          val foundControlleds = act.arguments.getOrElse("controlled", Seq()) match {
            case unres if unres.exists(m => m matches "Unresolved") =>

              // this gnarly thing ensures that we only look at possible controllers that don't themselves control this
              // regulation in this or the prior sentence
              val priors = orderedMentions.slice(0, orderedMentions.indexOf(unres.sorted.head)) filter (x => !x.matches("Unresolved") &&
                (act.sentence == x.sentence || (act.sentence - x.sentence == 1)) &&
                doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
                !antecedents.contains(x) &&
                x.asInstanceOf[BioMention].matches("BioChemicalEntity") &&
                !act.arguments.getOrElse("controller", Seq()).contains(x))

              if (debug) println(s"Controlled priors: $priors")

              if (priors.isEmpty) {
                Seq()
              } else {
                val ants = priors.takeRight(math.min(cardinality(unres.head), priors.length))
                antecedents ++= ants
                ants
              }

            case x => x
          }

          val foundControllers = act.arguments.getOrElse("controller", Seq()) match {
            case unres if unres.exists(m => m matches "Unresolved") =>
              // this gnarly thing ensures that we only look at possible controllers that don't themselves control this
              // regulation in this or the prior sentence
              val priors = orderedMentions.slice(0, orderedMentions.indexOf(unres.sorted.head)) filter (x => !x.matches("Unresolved") &&
                (act.sentence == x.sentence || (act.sentence - x.sentence == 1)) &&
                doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
                !antecedents.contains(x) &&
                x.asInstanceOf[BioMention].matches("BioChemicalEntity") &&
                !act.arguments.getOrElse("controlled", Seq()).contains(x))

              if (debug) println(s"Controller priors: $priors")

              if (priors.isEmpty) {
                Seq()
              } else {
                val ants = priors.takeRight(math.min(cardinality(unres.head), priors.length))
                antecedents ++= ants
                ants
              }
            case x => x
          }

          if (debug) {
            println(s"Number of controllers: ${foundControllers.length}")
            println(s"Number of controlleds: ${foundControlleds.length}")
          }

          val newActs = for {
            r <- foundControllers
            d <- foundControlleds
            if !sameEntityID(Seq(r,d))
          } yield {
              val newAct = act match {
                case event: BioEventMention =>
                  val ev = new BioEventMention(event.labels, event.asInstanceOf[BioEventMention].trigger,
                    Map("controller" -> Seq(r), "controlled" -> Seq(d)), event.sentence, doc, event.keep, "corefCardinality")
                  ev.modifications ++= event.modifications
                  ev.mutableTokenInterval = event.tokenInterval
                  ev
                case _ =>
                  val rel = new BioRelationMention(act.labels, Map("controller" -> Seq(r), "controlled" -> Seq(d)),
                    act.sentence, doc, act.keep, "corefCardinality")
                  rel.modifications ++= act.asInstanceOf[BioMention].modifications
                  rel.mutableTokenInterval = act.tokenInterval
                  rel
              }
              newAct
            }

          if (debug) println(s"Length of newRegs: ${newActs.length}")

          if (newActs.isEmpty && chains.contains(act)) {
            orderedMentions -= act
            chains(act).foreach(link => chains(link) = chains(link).filter(_ != act))
            chains -= act
          } else if (!newActs.forall(m => chains.contains(m))) {
            // replace current reg with new regs in the ordered mentions and in the chain map
            orderedMentions = (orderedMentions - act ++ newActs).sorted
            for (link <- chains(act)) {
              chains(link) = chains(link).filter(_ != act) ++ newActs
            }
            if (chains contains act) chains -= act
          }

          alreadySplit += (act -> newActs)

          newActs


        case ev: BioEventMention if !ev.matches("Binding") & !ev.matches("ComplexEvent") &
          ev.arguments.getOrElse("theme", Seq()).exists(thm => thm.matches("Unresolved")) =>

          if (debug) println(s"Checking numerosity of non-binding BioEventMention '${ev.text}' themes...")

          var antecedents: Seq[Mention] = Seq()
          val foundThemes = for {
            m <- ev.arguments.getOrElse("theme",Seq()).filter(thm => thm.matches("Unresolved"))
            priors = (orderedMentions.slice(0, orderedMentions.indexOf(ev.arguments.flatMap(_._2).toSeq.sorted.lastOption.getOrElse(ev))) filter (x => x.isInstanceOf[BioTextBoundMention] &&
              (x.sentence == m.sentence || m.sentence - x.sentence == 1) &&
              !x.matches("Unresolved") &&
              x.matches(m.labels(1)) &&
              doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval, doc.sentences(x.sentence)).get).drop(1) == "N" &&
              !antecedents.contains(x) &&
              !chains.keys.exists(y => y.matches("ComplexEvent") &&
                y.arguments.getOrElse("controller", Seq()).contains(x) &&
                y.arguments.getOrElse("controlled", Seq()).contains(ev)))).distinct

            brk1 = breakable {
              if (priors.isEmpty) break()
            }

            num = cardinality(m)

            themes = priors.takeRight(math.min(num, priors.length))
            if themes.combinations(2).forall(pair => !sameEntityID(pair))
          } yield {
              antecedents ++= themes
              (m, themes)
            }

          val newEvs: Seq[BioEventMention] = (for {
            themeSets <- foundThemes.map(_._2)
            t <- themeSets
          } yield {
              val newEv = new BioEventMention(
                ev.labels, ev.trigger, ev.arguments - "theme" + ("theme" -> Seq(t)), ev.sentence, doc, ev.keep, "corefCardinality")
              newEv.modifications ++= ev.modifications
              newEv.mutableTokenInterval = ev.mutableTokenInterval
              newEv
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


    results = results :+ (for {
      m <- orderedMentions
    } yield resolve(m)).flatten.sorted

    results.map(_.map(_.toBioMention).distinct)
  }
}

