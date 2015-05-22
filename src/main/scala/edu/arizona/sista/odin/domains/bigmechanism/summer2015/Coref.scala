package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.bionlp.display
import edu.arizona.sista.bionlp.display._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
import util.control.Breaks._
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.DependencyUtils._

import scala.collection.mutable

class Coref extends DarpaFlow {
  def apply(mentions: Seq[Mention], state: State): Seq[BioMention] = applyAll(mentions).lastOption.getOrElse(Seq())

  def applyAll(mentions: Seq[Mention]): Seq[Seq[BioMention]] = {

    val doc: Document = mentions.headOption.getOrElse(return Seq()).document

    println("BEFORE COREF")
    displayMentions(mentions,doc)

    var chains = new mutable.HashMap[Mention,Seq[Mention]]

    val toSplit = new mutable.HashMap[Mention,Seq[Seq[Mention]]]

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
        case ut: BioTextBoundMention if ut.labels.contains("Unresolved") => Seq()
        case t: BioTextBoundMention if !t.labels.contains("Unresolved") => Seq(t)
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
        case mention: TextBoundMention if !mention.labels.contains("Unresolved") => Seq(mention)

        case mention: TextBoundMention if mention.labels.contains("Unresolved") => {
          Seq(chains.getOrElse(mention, {
            println("Mention not used in coreference: " + mention.label + ": " + mention.text)
            Seq(mention)
          })
            .filter(!_.labels.contains("Unresolved"))
            .span(m => m.precedes(mention))._1.lastOption.getOrElse(return Seq()))
        }

        case mention: RelationMention => {
          val args = (for {
            (argType, argMentions) <- mention.arguments
          } yield argType -> argMentions.map(a => resolve(a)).flatten).filter(_._2.nonEmpty)
          args match {
            case stillUnresolved if args.size < 1 => Seq()
            case _ => Seq(new BioRelationMention(mention.labels, args, mention.sentence, doc, mention.keep, mention.foundBy))
          }
        }

        case mention: EventMention if mention.labels.contains("Unresolved") => {
          Seq(chains.getOrElse(mention, {
            println("Mention not used in coreference: " + mention.label + ": " + mention.text)
            Seq(mention)
          })
            .filter(!_.labels.contains("Unresolved"))
            .span(m => m.precedes(mention))._1.lastOption.getOrElse(return Seq()))
        }

        // TODO: special case for complex events (need resolved controller)
        // TODO: special case for bindings based on DarpaActions?

        case mention: EventMention if !mention.labels.contains("Unresolved") & toSplit.contains(mention) =>

            val themeSets = combination(toSplit(mention), themeCardinality(mention.label))
            //println("Number of sets: " + themeSets.length)
            //themeSets.foreach(s => println(s"(${for (m<-s) yield m.text + ","})"))
            for (themeSet <- themeSets) yield new BioEventMention(mention.labels,
              mention.trigger,
              mention.arguments - "theme" + ("theme" -> themeSet),
              mention.sentence,
              mention.document,
              mention.keep,
              "corefSplitter")

        case mention:EventMention if !mention.labels.contains("Unresolved") & !toSplit.contains(mention) =>
            val args = (for {
              (argType, argMentions) <- mention.arguments
            } yield argType -> argMentions.map(a => resolve(a)).flatten.distinct).filter(_._2.nonEmpty)
            args match {
              case stillUnresolved if args.size < 1 => Seq()
              case _ => Seq(new BioEventMention(mention.labels, mention.trigger, args, mention.sentence, mention.document, mention.keep, mention.foundBy))
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

    val hiddenMentions = (for {
      m <- mentions
    } yield lookInside(m)).flatten

    val orderedMentions = (for {m <- (mentions ++ hiddenMentions).distinct} yield getChildren(m)).flatten.distinct.sorted

    orderedMentions.foreach(m => chains += m -> Seq(m))

    var results: Seq[Seq[Mention]] = Seq()

    // exact string matching
    val sameText = chains.keys
      .filter(x => x.isInstanceOf[BioTextBoundMention] && !x.labels.contains("Unresolved"))
      .groupBy (m => m.text.toLowerCase)
      .filter(_._2.toSeq.length > 1)
    sameText.foreach {
      case (ent, ms) =>
        //ms.foreach(m => println("Exact string match for " + m.text))
        val newChain = ms.flatMap(m => chains(m)).toSeq.distinct.sorted
        sameText(ent).foreach(link => chains(link) = newChain)
    }

    results = results :+ (for {
      m <- orderedMentions
    } yield resolve(m)).flatten.sorted

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

    results = results :+ (for {
      m <- orderedMentions
    } yield resolve(m)).flatten.sorted

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
      val sent = m.document.sentences(m.sentence)

      val mhead = findHeadStrict(m.tokenInterval, sent).getOrElse(m.tokenInterval.start)

      val phrase = subgraph(m.tokenInterval, sent)

      // debug1 = println(s"words: ${sent.words.slice(phrase.get.start, phrase.get.end).mkString(",")}\ntags: ${sent.tags.get.slice(phrase.get.start, phrase.get.end).mkString(",")}")
      val dc = detCardinality(sent.words.slice(phrase.get.start, phrase.get.end), sent.tags.get.slice(phrase.get.start, phrase.get.end))

      val hc = headCardinality(sent.words(mhead), sent.tags.get(mhead))

      m match {
        case informativeDeterminer if dc != 0 => dc
        case informativeHead if dc == 0 & hc != 0 => hc
        case _ => 1
      }
    }

    for((mention,i) <- orderedMentions.zipWithIndex) {
      mention match {
        case binding: BioEventMention if binding.labels.contains("Binding") &
          (binding.arguments.getOrElse("theme1", Seq()).exists(thm => thm.labels.contains("Unresolved")) ||
            binding.arguments.getOrElse("theme2", Seq()).exists(thm => thm.labels.contains("Unresolved"))) =>
          println("entering cardinality detector: binding")

          val theme1s = binding.arguments.getOrElse("theme1",Seq())
          val theme2s = binding.arguments.getOrElse("theme2",Seq())
          val totalPriorsNeeded = (theme1s ++ theme2s).foldLeft(0)((sum,m) => sum + cardinality(m))

        case ev: BioEventMention if !ev.labels.contains("Binding") &
          ev.arguments.getOrElse("theme", Seq()).exists(thm => thm.labels.contains("Unresolved")) =>
          println("entering cardinality detector: non-binding")
          val foundThemes = for {
            m <- ev.arguments("theme").filter(thm => thm.labels.contains("Unresolved"))
            priors = orderedMentions.slice(0, i) filter (x => x.isInstanceOf[BioTextBoundMention] &&
              !x.labels.contains("Unresolved") &&
              x.labels.contains(m.labels(1)) &&
              !chains.keys.exists(y => y.labels.contains("ComplexEvent") &&
                y.arguments.getOrElse("controller",Seq()).contains(x) &&
                y.arguments.getOrElse("controlled",Seq()).contains(ev)))

            brk1 = breakable {
              if (priors.isEmpty) break()
            }

            num = cardinality(m)

            themes = priors.takeRight(math.min(num, priors.length))
          } yield (m, themes)

          val numThemes = themeCardinality(ev.label)

          numThemes match {
            case splitEv if foundThemes.map(_._2).flatten.length > numThemes =>
              toSplit(ev.asInstanceOf[EventMention]) = foundThemes.map(_._2)
            case _ =>
              for (t <- foundThemes) {
                val sumChain = chains(t._1) ++ (for {
                  ant <- t._2
                } yield chains(ant)).flatten
                sumChain.foreach(link => chains(link) = sumChain)
              }
          }
        case _ => ()
      }
    }

    results = results :+ (for {
      m <- orderedMentions
    } yield resolve(m)).flatten.sorted

    results.map(_.map(_.toBioMention))
  }
}
