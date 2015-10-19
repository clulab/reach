package edu.arizona.sista.coref

import edu.arizona.sista.odin.{Mention, _}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.DarpaLinks
import edu.arizona.sista.reach.display._
import edu.arizona.sista.reach.mentions._

import scala.annotation.tailrec

class Coref {

  val debug: Boolean = false
  val verbose: Boolean = false

  def apply(mentions: Seq[Mention]): Seq[CorefMention] = applyAll(mentions).lastOption.getOrElse(Nil)

  def applyAll(mentions: Seq[Mention], keepAll: Boolean = false): Seq[Seq[CorefMention]] = {

    val doc: Document = mentions.headOption.getOrElse(return Nil).document

    if (debug) {
      println("BEFORE COREF")
      displayMentions(mentions,doc)
      println("Starting coref...")
    }

    val orderedMentions: Seq[CorefMention] = mentions.sorted[Mention].map(_.toCorefMention)

    val links = new DarpaLinks(doc)

    val allLinks = CorefFlow(links.exactStringMatch) andThen
      CorefFlow(links.groundingMatch) andThen
      CorefFlow(links.strictHeadMatch) andThen
      CorefFlow(links.pronominalMatch) andThen
      CorefFlow(links.nounPhraseMatch)

    def genericInside (m: CorefMention): Boolean = {
      @tailrec def genericInsideRec(ms: Seq[CorefMention]): Boolean = {
        if (ms.exists(m => (m matches "Generic_entity") || (m matches "Generic_event"))) true
        else {
          val (tbs, others) = ms.partition(mention => mention.isInstanceOf[CorefTextBoundMention])
          if (others.isEmpty) false
          else genericInsideRec(others.flatMap(_.arguments.values.flatten.map(_.toCorefMention)))
        }
      }
      m.isGeneric || genericInsideRec(Seq(m))
    }

    def argsComplete(args: Map[String,Seq[CorefMention]], lbls: Seq[String]): Boolean = {
      lbls match {
        case binding if lbls contains "Binding" =>
          args.contains("theme") && args("theme").length > 1
        case simple if lbls contains "SimpleEvent" =>
          args.contains("theme") && args("theme").nonEmpty
        case complex if (lbls contains "ComplexEvent") || (lbls contains "Activation") =>
          args.contains("controller") && args.contains("controlled") &&
            args("controller").nonEmpty && args("controlled").nonEmpty
        case _ => true
      }
    }

    def resolveTBMs(mentions: Seq[CorefTextBoundMention]): Map[CorefTextBoundMention,Seq[CorefMention]] = {
      mentions.map(mention => (mention, mention.firstSpecific.map(_.asInstanceOf[CorefMention]))).toMap
    }

    def resolveSimpleEvents(evts: Seq[CorefEventMention], resolvedTBMs: Map[CorefTextBoundMention,Seq[CorefMention]]): Map[CorefEventMention, Seq[CorefEventMention]] = {
      require(evts.forall(_.matches("SimpleEvent")))

      val (generics, specifics) = evts.partition(m => m.isGeneric)

      val (toInspect, solid) = specifics.partition(m => genericInside(m))

      val solidMap = (for {
        s <- solid
      } yield {
          s -> Seq(s)
        }).toMap

      val inspectedMap = (for {
        specific <- toInspect

        resolvedArgs = for {
          (lbl, arg) <- specific.arguments
          argMs = arg.map(m => resolvedTBMs.getOrElse(m.asInstanceOf[CorefTextBoundMention], Nil))
        } yield lbl -> argMs

        argSets = specific match {
          case binding if binding matches "Binding" => {
            val exceptTheme = combineArgs((resolvedArgs - "theme" - "theme1" - "theme2")
              .map(entry => entry._1 -> entry._2.flatten))
            val themeSets = combination(resolvedArgs.getOrElse("theme",Nil) ++
              resolvedArgs.getOrElse("theme1",Nil) ++
              resolvedArgs.getOrElse("theme2",Nil),2)
            val newSets = exceptTheme.flatMap(argMap => themeSets.map(themeSet =>
              argMap + ("theme" -> Seq(themeSet.headOption, themeSet.lastOption).flatten.distinct)))
            newSets
          }
          case _ => combineArgs(resolvedArgs.map(entry => entry._1 -> entry._2.flatten))
        }
      } yield {
          if (verbose && argSets.nonEmpty) {
            argSets.foreach { argSet =>
              println("argSet: ")
              argSet.foreach {
                case (lbl: String, ms: Seq[CorefMention]) =>
                  println(lbl + " -> " + ms.map(_.text).mkString(","))
              }
            }
          }
         val value = argSets.flatMap(argSet =>
           if (argsComplete(argSet,specific.labels)) {
             val generated = new CorefEventMention(
               specific.labels,
               specific.trigger,
               argSet,
               specific.sentence,
               specific.document,
               specific.keep,
               specific.foundBy + specific.sieves.mkString(", ", ", ", ""))
             generated.modifications ++= specific.modifications
             Seq(generated)
           } else Nil
         )
        specific -> value
      }).toMap

      val specificMap = solidMap ++ inspectedMap

      val genericMap = (for {
        generic <- generics
      } yield {
          // FIXME: first specific antecedent might not be best -- might want to combine arguments with previous
          generic -> generic.firstSpecific.flatMap(m => specificMap.getOrElse(m.asInstanceOf[CorefEventMention],Nil))
        }).toMap

      specificMap ++ genericMap
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
      if (ms.flatten.distinct.length <= size) return Seq(ms.flatten.distinct)

      ms.foldLeft[Seq[Seq[A]]](Nil)((args,thm) => args ++ (for {
        ant <- thm
        nxt <- if (size - 1 > 0) group(ms.span(ms.indexOf(_) <= ms.indexOf(thm))._2.flatten.toList,size-1).toSeq else Seq(Nil)
      } yield Seq(Seq(ant), nxt.toSeq).flatten))
    }

    def combineArgs(argRaw: Map[String, Seq[CorefMention]], numThemes: Int = 1): Seq[Map[String, Seq[CorefMention]]] = {
      val args = argRaw.filterKeys(k => argRaw(k).nonEmpty)
      val stableKeys = args.keys.toSeq

      def sum(xs: Seq[Int]): Int = {
        @tailrec
        def inner(xs: List[Int], accum: Int): Int = {
          xs match {
            case x :: tail => inner(tail, accum + x)
            case Nil => accum
          }
        }
        inner(xs.toList, 0)
      }
      def oneLess(countdown: Seq[Int], toRemove: Int): Seq[Int] = {
        var i = countdown.length - 1
        while (i >= 0) {
          if (countdown(i) - toRemove >= 0) {
            return countdown.patch(i,Seq(countdown(i) - toRemove),1)
          }
          i -= 1
        }
        Seq(-1)
      }
      def oneIteration(iteration: Seq[Int], sofar: Seq[Map[String, Seq[CorefMention]]], numThemes: Int): Seq[Map[String, Seq[CorefMention]]] = {
        iteration match {
          case end if sum(iteration) < 0 => sofar
          case _ => {
            oneIteration(
              oneLess(iteration, numThemes),
              sofar :+
                iteration.zipWithIndex.map(arg => stableKeys(arg._2) -> Seq(args(stableKeys(arg._2))(arg._1))).toMap,
              numThemes
            )
          }
          }
        }
      oneIteration(stableKeys.map(args(_).length - 1), Nil, numThemes)
    }

    def resolveComplexEvents(evts: Seq[CorefMention], resolved: Map[CorefMention,Seq[CorefMention]]): Map[CorefMention, Seq[CorefMention]] = {
      require(evts.forall(_.matches("ComplexEvent")))

      val (toInspect, solid) = evts.partition(m => genericInside(m))

      val solidMap = (for {
        s <- solid
      } yield {
          s -> Seq(s)
        }).toMap

      val inspectedMap = (for {
        evt <- evts

        resolvedArgs = for {
          (lbl, arg) <- evt match {
            case rel: CorefRelationMention => rel.arguments
            case evm: CorefEventMention => evm.arguments
          }
          argMs = arg.map(m => resolved.getOrElse(m.asInstanceOf[CorefMention], Nil))
        } yield lbl -> argMs.flatten

        argSets = combineArgs(resolvedArgs)
      } yield {
          evt match {
            case rel: CorefRelationMention => evt.asInstanceOf[CorefRelationMention].arguments
            case evm: CorefEventMention => evt.asInstanceOf[CorefEventMention].arguments
          }
          if (verbose) {
            println("argSets:")
            argSets.foreach { argSet =>
              argSet.foreach {
                case (lbl: String, ms: Seq[CorefMention]) =>
                  println(lbl + " -> " + ms.map(_.text).mkString(","))
              }
            }
          }
          val value = argSets.flatMap(argSet =>
            if (argsComplete(argSet,evt.labels)){
              evt match {
                case rel: CorefRelationMention =>
                  val generated = new CorefRelationMention(
                    evt.labels,
                    argSet,
                    evt.sentence,
                    evt.document,
                    evt.keep,
                    evt.foundBy + evt.sieves.mkString(", ", ", ", ""))
                  generated.modifications ++= evt.modifications
                  Seq(generated)
                case evm: CorefEventMention =>
                  val generated = new CorefEventMention(
                    evt.labels,
                    evt.asInstanceOf[CorefEventMention].trigger,
                    argSet,
                    evt.sentence,
                    evt.document,
                    evt.keep,
                    evt.foundBy + evt.sieves.mkString(", ", ", ", ""))
                  generated.modifications ++= evt.modifications
                  Seq(generated)
              }
            }
            else Nil
          )
          evt -> value
        }).toMap
      solidMap ++ inspectedMap
    }

    def resolve(mentions: Seq[CorefMention]): Seq[CorefMention] = {
      val tbms = mentions.filter(_.isInstanceOf[CorefTextBoundMention]).map(_.asInstanceOf[CorefTextBoundMention])
      val sevts = mentions.filter(m => m.isInstanceOf[CorefEventMention] && m.matches("SimpleEvent")).map(_.asInstanceOf[CorefEventMention])
      val cevts = mentions.filter(m => m.matches("ComplexEvent"))
      val resolvedTBMs = resolveTBMs(tbms)
      if (verbose) resolvedTBMs.foreach{ case (k,v) => println(s"TBM: ${k.text} => (" + v.map(_.text).mkString(",") + ")")}
      val resolvedSimple = resolveSimpleEvents(sevts,resolvedTBMs)
      if (verbose) resolvedSimple.foreach{ case (k,v) => println(s"SimpleEvent: ${k.text} => (" + v.map(_.text).mkString(",") + ")")}
      val resolvedComplex = resolveComplexEvents(cevts, resolvedTBMs ++ resolvedSimple)
      if (verbose) resolvedComplex.foreach{ case (k,v) => println(s"ComplexEvent: ${k.text} => (" + v.map(_.text).mkString(",") + ")")}
      val resolved = resolvedTBMs ++ resolvedSimple ++ resolvedComplex

      val retVal = mentions.flatMap(mention => resolved.getOrElse(mention, Nil)).distinct
      if (verbose) displayMentions(retVal,doc)
      retVal
    }

  Seq(resolve(allLinks(orderedMentions, new LinearSelector)))
  }
}

/*
    def resolve(mention: Mention): Seq[Mention] = {
      mention match {
        case mention: BioTextBoundMention if !mention.matches("Unresolved") => Seq(mention)

        case mention: BioTextBoundMention if mention.matches("Unresolved") => {
          if (debug) print(s"Resolving unresolved BioTextBoundMention ${mention.text} => ")

          val resolved = chains(mention)
            .nextResolved()
            .filter(x => (x.sentence == mention.sentence || mention.sentence - x.sentence == 1) &&
            x.document.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval,x.document.sentences(x.sentence)).get).drop(1) == "N"
            )

          /*
                    val resolved = chains.getOrElse(mention, {
                      println("Mention not used in coreference: " + mention.label + ": " + mention.text)
                      Seq(mention)
                    })
                      .filter(x => !x.matches("Unresolved") &&
                      (x.sentence == mention.sentence || mention.sentence - x.sentence == 1) &&
                      doc.sentences(x.sentence).tags.getOrElse(Array())(findHeadStrict(x.tokenInterval,doc.sentences(x.sentence)).get).drop(1) == "N"
                      )
                      .span(m => m.precedes(mention))._1.lastOption.getOrElse({if (debug) println; return Nil})
                    // resolved.asInstanceOf[BioTextBoundMention].modifications ++= mention.asInstanceOf[BioTextBoundMention].modifications
          */

          if (debug) resolved.foreach(x => println(x.text))

          resolved
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
              args.get("controlled").isEmpty =>
              if (debug) println
              Nil

            case stillUnresolved if args.size < 1 =>
              if (debug) println
              Nil

            case _ =>
              val resolved = new BioRelationMention(mention.labels, args, mention.sentence, doc, mention.keep, mention.foundBy + ",corefResolve")
              resolved.modifications ++= mention.asInstanceOf[BioRelationMention].modifications
              resolved.mutableTokenInterval = mention.tokenInterval
              if (debug) println(resolved.text)
              Seq(resolved)
          }
        }

        case mention: BioEventMention if mention.matches("Unresolved") => {
          if (debug) print(s"Resolving unresolved BioEventMention ${mention.text} => ")

          val resolved = chains(mention)
            .nextResolved()
            .filter(x => x.sentence == mention.sentence || mention.sentence - x.sentence == 1)

          resolved.foreach(m => m.asInstanceOf[BioEventMention].modifications ++= mention.asInstanceOf[BioEventMention].modifications)
          resolved.foreach(m => m.asInstanceOf[BioEventMention].mutableTokenInterval = mention.tokenInterval)

          if (debug) resolved.foreach(m => println(m.text))

          resolved
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
              args.get("controlled").isEmpty =>
              if (debug) println
              Nil
            case stillUnresolved if args.size < 1 =>
              if (debug) println
              Nil
            case _ =>
<<<<<<< HEAD
              val resolved = new BioEventMention(
                mention.labels,
                mention.trigger,
                args,
                mention.sentence,
                mention.document,
                mention.keep,
                mention.foundBy + ",corefResolveArgs")
              resolved.modifications ++= mention.asInstanceOf[BioEventMention].modifications
              resolved.mutableTokenInterval = mention.tokenInterval
              if (debug) println(resolved.text)
              Seq(resolved)
=======
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

    /*
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
>>>>>>> master
          }

        case m => Seq(m)
      }
    }
*/
