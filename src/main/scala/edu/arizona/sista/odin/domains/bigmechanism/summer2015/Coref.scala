package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.bionlp.mentions.BioTextBoundMention
import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document

import scala.collection.mutable

class Coref extends DarpaFlow {
  def apply(mentions: Seq[Mention], state: State): Seq[Mention] = applyAll(mentions).last

  def applyAll(mentions: Seq[Mention]): Seq[Seq[Mention]] = {
    val doc: Document = mentions.head.document

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

    def getChildren (m: Mention): Seq[Mention] = {
      case t: TextBoundMention => Seq(t)
      case e: EventMention =>
        (for {
          (k, v) <- e.arguments
          a <- v
        } yield {
            a match {
              case en: TextBoundMention => Seq(en)
              case ev: EventMention => ev +: getChildren(ev)
              case rm: RelationMention => rm +: getChildren(rm)
            }
          }).flatten.toSeq
      case r: RelationMention =>
        (for {
          (k, v) <- r.arguments
          a <- v
        } yield {
            a match {
              case en: TextBoundMention => Seq(en)
              case ev: EventMention => ev +: getChildren(ev)
              case rm: RelationMention => rm +: getChildren(rm)
            }
          }).flatten.toSeq
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
            case _ => Seq(new RelationMention(mention.labels, args, mention.sentence, doc, mention.keep, mention.foundBy))
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

        case mention: EventMention if !mention.labels.contains("Unresolved") => {
          if (toSplit contains mention.trigger) {
            val themeSets = combination(toSplit(mention.trigger), themeCardinality(mention.label))
            //println("Number of sets: " + themeSets.length)
            //themeSets.foreach(s => println(s"(${for (m<-s) yield m.text + ","})"))
            for (themeSet <- themeSets) yield new EventMention(mention.labels,
              mention.trigger,
              mention.arguments - "theme" + ("theme" -> themeSet),
              mention.sentence,
              mention.document,
              mention.keep,
              "corefSplitter")
          } else {
            val args = (for {
              (argType, argMentions) <- mention.arguments
            } yield argType -> argMentions.map(a => resolve(a)).flatten.distinct).filter(_._2.nonEmpty)
            args match {
              case stillUnresolved if args.size < 1 => Seq()
              case _ => Seq(new EventMention(mention.labels, mention.trigger, args, mention.sentence, mention.document, mention.keep, mention.foundBy))
            }
          }
        }
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
      .filter(x => x.isInstanceOf[TextBoundMention] && !x.labels.contains("Unresolved"))
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
      .groupBy(m => m.asInstanceOf[BioTextBoundMention].xref.get.namespace + m.asInstanceOf[BioTextBoundMention].xref.get.id)
    sameGrounding.foreach {
      case (gr, ms) =>
        val newChain = ms.flatMap(m => chains(m)).toSeq.distinct.sorted
        sameGrounding(gr).foreach(link => chains(link) = newChain)
    }

    results = results :+ (for {
      m <- orderedMentions
    } yield resolve(m)).flatten.sorted

    results
  }
}
