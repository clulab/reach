package edu.arizona.sista.reach.extern.export.context

import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.context.ContextEngine
import scala.collection.mutable
import scala.collection.mutable.SortedSet
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.context.ContextEngine.contextMatching
import edu.arizona.sista.reach.nxml._
import edu.arizona.sista.reach.mentions._

/**
  * Class to output files used by python to generate an HTML file
  *   Written by Enrique Noriega. 27/7/2015.
  *   Last Modified: Correct spelling of possessive.
  */
class IntervalOutput(docs:Seq[Document], entries:Seq[FriesEntry], mentions:Seq[Mention]){

  val sentences:Seq[String] = docs.flatMap(_.sentences.map(s => s.words.mkString(" ")))
  val ctxMentions = new mutable.ArrayBuffer[String]
  val evtIntervals = new mutable.ArrayBuffer[String]
  val evtCtxIndicence = new mutable.ArrayBuffer[String]
  val sections:Seq[String] = docs.zip(entries).flatMap{case (d, e) => List.fill(d.sentences.size)(e.sectionName)}
  val titles:Seq[Boolean] = docs.zip(entries).flatMap{ case (d, e) => List.fill(d.sentences.size)(e.isTitle)}
  val docNums:Seq[Int] = docs.zipWithIndex.flatMap{ case (d, i) => List.fill(d.sentences.size)(i)}
  val eventLines = new mutable.ArrayBuffer[String]
  val citationLines = docs.zip(entries).flatMap{
    case (d, e) =>
      val citations = e.references.toSet
      d.sentences.map{
        s =>
          val start:Int = s.startOffsets(0)
          val end:Int = s.endOffsets.takeRight(1)(0)
          citations.exists{
            i:Int =>
              if(i >= start && i <= end)
                true
              else
                false
          }
      }
  }

  val events = mentions filter {
      case ev:EventMention => true
      case _ => false

  }

  // Sentence counter
  var x = 0

  for (doc <- docs) {

    for(i <- 0 to doc.sentences.size){
        val localEvents = events.filter(e => e.document.id == doc.id && e.sentence == i)

        val its = localEvents.sortWith(_.tokenInterval <= _.tokenInterval) map {
            ev =>
              eventLines += s"${ev.labels.head}\t${x + i}"

              val ti = ev.tokenInterval
              s"${ti.start}-${ti.end-1}"
        }
        if(its.size > 0){
          val itsStr = s"${x+i} " + its.mkString(" ")
          evtIntervals += itsStr
        }

        evtCtxIndicence ++= localEvents.map(_.asInstanceOf[BioEventMention]) flatMap {
           evt =>
             val contexts = evt.context match {
               case Some(ctx) => ctx
               case None => Nil
             }

             val label = evt.label
             val sentenceIx = x + i

             contexts flatMap {
               case (ctxType: String, ctxIds: Seq[String]) =>
                 ctxIds flatMap {
                   ctxId =>
                     val ctxIdx = ContextEngine.getIndex((ctxType, ctxId), ContextEngine.latentVocabulary)
                     Seq(s"$label\t$sentenceIx\t$ctxType\t$ctxIdx")
                 }

             }

        }
    }

    // Store context mentions
    val ctsG = mentions filter {
        case tb:BioTextBoundMention => tb.labels.map(contextMatching.contains(_)).exists(x => x)
        case _ => false
    }

    val cts:Seq[BioTextBoundMention] = ctsG.map(_.asInstanceOf[BioTextBoundMention])


    for(i <- 0 to doc.sentences.size){
        val its = cts.filter(e => e.document.id == doc.id && e.sentence == i).sortWith(_.tokenInterval <= _.tokenInterval) map {
            tb =>
            val ti = tb.tokenInterval
            s"${ti.start}-${ti.end-1}-${tb.text.replace(' ', '_')}-${tb.nsId}"
        }
        val itsStr = s"${x+i} " + (if(its.size > 0) its.mkString(" ") else "")
        ctxMentions += itsStr
    }

    // Sentence counter
    x += doc.sentences.size
  }

}
