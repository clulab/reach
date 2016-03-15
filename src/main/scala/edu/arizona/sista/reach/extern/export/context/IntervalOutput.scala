package edu.arizona.sista.reach.extern.export.context

import edu.arizona.sista.processors.Document
import scala.collection.mutable
import scala.collection.mutable.SortedSet
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.context.ContextEngine.contextMatching
import edu.arizona.sista.reach.nxml._

/**
  * Class to output files used by python to generate an HTML file
  *   Written by Enrique Noriega. 27/7/2015.
  *   Last Modified: Correct spelling of possessive.
  */
class IntervalOutput(docs:Seq[Document], entries:Seq[FriesEntry], mentions:Seq[Mention]){

  val sentences:Seq[String] = docs.flatMap(_.sentences.map(s => s.words.mkString(" ")))
  val ctxMentions = new mutable.ArrayBuffer[String]
  val evtIntervals = new mutable.ArrayBuffer[String]
  val sections:Seq[String] = docs.zip(entries).flatMap(_ match {case (d, e) => List.fill(d.sentences.size)(e.sectionName)})
  val titles:Seq[Boolean] = docs.zip(entries).flatMap(_ match { case (d, e) =>
  List.fill(d.sentences.size)(e.isTitle)})
  val eventLines = new mutable.ArrayBuffer[String]

  val events = mentions filter {
      case ev:EventMention => true
      case _ => false

  }

  // Sentence counter
  var x = 0

  for (doc <- docs) {

    for(i <- 0 to doc.sentences.size){
        val its = events.filter(e => e.document.id == doc.id && e.sentence == i).sortWith(_.tokenInterval <= _.tokenInterval) map {
            ev =>
              eventLines += s"${ev.labels.head}\t${x + i}"

              val ti = ev.tokenInterval
              s"${ti.start}-${ti.end-1}"
        }
        if(its.size > 0){
          val itsStr = s"${x+i} " + its.mkString(" ")
          evtIntervals += itsStr
        }
    }

    // Store context mentions
    val cts = mentions filter {
        case tb:TextBoundMention => tb.labels.map(contextMatching.contains(_)).exists(x => x)
        case _ => false
    }


    for(i <- 0 to doc.sentences.size){
        val its = cts.filter(e => e.document.id == doc.id && e.sentence == i).sortWith(_.tokenInterval <= _.tokenInterval) map {
            tb =>
            val ti = tb.tokenInterval
            s"${ti.start}-${ti.end-1}"
        }
        val itsStr = s"${x+i} " + (if(its.size > 0) its.mkString(" ") else "")
        ctxMentions += itsStr
    }

    // Sentence counter
    x += doc.sentences.size
  }

}
