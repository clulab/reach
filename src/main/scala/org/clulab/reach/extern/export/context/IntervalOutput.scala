package org.clulab.reach.extern.export.context

import org.clulab.processors.{Document, Sentence}
import org.clulab.reach.context.ContextEngine
import scala.collection.mutable
import scala.collection.mutable.SortedSet
import org.clulab.odin._
import org.clulab.reach.context.ContextEngine.contextMatching
import org.clulab.reach.mentions._
import ai.lum.nxmlreader.NxmlDocument
import ai.lum.nxmlreader.standoff.Tree
import ai.lum.common.Interval

/**
  * Class to output files used by python to generate an HTML file
  *   Written by Enrique Noriega. 27/7/2015.
  *   Last Modified: Correct spelling of possessive.
  */
class IntervalOutput(doc:Document, nxmlDoc:NxmlDocument, mentions:Seq[Mention]){

  // Used to figure out the tags
  def getStanoffPath(sen:Sentence, nxmlDoc:NxmlDocument) = {
    // Build the sentence interval
    val interval = Interval.open(sen.startOffsets(0), sen.endOffsets.last)
    // Find the corresponding leaves
    val standoff = nxmlDoc.standoff
    val leaves = standoff.getTerminals.filter(i => interval.intersects(i.interval)).toSeq

    leaves.flatMap(l => l.path.split(' ') ++ Seq(l.label)).toSet
  }

  def getSentenceSection(sen:Sentence, nxmlDoc:NxmlDocument):List[String] = {
    // Build the sentence interval
    val interval = Interval.open(sen.startOffsets(0), sen.endOffsets.last)
    // Find the corresponding leaves
    val standoff = nxmlDoc.standoff

    def navigate(n:Tree, i:Interval, counter:Int, buff:List[String]):(Int, List[String]) ={
      var ix = counter
      val nBuff = if(n.label == "sec"){
        val b = if(i.subset(n.interval)){
          val name = if(n.attributes.contains("sec-type")) n.attributes("sec-type") else s"sec$ix"
          name :: buff
        }
        else{
          buff
        }

        ix += 1
        b
      }
      else
        buff


      val mBuff = new mutable.ArrayBuffer[String]
      for(child <- n.children){
        val x = navigate(child, i, ix, Nil)
        ix = x._1
        mBuff ++= x._2
      }

      (ix, nBuff ++ mBuff.toList)
    }

    // See if it is in the abstract
    val path = getStanoffPath(sen, nxmlDoc)

    if(path.contains("abstract"))
      return List("abstract")
    else if(path.contains("article-title")){
      return List("article-title")
    }
    else{
      navigate(standoff, interval, 1, Nil)._2
    }
  }

  val sentences:Seq[String] = doc.sentences.map(s => s.getSentenceText)
  val ctxMentions = new mutable.ArrayBuffer[String]
  val evtIntervals = new mutable.ArrayBuffer[String]
  val evtCtxIndicence = new mutable.ArrayBuffer[String]

  val sections:Seq[String] = doc.sentences.map{
    s =>
      val secs = getSentenceSection(s, nxmlDoc)
      secs match {
        case head::tail => head
        case Nil => "UNDEF"
      }
  }
  val titles:Seq[Boolean] = doc.sentences.map(getStanoffPath(_, nxmlDoc)).map(s => if(s.contains("article-title") || s.contains("title")) true else false)
  val eventLines = new mutable.ArrayBuffer[String]
  // val citationLines = docs.zip(entries).flatMap{
  //   case (d, e) =>
  //     val citations = e.references.toSet
  //     d.sentences.map{
  //       s =>
  //         val start:Int = s.startOffsets(0)
  //         val end:Int = s.endOffsets.takeRight(1)(0)
  //         citations.exists{
  //           i:Int =>
  //             if(i >= start && i <= end)
  //               true
  //             else
  //               false
  //         }
  //     }
  // }

  val events = mentions filter {
      case ev:EventMention => true
      case _ => false
  }

  for(i <- 0 to doc.sentences.size){
      val localEvents = events.filter(e => e.document.id == doc.id && e.sentence == i)

      val its = localEvents.sortWith(_.tokenInterval <= _.tokenInterval) map {
          ev =>
            eventLines += s"${ev.labels.head}\t${i}"

            val ti = ev.tokenInterval
            s"${ti.start}-${ti.end-1}"
      }
      if(its.size > 0){
        val itsStr = s"${i} " + its.mkString(" ")
        evtIntervals += itsStr
      }

      evtCtxIndicence ++= localEvents.map(_.asInstanceOf[BioEventMention]) flatMap {
         evt =>
           val contexts = evt.context match {
             case Some(ctx) => ctx
             case None => Nil
           }

           val label = evt.label
           val sentenceIx = i

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
          s"${ti.start}%${ti.end-1}%${tb.text.replace(' ', '_')}%${tb.nsId}"
      }
      val itsStr = s"${i} " + (if(its.size > 0) its.mkString(" ") else "")
      ctxMentions += itsStr
  }



}
