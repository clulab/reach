package org.clulab.reach.context.utils.annotation_alignment_utils

import scala.collection.mutable
import scala.collection.immutable.ListMap
object EventAlignmentUtils {
  def isThereSomeMatch(evt1Start:Int, evt1End:Int, evt2Start:Int, evt2End: Int):Boolean = {
    // exact match is when both events have the same start and end token values
    val exactMatch = ((evt1Start == evt2Start) && (evt1End == evt2End))


    // same start is when the tokens start at the same point, but one event must end before the other
    // please note that one event has to end before the other, because if they were the same, they would have already
    // been counted as an exactMatch
    val sameStart = ((evt1Start == evt2Start) && (evt1End < evt2End))


    // same end is when one event may start after the other has already started, but they end at the same token
    // again, they must start at different points, else they would have been counted as an exact match
    val sameEnd = ((evt1End == evt2End) && (evt1Start < evt2Start))

    // containment is when one event is completely inside the other event
    val containment = ((evt1Start < evt2Start) && (evt1End > evt2End))

    //overlap is when one event starts before the other, but also ends before the other.
    // the end of the first event has to be before the second event finishes.
    val overlap = ((evt1Start < evt2Start) && (evt1End < evt2End) && (evt1End > evt2Start))


    exactMatch || sameStart || sameEnd || containment || overlap
  }

  def eventsAlign(eventSpec1:(Int,Int,Int), eventSpec2:(Int,Int,Int)):Boolean = {
    val sameSentenceIndex = eventSpec1._1 == eventSpec2._1
    val someMatchExists = EventAlignmentUtils.isThereSomeMatch(eventSpec1._2, eventSpec1._3, eventSpec2._2, eventSpec2._3)
    sameSentenceIndex && someMatchExists
  }

  def parseEventIDFromStringToTup(eventID: String):(Int,Int,Int) = {
    val sentenceIndexString = eventID.split("from")(0).replace("in","")
    val eventTokenString = eventID.split("from")(1)
    val sentenceIndex = Integer.parseInt(sentenceIndexString)
    val eventTokenStart = Integer.parseInt(eventTokenString.split("to")(0))
    val eventTokenEnd = Integer.parseInt(eventTokenString.split("to")(1))
    (sentenceIndex,eventTokenStart,eventTokenEnd)
  }

  def eventsAlign(evtID1: String, evtID2: String):Boolean = {
    val tupEvt1 = EventAlignmentUtils.parseEventIDFromStringToTup(evtID1)
    val tupEvt2 = EventAlignmentUtils.parseEventIDFromStringToTup(evtID2)
    // the purpose of this function is to align events.
    // Since overlap or containment of one event by another is possible,
    // we need to test if one event contains the other, or vice versa. Same holds for overlap.
    EventAlignmentUtils.eventsAlign(tupEvt1, tupEvt2) || EventAlignmentUtils.eventsAlign(tupEvt2, tupEvt1)
  }


  def parseEventIDFromTupToString(eventID:(Int,Int,Int)):String = {
    s"in${eventID._1}from${eventID._2}to${eventID._3}"
  }

  // checking the condition for neighborhood of events:
 // events A and B are adjacent if B starts exactly when A ends, or B starts at the next word
  def areEventsAdjacent(leftEvent:(Int,Int,Int), rightEvent:(Int,Int,Int)):Boolean = {

    rightEvent._2 == leftEvent._3 + 1
  }


  def getSortedEventSpansPerPaper(eventSpansPerPaper:Map[String,Seq[String]]):Map[String,Map[Int,Seq[(Int,Int,Int)]]] = {

    val toReturn = collection.mutable.HashMap[String,Map[Int,Seq[(Int,Int,Int)]]]()
    for((paperID, eventSpans)<-eventSpansPerPaper) {
      val eventsInTupForm = eventSpans.map(parseEventIDFromStringToTup(_))
      // group the events by sentence index
      val eventsGroupedBySentIndex = eventsInTupForm.groupBy(_._1)
      // sort all the groups by sentence index, and sort each group by the start token of the event span
      val eventsSortedBySentIndex = ListMap(eventsGroupedBySentIndex.toSeq.sortBy(_._1):_*)
      val eventSpansSortedByStartToken = eventsSortedBySentIndex.mapValues(x => x.sortBy(_._2))
      val mapEntry = Map(paperID -> eventSpansSortedByStartToken)
      toReturn ++= mapEntry
    }
    toReturn.toMap
  }


  // we want to build a binary string for each sentence in the current paper.
  // for this, we will lookup the map of unique event spans.
  // If the position (sentence index) of the current paper appeared in the map, it means the current sentence has some missing events,
  // and we need to find the spans and add a 1 to the span and 0 to the rest of the sentence.
  // If not, the current sentence has no unique event spans and we can fill a list of 0s for the length of the sentence
  def makeBinarySentenceFromWords(sentence: String, sentenceIndex: Int,
                                  mapOfEventSpans:Map[Int,Seq[(Int,Int,Int)]]): String = {

    if(mapOfEventSpans.contains(sentenceIndex)) {
      val sentenceToSend = convertWordsToBinaryString(sentence,sentenceIndex,mapOfEventSpans)
      sentenceToSend
    } else {
      val sentenceToSend = List.fill(sentence.length)("0").mkString("")
      sentenceToSend
    }
  }


  // this function handles the case wherein a given sentence does have some unique events in it.
  // the way we do this is by iterating over the tokens of the original sentence, and if the index of the token in the string was a part of
  // an event span, then we keep a 1, else we replace the token with a 0
  // we then return this string of 1s and 0s

  def convertWordsToBinaryString(sentence:String, sentenceIndex:Int, mapOfEventSpans:Map[Int,Seq[(Int,Int,Int)]]):String = {
    val missingEventsInCurrentSent = mapOfEventSpans(sentenceIndex)
    println(missingEventsInCurrentSent)
    val originalTokens = sentence.split(" ").zipWithIndex
    val tokensToReturn = collection.mutable.ListBuffer[String]()
    for((_,index) <- originalTokens) {
      for(m <- missingEventsInCurrentSent) {
        // checking if the index of the current sentence is the start of some event, end, or lies in the event span, then we add a "1"
        // else a "0"
        if(index == m._2 || index == m._3 || index == m._3-m._2)
          tokensToReturn += "1"
        else tokensToReturn += "0"
      }
    }
    tokensToReturn.mkString("")
  }
}
