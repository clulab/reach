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


  def getSortedEventSpansPerPaper(eventSpansPerPaper:Map[String,Seq[String]]):Seq[(String,Seq[(Int,Seq[(Int,Int,Int)])])] = {

    val toReturn = collection.mutable.ListBuffer[(String,Seq[(Int,Seq[(Int,Int,Int)])])]()
    for((paperID, eventSpans)<-eventSpansPerPaper) {
      val eventsInTupForm = eventSpans.map(parseEventIDFromStringToTup(_))
      // group the events by sentence index
      val eventsGroupedBySentIndex = eventsInTupForm.groupBy(_._1).toSeq
      // sort all the groups by sentence index, and sort each group by the start token and end token of the event span
      val eventsSortedBySentIndex = eventsGroupedBySentIndex.sortBy(_._1)

      val eventSpansSortedByStartToken = eventsSortedBySentIndex.map(x => (x._1, x._2.sortBy(y => (y._2,y._3))))
      val mapEntry = (paperID, eventSpansSortedByStartToken)
      toReturn += mapEntry
    }
    toReturn
  }


  // we want to build a binary string for each sentence in the current paper.
  // for this, we will lookup the map of unique event spans.
  // If the position (sentence index) of the current paper appeared in the map, it means the current sentence has some missing events,
  // and we need to find the spans and add a 1 to the span and 0 to the rest of the sentence.
  // If not, the current sentence has no unique event spans and we can fill a list of 0s for the length of the sentence
  def makeBinarySentenceFromWords(sentence: String, sentenceIndex: Int, mapOfEventSpans: Seq[(Int, Seq[(Int, Int, Int)])], paperID: String, reachVersion: String): String = {
    val sentenceIndices = mapOfEventSpans.map(_._1)
    if(sentenceIndices.contains(sentenceIndex)) {
      val uniqueEventsFromCurrentSent = mapOfEventSpans.toMap
      println(s"Reach version: ${reachVersion}")
      println(s"Map of event spans in sentence ${sentenceIndex}: ")
      println(uniqueEventsFromCurrentSent(sentenceIndex))
      val sentenceToSend = s"${paperID},${sentenceIndex}:=${convertWordsToBinaryString(sentence,uniqueEventsFromCurrentSent(sentenceIndex))}"
      sentenceToSend
    } else {
      val sentenceToSend = s"${paperID},${sentenceIndex}:=${List.fill(sentence.length)("0").mkString("")}"
      sentenceToSend
    }
  }


  // this function handles the case wherein a given sentence does have some unique events in it.



  def convertWordsToBinaryString(sentence:String, mapOfEventSpans:Seq[(Int,Int,Int)]):String = {
    val eventSpansAs1s = mapOfEventSpans.map(x => {
      val numOf1s = Math.abs(x._3 - x._2) + 1
      val stringOf1s = List.fill(numOf1s)("1").mkString("")
      (x._2, x._3, stringOf1s)
    })
    val leadingZeroesLength = eventSpansAs1s(0)._1
    println(eventSpansAs1s)
    println(s"Number of leading zeroes: ${leadingZeroesLength}")
    println(s"Current sentence size: ${sentence.length}")
    println(s"Current sentence: \n ${sentence}")
    val stringOfLeadingZeroes = List.fill(leadingZeroesLength)("0").mkString("")
    val stringBuilder = new StringBuilder(stringOfLeadingZeroes)
    for(i <- 1 until eventSpansAs1s.length) {
      val previousSpan = eventSpansAs1s(i-1)._3
      stringBuilder ++= previousSpan
      val numOfZerosBtwnLeftRight = (Math.abs(eventSpansAs1s(i)._1 - eventSpansAs1s(i-1)._2))-1
      val zeroesBetweenEvents = List.fill(numOfZerosBtwnLeftRight)("0").mkString("")
      stringBuilder ++= zeroesBetweenEvents
    }
    val trailingZeroesLength = Math.abs(sentence.length - 1 - eventSpansAs1s(eventSpansAs1s.length-1)._2)
    val stringOfTrailingZeroes = List.fill(trailingZeroesLength)("0").mkString("")
    stringBuilder ++= stringOfTrailingZeroes
    stringBuilder.toString()
  }
}
