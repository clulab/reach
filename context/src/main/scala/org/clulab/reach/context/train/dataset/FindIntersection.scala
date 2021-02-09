package org.clulab.reach.context.train.dataset

import com.typesafe.config.ConfigFactory
import org.clulab.reach.context.utils
import org.clulab.reach.context.utils.PaperExtraction
import org.clulab.struct.Interval

import scala.collection.mutable.ListBuffer

/**
  * Utility to compute the intersection of the annotated events and the paper extractions
  */
object FindIntersection extends App {

  val config = ConfigFactory.load().getConfig("findIntersection")

  val inputPaperData = config.getString("inputPaperData")
  val inputExtractions = config.getString("inputExtractions")

  def getIntersection(left: Seq[PaperExtraction], right: Seq[PaperExtraction]): Seq[PaperExtraction] = {
    // Small helper function
    def intersects(a: Interval, b: Interval): Boolean = {
      val (shorter, longer) = if (a.size <= b.size) (a, b) else (b, a)
      if (longer contains shorter)
        true
      else if (shorter.intersect(longer).nonEmpty)
        true
      else
        false
    }

    val sentences =
      ((left map (_.sent)) ++
        (right map (_.sent))).distinct

    val gpBySentLeft = left.groupBy(_.sent)
    val gpBySentRight = right.groupBy(_.sent)

    val intersection = ListBuffer[PaperExtraction]()

    for (sentIx <- sentences) {
      if ((gpBySentLeft contains sentIx) && (gpBySentRight contains sentIx)) {
        for {
          l <- gpBySentLeft(sentIx)
          r <- gpBySentRight(sentIx)
        } {
          if (intersects(l.interval, r.interval))
            intersection += l
        }
      }
    }

    intersection.toList.distinct
  }

  val parsedAnnotations = utils.readSerializedPaperAnnotations(inputPaperData)
  val reachExtractions = utils.readSerializedExtractions(inputExtractions)

  val parsedEvents = parsedAnnotations.mapValues(t => utils.getEvents(t.extractions))
  val reachEvents = reachExtractions.mapValues(utils.getEvents)

  for (pmcid <- parsedEvents.keys) yield {
    val parsed = parsedEvents(pmcid)
    val extracted = reachEvents(pmcid)
    val intersection = getIntersection(parsed, extracted)


    println(s"$pmcid Original size: ${parsed.size}, Extracted size: ${extracted.distinct.size}, Intersection: ${intersection.size}")
  }
}
