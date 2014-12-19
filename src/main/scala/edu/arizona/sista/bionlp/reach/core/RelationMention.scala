package edu.arizona.sista.bionlp.reach.core

import edu.arizona.sista.matcher.Mention
import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Interval

/**
 * Created by gus on 12/19/14.
 */

class RelationMention
  (val label: String,
  val arguments: Map[String, Seq[Mention]],
  val sentence: Int,
  val document: Document,
  val foundBy: String) extends Mention {
    // token interval that contains trigger and all matched arguments
    override def tokenInterval: Interval = {
      val allStarts = arguments.values.flatMap(_.map(_.start)).toSeq
      val allEnds = arguments.values.flatMap(_.map(_.end)).toSeq
      Interval(allStarts.min, allEnds.max)
    }
}
