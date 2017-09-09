package org.clulab.reach

import org.clulab.reach.grounding._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._
import org.clulab.reach.grounding.ReachKBUtils._


/**
  * Test that the SRI drug override file produces all the right entity mentions.
  *   Written by: Tom Hicks. 9/7/2016.
  *   Last Modified: Initial creation.
  */
class TestSriDrugs extends FlatSpec with Matchers {

  /** Override test driver method. */
  def testMentions (
    text: String,
    id: String,
    label: String,
    displayLabel: Option[String] = None,
    groundedHuman: Boolean = false
  ): Unit = {
    val mentions = getBioMentions(text)

    // printMentions(Try(mentions), true)      // DEBUGGING

    text should "have expected number of results" in {
      mentions should not be (empty)
      mentions should have size (1)
    }

    it should s"have labeled all mentions as ${label}" in {
      mentions.count(_ matches label) should be (1)
    }

    if (displayLabel.isDefined) {
      it should s"have display labeled all mentions as ${displayLabel}" in {
        mentions.count(_.displayLabel == displayLabel.get) should be (1)
      }
    }

    if (groundedHuman) {
      it should "have grounded all mentions as Human" in {
        mentions.forall(m => m.grounding.isDefined &&
          Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
      }
    }

    it should "have the expected grounding ID" in {
      mentions.forall(m => m.grounding.isDefined && (m.grounding.get.id == id)) should be (true)
    }

  }


  // def validLines (lines: List[String]): List[String] = {
  //   lines.map(_.trim).filter{ s => (!s.isEmpty) && (!s.startsWith("#")) }
  // }

  def readEmbedAndTest: Unit = {
//    val lines = validLines(ReachKBUtils.readLines("SRI-Drug-Overrides.tsv.gz"))
    val lines = ReachKBUtils.readLines("SRI-Drug-Overrides.tsv.gz")
                .map(_.trim).filter{ s => (!s.isEmpty) && (!s.startsWith("#")) }
    var cnt = 0
    lines.foreach { line =>
      val fields = line.split("\t")
      if (fields.size > 4) {
        cnt += 1
        val word = fields(0)
        val id = fields(1)
        val label = fields(4)
        val sent = s"The chemical ${word} will be classified a drug."
        // println(s"${word}\t\t${id}\t\t${label}") // DEBUGGING
        testMentions(sent, id, label, Some(label), true)
      }
    }
    println(s"Tested ${cnt} words (${cnt * 5} tests)")
  }

  readEmbedAndTest

}
