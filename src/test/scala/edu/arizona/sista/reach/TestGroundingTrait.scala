package edu.arizona.sista.reach

import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.grounding._

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

/**
  * Unit tests of the grounding trait.
  *   Written by: Tom Hicks. 2/16/2016.
  *   Last Modified: Fix: test after species reduction.
  */
class TestGroundingTrait extends FlatSpec with Matchers {

//  val g1 = "Mast cells and lymphocytes are not found in bullfrog adenoids."
  val text1 = "AKT1 and lymphocytes are not found in bullfrog adenoids."
  val mentions = getBioMentions(text1)
  val mentions2 = getBioMentions(text1)        // another copy
  val kbr1 = new KBResolution(new KBEntry("test-1", "test1", "UAZtest", "ID1"))
  val kbr2 = new KBResolution(new KBEntry("test2",  "test2", "UAZtest", "ID2"))
  val kbr3 = new KBResolution(new KBEntry("TEST-3", "test3", "UAZtest", "ID3"))
  val resols: Resolutions = Some(Seq(kbr1, kbr2, kbr3))

  text1 should "produce 4 entities mentions" in {
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size (4)
    mentions2 should have size (4)
  }

  "All mentions" should "be grounded" in {
    (mentions.forall(_.isGrounded)) should be (true)
  }

  "All mentions" should "have candidates" in {
    (mentions.forall(_.hasCandidates)) should be (true)
  }

  "First mention" should "have more candidates" in {
    (mentions(0).hasMoreCandidates) should be (true)
    (mentions(1).hasMoreCandidates) should be (false)
    (mentions(2).hasMoreCandidates) should be (false)
    (mentions(3).hasMoreCandidates) should be (false)
  }

  "All mentions" should "have non-empty nsId string" in {
    (mentions.forall(_.nsId != "")) should be (true)
  }


  "After copy, all mentions" should "still be grounded" in {
    mentions(0).copyGroundingFrom(mentions(2)) // overwrite first mention with third
    // printMentions(Try(mentions), true)      // DEBUGGING
    (mentions.forall(_.isGrounded)) should be (true)
  }

  "After copy, all mentions" should "still have candidates" in {
    (mentions.forall(_.hasCandidates)) should be (true)
  }

  "After copy, no mentions" should "have more candidates" in {
    (mentions.forall(! _.hasMoreCandidates)) should be (true)
  }

  "After copy, all mentions" should "still have non-empty nsId string" in {
    (mentions.forall(_.nsId != "")) should be (true)
  }


  "After select current grounding, all mentions" should "still be grounded" in {
    mentions.foreach(_.selectCurrentGrounding)
    // printMentions(Try(mentions), true)      // DEBUGGING
    (mentions.forall(_.isGrounded)) should be (true)
  }

  "After select current grounding, no mentions" should "have candidates anymore" in {
    (mentions.forall(! _.hasCandidates)) should be (true)
  }

  "After select current grounding, no mentions" should "have more candidates anymore" in {
    (mentions.forall(! _.hasMoreCandidates)) should be (true)
  }

  "After select current grounding, all mentions" should "still have non-empty nsId string" in {
    (mentions.forall(_.nsId != "")) should be (true)
  }


  "After nominate, all mentions" should "still be grounded" in {
    mentions2.foreach(_.nominate(resols))
    // printMentions(Try(mentions2), true)      // DEBUGGING
    (mentions2.forall(_.isGrounded)) should be (true)
  }

  "After nominate, all mentions" should "still have candidates" in {
    (mentions2.forall(_.hasCandidates)) should be (true)
  }

  "After nominate, all mentions" should "now have more candidates" in {
    (mentions2.forall(_.hasMoreCandidates)) should be (true)
  }

  "After nominate, all mentions" should "still have non-empty nsId string" in {
    (mentions2.forall(_.nsId != "")) should be (true)
  }


  "After final ground, all mentions" should "still be grounded" in {
    mentions.foreach(_.ground(kbr1))
    // printMentions(Try(mentions), true)      // DEBUGGING
    (mentions.forall(_.isGrounded)) should be (true)
  }

  "After final ground, no mentions" should "have candidates anymore" in {
    (mentions.forall(! _.hasCandidates)) should be (true)
  }

  "After final ground, no mentions" should "have more candidates anymore" in {
    (mentions.forall(! _.hasMoreCandidates)) should be (true)
  }

  "After final ground, all mentions" should "still have non-empty nsId string" in {
    (mentions.forall(_.nsId != "")) should be (true)
  }

}
