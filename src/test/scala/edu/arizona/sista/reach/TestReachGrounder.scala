package edu.arizona.sista.reach

import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.grounding._

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

/**
  * Unit tests of the grounding trait.
  *   Written by: Tom Hicks. 3/7/2016
  *   Last Modified: Minor cleanups.
  */
class TestReachGrounder extends FlatSpec with Matchers {

  val text1 = "AKT1 phosphorylates PTHR2."
  val mentions1 = getBioMentions(text1)

  text1 should "produce 4 mentions" in {
    // printMentions(Try(mentions1), true)      // DEBUGGING
    mentions1 should have size (4)
  }

  "Text1 non-BioTextBound mentions" should "not have groundings" in {
    mentions1.filter(!_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isEmpty) should be (true)
  }

  "Text1 BioTextBound mentions" should "have groundings" in {
    mentions1.filter(_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isDefined) should be (true)
  }

  "Text1 mentions" should "be grounded as human" in {
    mentions1.filter(_.isInstanceOf[BioTextBoundMention])
             .forall(m => m.grounding.isDefined &&
                     m.grounding.get.species == "homo sapiens") should be (true)
  }


  // val text2 = "Rat ITSN1 is different from human ITSN1."
  val text2 = "Mouse AKT1 is different from rice AKT1."
  val mentions2 = getBioMentions(text2)

  text2 should "produce 4 mentions" in {
  // printMentions(Try(mentions2), true)       // DEBUGGING
    mentions2 should have size (4)
  }

  "Text2 non-BioTextBound mentions" should "not have groundings" in {
    mentions2.filter(!_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isEmpty) should be (true)
  }

  "Text2 BioTextBound mentions" should "have groundings" in {
    mentions2.filter(_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isDefined) should be (true)
  }


  val text3 = "Mouse AKT1 phosphorylates PTHR2 in chicken adenoids."
  val mentions3 = getBioMentions(text3)

  text3 should "produce 7 mentions" in {
    // printMentions(Try(mentions3), true)      // DEBUGGING
    mentions3 should have size (7)
  }

  "Text3 non-BioTextBound mentions" should "not have groundings" in {
    mentions3.filter(!_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isEmpty) should be (true)
  }

  "Text3 BioTextBound mentions" should "have groundings" in {
    mentions3.filter(_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isDefined) should be (true)
  }

}
