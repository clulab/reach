package edu.arizona.sista.reach

import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.grounding._

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

/**
  * Unit tests of the grounding trait.
  *   Written by: Tom Hicks. 3/7/2016
  *   Last Modified: Initial creation.
  */
class TestReachGrounder extends FlatSpec with Matchers {

  val text1 = "Mouse AKT1 phosphorylates PTHR2 in chicken adenoids."
  val mentions = getBioMentions(text1)

  val kbrC = new KBResolution(new KBEntry("FAKE", "fake", "UAZtest", "ID3", "chicken"))
  val kbrH = new KBResolution(new KBEntry("FAKE", "fake", "UAZtest", "ID1", "human"))
  val kbrM = new KBResolution(new KBEntry("FAKE", "fake", "UAZtest", "ID3", "mouse"))
  val kbrR = new KBResolution(new KBEntry("FAKE", "fake", "UAZtest", "ID2", "rice"))
  val resols: Resolutions = Some(Seq(kbrC, kbrH, kbrM, kbrR))

  text1 should "produce 7 entities mentions" in {
    mentions should have size (7)
  }

}
