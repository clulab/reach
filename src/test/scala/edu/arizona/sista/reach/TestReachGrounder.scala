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

  val text1 = "Mouse AKT1 phosphorylates PTHR2 in chicken adenoids."
  val mentions = getBioMentions(text1)

  text1 should "produce 7 entities mentions" in {
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size (7)
  }

}
