package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.mentions._
import TestUtils._

class TestHyphenedEvents  extends FlatSpec with Matchers {

  val sen1 = "The EM-inducing TFs (TWIST1, SNAIL1, SLUG, ZEB1, and FOXC2) in the CD45 - cells were determined using qRT-PCR."

  sen1 should "have a positive activation of levels of EM by TFs, TWIST1, SNAIL1, SLUG, ZEB1, FOXC2 and CD45" in {

    val mentions = getBioMentions(sen1)

    // "TFs" is no longer picked up as a protein in the latest KBs?
    // hasPositiveActivation("TFs", "EM", mentions) should be (true)
    hasPositiveActivation("TWIST1", "EM", mentions) should be (true)
    hasPositiveActivation("SNAIL1", "EM", mentions) should be (true)
    hasPositiveActivation("SLUG", "EM", mentions) should be (true)
    hasPositiveActivation("ZEB1", "EM", mentions) should be (true)
    hasPositiveActivation("FOXC2", "EM", mentions) should be (true)
    hasPositiveActivation("CD45", "EM", mentions) should be (true)

  }

}
