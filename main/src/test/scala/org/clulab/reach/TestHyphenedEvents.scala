package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.mentions._
import TestUtils._

class TestHyphenedEvents  extends FlatSpec with Matchers {

  val sen1 = "The KRas-inducing TFs (TWIST1, SNAIL1, SLUG, ZEB1, and FOXC2) in the CD45 - cells were determined using qRT-PCR."

  sen1 should "have a positive activation of levels of EM by TFs, TWIST1, SNAIL1, SLUG, ZEB1, FOXC2 and CD45" in {

    val mentions = getBioMentions(sen1)
    val controlled = "KRas"

    // "TFs" is no longer picked up as a protein in the latest KBs?
    // hasPositiveActivation("TFs", "EM", mentions) should be (true)
    hasPositiveActivation("TWIST1", controlled, mentions) should be (true)
    hasPositiveActivation("SNAIL1", controlled, mentions) should be (true)
    hasPositiveActivation("SLUG", controlled, mentions) should be (true)
    hasPositiveActivation("ZEB1", controlled, mentions) should be (true)
    hasPositiveActivation("FOXC2", controlled, mentions) should be (true)
    hasPositiveActivation("CD45", controlled, mentions) should be (true)

  }

}
