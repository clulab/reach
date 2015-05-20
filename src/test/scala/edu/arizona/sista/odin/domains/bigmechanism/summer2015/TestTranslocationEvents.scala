package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
 * Unit tests to ensure Translocation rules are matching correctly
 * User: mihais
 * Date: 5/19/15
 */
class TestTranslocationEvents extends FlatSpec with Matchers {
  val sent1 = "We show here that ASPP2 is phosphorylated by the RAS/Raf/MAPK pathway and that this phosphorylation leads to its increased translocation to the cytosol/nucleus and increased binding to p53"
  sent1 should "contain 2 translocation events" in {
    val mentions = parseSentence(sent1)

    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)

    // not sure if this works with the new system...
    hasEventWithArguments("Translocation", List("nucleus"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("cytosol"), mentions) should be (true)

    // TODO: missing two regulations:  phosphorylation leads to transport and binding
    // TODO: missing the binding between ASPP2 and p53 (HARD; ok to miss)
  }

  val sent2 = "ASPP2 is transported from the membrane to the nucleus/cytosol"
  sent2 should "contain 2 translocation events for ASPP2" in {
    val mentions = parseSentence(sent2)

    hasEventWithArguments("Translocation", List("ASPP2", "membrane", "cytosol"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("ASPP2", "membrane", "nucleus"), mentions) should be (true)
  }
}
