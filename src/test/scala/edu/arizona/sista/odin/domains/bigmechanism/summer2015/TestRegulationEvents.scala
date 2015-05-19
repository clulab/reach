package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
 * Unit tests to ensure Regulation (both Pos and Neg) rules are matching correctly
 * Date: 5/19/15
 */
class TestRegulationEvents extends FlatSpec with Matchers {
  val sent1 = "Phosphorylation of ASPP2 by MAPK is required for RAS induced increased binding to p53 and increased transactivation of pro-apoptotic genes."
  sent1 should "have an up-regulated phosphorylation" in {
    val doc = testReach.mkDoc(sent1, "testdoc")
    val mentions = testReach extractFrom doc
    assert(hasPositiveRegulationByEntity("MAPK", "Phosphorylation", Seq("ASPP2", "MAPK"), mentions))
  }

  // there is an implicit regulation in the example text
  // it is the cause of the phosphorylation
  val sent2 = "The ubiquitinated Ras protein phosphorylates AKT."
  it should "extract a regulation" in {
    val mentions = testReach.extractFrom(sent2, "testdoc", "1")
    val reg = mentions.find(_.label == "Positive_regulation")
    reg.isDefined should be (true)
    reg.get.arguments.contains("controlled") should be (true)
    reg.get.arguments.contains("controller") should be (true)
    reg.get.arguments("controlled").head.label == "Phosphorylation" should be (true)
    reg.get.arguments("controller").head.text.contains("Ras") should be (true)
  }

}
