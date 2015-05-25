package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin.domains.bigmechanism.summer2015.TestUtils._
import org.scalatest.{Matchers, FlatSpec}

/**
 * Tests coreference-based events
 * Date: 5/22/15
 */
class TestCoreference extends FlatSpec with Matchers {
  val sent1 = "Even more than Ras, ASPP2 is common, as is its ubiquitination."
  sent1 should "produce a ubiquitination of ASPP2" in {
    val mentions = parseSentence(sent1)
    TestUtils.hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  it should "not produce a ubiquitination of Ras" in {
    val mentions = parseSentence(sent1)
    TestUtils.hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (false)
  }

  val sent2 = "Even more than Ras, ASPP2 is common, as is their phosphorylation."
  sent2 should "produce two phosphorylations, one of ASPP2 and one of Ras" in {
    val mentions = parseSentence(sent2)
    TestUtils.hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    mentions.filter(_.label == "Phosphorylation") should have size 2
  }

  val sent3 = "Even more than Ras, ASPP2 is common, as is their binding."
  sent3 should "produce one binding of Ras and ASPP2" in {
    val mentions = parseSentence(sent3)
    TestUtils.hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    mentions.filter(_.label == "Ubiquitination") should have size 2
  }

  val sent15 = "To address the effect of K-Ras ubiquitination on its binding to PI3K and Raf family members, either total G12V-K-Ras or the ubiquitinated subfraction of G12V-K-Ras was immunoprecipitated and the immunoprecipitates were probed with antibodies to detect associated Ras effector molecules."
  sent15 should "contain 2 binding events" in {
    val mentions = parseSentence(sent15)
    hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions) should be (true)
    // TODO: this requires coref!
    hasEventWithArguments("Binding", List("K-Ras", "Raf"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "K-Ras"), mentions) should be (true)
  }
}
