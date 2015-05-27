package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin.domains.bigmechanism.summer2015.TestUtils._
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.bionlp.mentions._

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
    TestUtils.hasEventWithArguments("Binding", List("Ras", "ASPP2"), mentions) should be (true)
    mentions.filter(_.label == "Binding") should have size 1
  }

  val sent4 = "Even more than Ras and Mek, ASPP2 is common, and so is their binding to it."
  sent4 should "produce two bindings: (Ras, ASPP2), (Mek, ASPP2)" in {
    val mentions = parseSentence(sent4)
    TestUtils.hasEventWithArguments("Binding", List("Ras", "ASPP2"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Binding", List("Mek", "ASPP2"), mentions) should be (true)
    mentions.filter(_.label == "Binding") should have size 2
  }

  val sent5 = "To address the effect of Ras ubiquitination on its binding to PI3K and Raf family members, either total G12V-K-Ras or the ubiquitinated subfraction of G12V-K-Ras was immunoprecipitated and the immunoprecipitates were probed with antibodies to detect associated Ras effector molecules."
  sent5 should "contain 2 binding events" in {
    val mentions = parseSentence(sent5)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Ras", "Raf"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "Ras"), mentions) should be (true)
  }

  // Ensure that regulation is removed if no resolved controller is found.
  val sent6 = "It phosphorylates Ras."
  sent6 should "contain no positive regulation" in {
    val mentions = parseSentence(sent6)
    mentions.filter(_ matches "Positive_regulation") should have size (0)
    hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
  }

  // Ensure that controller cannot be antecedent to controlled's arguments
  val sent7 = "Ras phosphorylates it."
  sent7 should "produce no events" in {
    val mentions = parseSentence(sent7)
    mentions.filter(_.isInstanceOf[BioEventMention]) should have size (0)
    mentions should have size (1)
  }

  val sent8 = "ASPP2 is common, it is well known, and Ras sumoylates it."
  sent8 should "contain one sumoylation and one regulation" in {
    val mentions = parseSentence(sent8)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.text should be ("Ras")
    controlled.text should be ("sumoylates it")
  }


}
