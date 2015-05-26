package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.bionlp.mentions._
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
  sent2 should "contain a regulation" in {
    val mentions = testReach.extractFrom(sent2, "testdoc", "1")
    val reg = mentions.find(_.label == "Positive_regulation")
    reg.isDefined should be (true)
    reg.get.arguments.contains("controlled") should be (true)
    reg.get.arguments.contains("controller") should be (true)
    reg.get.arguments("controlled").head.label == "Phosphorylation" should be (true)
    reg.get.arguments("controller").head.text.contains("Ras") should be (true)
  }

  val sent3 = "Interestingly, we observed two conserved putative MAPK phosphorylation sites in ASPP1 and ASPP2"
  sent3 should "contain 2 phosphorylations and 2 regulations" in {
    val mentions = parseSentence(sent3)
    hasEntity("MAPK", mentions) should be (true)
    hasEntity("ASPP1", mentions) should be (true)
    hasEntity("ASPP2", mentions) should be (true)

    hasEventWithArguments("Phosphorylation", List("ASPP1"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)

    hasPositiveRegulationByEntity("MAPK", "Phosphorylation", List("ASPP1"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent4 = "We thus tested whether RAS activation may regulate ASPP2 phosphorylation"
  sent4 should "contain 1 phosphorylation and no regulation" in {
    val mentions = parseSentence(sent4)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    // we don't do unspecified regulations anymore, only + and -
    hasPositiveRegulationByEntity("RAS", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent5 = "MAPK1 was clearly able to phosphorylate the ASPP2 fragment in vitro"
  sent5 should "contain 1 regulation" in {
    val mentions = parseSentence(sent5)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent6 = "Under the same conditions, ASPP2 (693-1128) fragment phosphorylated by p38 SAPK had very low levels of incorporated 32P"
  sent6 should "contain 1 regulation" in {
    val mentions = parseSentence(sent6)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    // TODO: This is failing because we're missing SAPK in "p38 SAPK"; we only get p38, but we used to get "p38 SAPK"
    hasPositiveRegulationByEntity("SAPK", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent7 = "The phosphorylated ASPP2 fragment by MAPK1 was digested by trypsin and fractioned on a high performance liquid chromatography."
  sent7 should "contain 1 regulation" in {
    val mentions = parseSentence(sent7)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent8 = "Hence ASPP2 can be phosphorylated at serine 827 by MAPK1 in vitro."
  sent8 should "contain 1 regulation" in {
    val mentions = parseSentence(sent8)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent10 = "ASPP1 fails to upregulate the phosphorylation of ASPP2."
  sent10 should "contains 1 regulation and 1 phosphorylation event" in {
    val mentions = parseSentence(sent10)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent11 = "ASPP1 fails to downregulate the phosphorylation of ASPP2."
  sent11 should "contains 1 downregulation and 1 phosphorylation event" in {
    val mentions = parseSentence(sent11)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent12 = "ASPP1 downregulates the phosphorylation of ASPP2."
  sent12 should "contains 1 downregulation and 1 phosphorylation event" in {
    val mentions = parseSentence(sent12)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent13 = "The inhibition of ASPP1 increases the phosphorylation of ASPP2."
  sent13 should "contain 1 downregulation and NO upregulation events" in {
    val mentions = parseSentence(sent13)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent14 = "the phosphorylation of ASPP2 is increased by the inhibition of ASPP1."
  sent14 should "contain 1 downregulation and NO upregulation events" in {
    val mentions = parseSentence(sent14, verbose = false)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent15 = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D)."
  sent15 should "contain 1 negative regulation and NO positive activation or regulation events" in {
    val mentions = parseSentence(sent15)
    hasNegativeRegulationByEntity("MEK", "Binding", List("ERBB3", "PI3K"), mentions) should be (true)
    mentions.filter(_.label == "Negative_activation") should have size (0)
    mentions.filter(_.label == "Positive_activation") should have size (0)
  }

  val sent16 = "the inhibition of ASPP1 decreases ASPP2 phosphorylation."
  sent16 should "contain 1 positive regulation, and NO negative regulations or activations" in {
    val mentions = parseSentence(sent16)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent17 = "ASPP1 is an activator of the ubiquitination of ASPP2"
  sent17 should "contain 1 positive regulation, and NO negative regulations or activations" in {
    val mentions = parseSentence(sent17)
    hasPositiveRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent18 = "ASPP1 is an inhibitor of the ubiquitination of ASPP2"
  sent18 should "contain 1 negative regulation, and NO positive regulations or activations" in {
    val mentions = parseSentence(sent18)
    hasNegativeRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent19 = "The phosphorylation of ASPP1 inhibits the ubiquitination of ASPP2"
  sent19 should "contain a controller with a PTM" in {
    val mentions = parseSentence(sent19)
    val reg = mentions.find(_ matches "Negative_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.text should be ("ASPP1")
    controller.modifications should have size (1)
    controller.modifications.head.label should be ("phosphorylated")
    controlled.labels should contain ("Ubiquitination")
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("ASPP2")
  }

  val sent20 = "The binding of ASPP1 and ASPP2 promotes the phosphorylation of MEK"
  sent20 should "contain a controller with a complex" in {
    val mentions = parseSentence(sent20)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.text should be ("ASPP1 and ASPP2")
    controller.labels should contain ("Complex")
    controlled.labels should contain ("Phosphorylation")
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("MEK")
  }

}
