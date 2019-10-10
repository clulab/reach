package org.clulab.reach

import org.clulab.reach.TestUtils._
import org.clulab.reach.mentions._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Unit tests to ensure Regulation (both Pos and Neg) rules are matching correctly
  * Date: 5/19/15
  * Last Modified: Update tests for issue #538.
  */
class Failed_TestRegulationEvents extends FlatSpec with Matchers {
  val sent13 = "The inhibition of ASPP1 increases the phosphorylation of ASPP2."
  sent13 should "contain 1 downregulation and NO upregulation events" in {
    val mentions = getBioMentions(sent13)

    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent19 = "The phosphorylation of ASPP1 inhibits the ubiquitination of ASPP2"
  sent19 should "contain a controller with a PTM" in {
    val mentions = getBioMentions(sent19)
    val reg = mentions.find(_ matches "Negative_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.label should equal ("Phosphorylation")
    controlled.labels should contain ("Ubiquitination")
    controlled.asInstanceOf[BioEventMention].isDirect should be (false)
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("ASPP2")
  }

  val sent27 = "rapamycin blocked the serum-stimulated phosphorylation of ERK"
  sent27 should "contain one regulation controlled by rapamycin" in {
    val mentions = getBioMentions(sent27)
    hasNegativeRegulationByEntity("rapamycin", "Phosphorylation", List("ERK"), mentions) should be (true)
  }

  val sent36 = "Taken together , these data suggest that decreased PTPN13 expression enhances EphrinB1 and Erk1 and phosphorylation in epithelial cells ."
  sent36 should "contain 2 negative regulations (not positive)" in {
    val mentions = getBioMentions(sent36)
    // this tests that we capture amods for elements in the paths; necessary to correctly model semantic negatives

    mentions.filter(_.label == "Negative_regulation") should have size (2)
  }

  val sent38 = "These data are consistent with EphrinB1 being a PTPN13 phosphatase substrate and suggest that decreased PTPN13 expression in BL breast cancer cell lines increases phosphorylation of EphrinB1 ."
  sent38 should "contain 1 negative regulation (not positive)" in {
    val mentions = getBioMentions(sent38)
    // this tests that we capture amods for elements in the paths; necessary to correctly model semantic negatives
    mentions.filter(_.label == "Negative_regulation") should have size (1)
  }

  val sent44 = "Cells expressing ErbB3 show tyrosine phosphorylation in response to RAS inhibition"
  sent44 should "contain 1 negative regulation and 1 phosphorylation" in {
    val mentions = getBioMentions(sent44)
    hasNegativeRegulationByEntity("RAS", "Phosphorylation", List("ErbB3"), mentions) should be (true)

    mentions.filter(_ matches "Negative_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
  }

  val sent45 = "Together these data demonstrate that E2-induced SRC-3 phosphorylation is dependent on a direct interaction between SRC-3 and ERalpha and can occur outside of the nucleus."
  sent45 should "contain 1 phosphorylation, 1 positive regulation, and 1 binding" in {
    val mentions = getBioMentions(sent45)
    hasPositiveRegulationByEntity("E2", "Phosphorylation", List("SRC-3"), mentions) should be (true)

    mentions.filter(_ matches "Positive_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    mentions.filter(_ matches "Binding") should have size (1)
    hasEventWithArguments("Binding", List("SRC-3", "ERalpha"), mentions) should be (true)
  }

  val sent47 = "The phosphorylation of AFT by BEF is inhibited by the ubiquitination of Akt."
  sent47 should "contain a regulation of a regulation" in {
    val mentions = getBioMentions(sent47)
    hasPositiveRegulationByEntity("BEF", "Phosphorylation", List("AFT"), mentions) should be (true)

    val inner = mentions.filter(_ matches "Positive_regulation")
    val outer = mentions.filter(_ matches "Negative_regulation")
    inner should have size (1)
    outer should have size (1)
    outer.head.arguments("controlled").head == inner.head should be (true)
  }

  val sent54c = "The phosphorylation of AKT1 following inhibition of MEK."
  sent54c should "contain 1 negative regulation" in {
    val mentions = getBioMentions(sent54c)
    hasNegativeRegulationByEntity("MEK", "Phosphorylation", List("AKT1"), mentions) should be (true)
  }

  val sent41 = "Our model, in which E2-induced SRC-3 phosphorylation occurs in a complex with ER"
  sent41 should "contain 1 positive regulation and 1 phosphorylation" in {
    val mentions = getBioMentions(sent41)
    hasPositiveRegulationByEntity("E2", "Phosphorylation", List("SRC-3"), mentions) should be (true)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
  }

  val sent58 = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D), and accordingly, MEK inhibition substantially increased tyrosine phosphorylated ERBB3 levels (Figure 1A)."
  sent58 should "contain 1 amount, 1 binding, and 2 negative regulations" in {
    val mentions = getBioMentions(sent58)
    mentions.filter(_.label.contains("Amount")) should have size (1)
    mentions.filter(_.label.contains("Binding")) should have size (1)
    mentions.filter(_.label.contains("Negative_regulation")) should have size (2)
    mentions.filter(_.label.contains("Negative_activation")) should have size (0)
  }

  val sent60 = "ATP reduced GSH depletion"
  sent60 should "recognize depletion as a positive activation" in {
    val mentions = getBioMentions(sent60)
    mentions.filter(_.label == "Positive_activation") should have size (1)
  }


}
