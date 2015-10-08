package edu.arizona.sista.reach

import edu.arizona.sista.reach.mentions.BioEventMention
import org.scalatest.{Matchers, FlatSpec}
import TestUtils._


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

  // TODO: this test should account for the binding we DON'T want
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
    val controller = reg.get.arguments("controller").head
    val controlled = reg.get.arguments("controlled").head
    controller.text should be ("Ras")
    controlled.text should be ("sumoylates it")
  }

  // Works across sentences; ignores irrelevant pronouns.
  val sent9 = "Much work has been done on ASPP2. It is known that Ras binds it."
  sent9 should "contain one binding and no other events" in {
    val mentions = parseSentence(sent9)
    mentions.find(_ matches "ComplexEvent") should not be ('defined)
    hasEventWithArguments("Binding", List("Ras", "ASPP2"), mentions) should be (true)
    mentions.filter(_.isInstanceOf[BioEventMention]) should have size (1)
  }

  // Number-sensitive search works with cause controllers but not triggered regulation plurals
  val sent10 = "Ras and Mek are in proximity, and they phosphorylate ASPP2."
  val sent10a = "Ras and Mek are in proximity, and they upregulate the phosphorylation of ASPP2."
  sent10 should "contain one phosphorylation and two regulations" in {
    val mentions = parseSentence(sent10)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    hasPositiveRegulationByEntity("Ras","BioChemicalEntity",Seq("ASPP2"),mentions)
    hasPositiveRegulationByEntity("Mek","BioChemicalEntity",Seq("ASPP2"),mentions)
  }
  sent10a should "contain no regulation events" in {
    val mentions = parseSentence(sent10a)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions)
    mentions.filter(_ matches "Positive_regulation") should have size (0)
  }

  // Number-sensitive search works with cause controlleds but not triggered regulation plurals
  val sent11 = "Ras and Mek are in proximity, and ASPP2 phosphorylates them."
  val sent11a = "The phosphorylation of Ras and the ubiquitination of Mek are common. ASPP2 upregulates both of them."
  sent11 should "contain two phosphorylation and two regulations" in {
    val mentions = parseSentence(sent11)
    mentions.filter(_ matches "Phosphorylation") should have size (2)
    hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("Mek"), mentions) should be (true)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    hasPositiveRegulationByEntity("ASPP2","Phosphorylation",Seq("Ras"),mentions)
    hasPositiveRegulationByEntity("ASPP2","Phosphorylation",Seq("Mek"),mentions)
  }
  sent11a should "contain no regulations" in {
    val mentions = parseSentence(sent11a)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
    mentions.filter(_ matches "Ubiquitination") should have size (1)
    hasEventWithArguments("Ubiquitination", List("Mek"), mentions) should be (true)
    mentions.filter(_ matches "ComplexEvent") should have size (0)
    mentions.filter(_ matches "ActivationEvent") should have size (0)
  }

  // Number-sensitive search works with activation controllers, but plurals are forbidden.
  val sent12 = "Ras is in proximity, and it activates ASPP2."
  val sent12a = "Ras and Mek are in proximity, and they activate ASPP2."
  sent12 should "contain a Positive_activation" in {
    val mentions = parseSentence(sent12)
    mentions.filter(_ matches "ActivationEvent") should have size (1)
    hasEventWithArguments("Positive_activation", List("Ras", "ASPP2"), mentions) should be (true)
  }

  sent12a should "contain no events" in {
    val mentions = parseSentence(sent12a)
    mentions.filter(_ matches "Event") should have size (0)
  }

  // Number-sensitive search works with activation controlleds, but plurals are forbidden.
  val sent13 = "Mek is in proximity, and ASPP2 activates it."
  val sent13a = "Ras and Mek are in proximity, and ASPP2 activates them."
  sent13 should "contain one activation and one regulation" in {
    val mentions = parseSentence(sent13)
    mentions.filter(_ matches "ActivationEvent") should have size (1)
    hasEventWithArguments("Positive_activation", List("ASPP2", "Mek"), mentions) should be (true)
  }

  sent13a should "contain no events" in {
    val mentions = parseSentence(sent13a)
    mentions.filter(_ matches "Event") should have size (0)
  }

  // Sane noun phrases should be matched
  /**
  val sent14 = "Ras is common, and this protein binds GTP."
  sent14 should "contain one binding event only" in {
    val mentions = parseSentence(sent14)
    hasEventWithArguments("Binding", List("Ras", "GTP"), mentions) should be (true)
    mentions should have size (3)
  }
    */

  // Ignore noun phrases that can't have BioChemicalEntity antecedents
  val sent15 = "Ras is common, and a mouse binds GTP."
  sent15 should "not contain any events" in {
    val mentions = parseSentence(sent15)
    mentions filter (_ matches "Event") should have size (0)
    mentions should have size (2)
  }

  // Ignore anything two sentences prior when searching for antecedents.
  val sent16 = "Ras is common. This is an intervening sentence. It binds Mek."
  sent16 should "not contain any events" in {
    val mentions = parseSentence(sent16)
    mentions filter (_ matches "Event") should have size (0)
  }

  // Can find an antecedent mention between start of event mention and start of text bound mention
  val sent17 = "ASPP2 is common, and Ras binds the Mek protein."
  sent17 should "contain a single binding between Mek and Ras" in {
    val mentions = parseSentence(sent17)
    hasEventWithArguments("Binding", List("Ras", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Ras", "ASPP2"), mentions) should be (false)
    hasEventWithArguments("Binding", List("Mek", "ASPP2"), mentions) should be (false)
  }

  // Events with invalid numbers of antecedents are ignored
  val sent18 = "ASPP2 and Ras are common, as is its binding."
  val sent18a = "ASPP2 and Ras are common, as is their activation."
  val sent18b = "The phosphorylation of ASPP2 and Ras is common, as is their upregulation."
  sent18 should "not contain any events" in {
    val mentions = parseSentence(sent18)
    mentions filter (_ matches "Event") should have size (0)
  }

  sent18a should "not contain any events" in {
    val mentions = parseSentence(sent18a)
    mentions filter (_ matches "Event") should have size (0)
  }

  sent18b should "contain no activations or regulations" in {
    val mentions = parseSentence(sent18b)
    mentions filter (_ matches "ComplexEvent") should have size (0)
    mentions filter (_ matches "ActivationEvent") should have size (0)
    mentions filter (_ matches "Phosphorylation") should have size (2)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
  }

  val sent19 = "ASPP1 is common, and it binds Mek and Ras"
  sent19 should "contain two bindings, (ASPP1,Mek) and (ASPP1,Ras)" in {
    val mentions = parseSentence(sent19)
    mentions filter (_ matches "Binding") should have size (2)
    mentions filter (_ matches "Event") should have size (2)
    hasEventWithArguments("Binding", List("ASPP1", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP1", "Ras"), mentions) should be (true)
  }

  val sent20 = "We also monitored how siRNA-induced loss of LMTK2 influenced phosphorylation of PP1Cthr320. Four different LMTK2 siRNAs all markedly reduced LMTK2 levels and this led to a corresponding decrease in PP1Cthr320 phosphorylation."
  sent20 should "not contain an activation of and by the same entity" in {
    val mentions = parseSentence(sent20)
    hasPositiveActivation("LMTK2","LMTK2",mentions) should be (false)
  }
}

