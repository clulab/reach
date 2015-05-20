package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
 * Unit tests to ensure that SimpleEvent rules coming from the templatic grammar are matching correctly
 * Date: 5/19/15
 */
class TestTemplaticSimpleEvents extends FlatSpec with Matchers {
  val sent1 = "The phosphorylation on AKT was great."
  sent1 should "not produce a phosphorylation based on the preposition \"on\"" in {
    // TODO: Fails! (GUS)
    val mentions = parseSentence(sent1)
    val p = mentions.find(_.label == "Phosphorylation")
    p.isDefined should be (false)
  }

  val sent2 = "JAK3 phosphorylates three HuR residues (Y63, Y68, Y200)"
  it should "extract 3 phosphorylations and 3 positive regulations" in {
    // TODO: this fails because of bad syntax around Hur. Fix with a surface rule for phosphorylation? (GUS)
    val mentions = parseSentence(sent2)

    val p = mentions.filter(_.label == "Phosphorylation")
    p should have size (3)
    val r = mentions.filter(_.label == "Positive_regulation")
    r should have size (3)
  }

  val sent3 = "The deubiquitination of ASPP2 is promoted by optineurin."
  sent3 should "not contain a ubiquitination event" in {
    val doc = testReach.mkDoc(sent3, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_.label == "Ubiquitination") should be (false)
  }

  val sent4 = "The dephosphorylation of ASPP2 is promotted by optineurin."
  sent4 should "not contain a phosphorylation event" in {
    val doc = testReach.mkDoc(sent4, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_.label == "Phosphorylation") should be (false)
  }

  // This test has been ported from TestDarpaEval2015Training
  val sent5 = "In contrast, the EGFR T669A mutant increased both basal EGFR and ERBB3 tyrosine phosphorylation that was not augmented by MEK inhibition"
  sent5 should "contain two phosphorylations" in {
    val text = sent5
    val doc = testReach.mkDoc(text, "testdoc")
    val phosphorylations = testReach.extractFrom(doc).filter(_.label == "Phosphorylation")
    phosphorylations.size should be (2)
    TestUtils.hasEventWithArguments("Phosphorylation", List("EGFR"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ERBB3"), phosphorylations) should be (true)
  }

  // This test has been ported from TestDarpaEval2015Training
  val sent6 = "We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine phosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 phosphorylation."
  sent6 should "contain three phosphorylations" in {
    val text = sent6
    val doc = testReach.mkDoc(text, "testdoc")
    val phosphorylations = testReach.extractFrom(doc).filter(_.label == "Phosphorylation")
    phosphorylations.size should be (3)
    TestUtils.hasEventWithArguments("Phosphorylation", List("EGFR"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("HER2"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ERBB3"), phosphorylations) should be (true)
  }

  // there is a phosphorylation event in the example text
  val sent7 = "The ubiquitinated Ras protein phosphorylates AKT."
  sent7 should "contain a phosphorylation" in {
    val mentions = testReach.extractFrom(sent7, "testdoc", "1")
    val phospho = mentions.find(_.label == "Phosphorylation")
    phospho.isDefined should be (true)
    phospho.get.arguments.contains("theme") should be (true)
    // simple events get a single argument
    phospho.get.arguments("theme").size should be (1)
    // simple events shouldn't have causes
    // because they are promoted to Positive_regulations
    phospho.get.arguments.contains("cause") should be (false)
    phospho.get.arguments("theme").head.text.contains("AKT") should be (true)
  }

  val sent8 = "We next considered the effect of Ras monoubiquitination on GAP-mediated hydrolysis"
  sent8 should "contain a ubiquitination" in {
    val mentions = parseSentence(sent8)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent9 = "The effects of monoubiquitination on Ras are not isoform-specific."
  sent9 should "contain a ubiquitination" in {
    val mentions = parseSentence(sent9)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent10 = "We measured the rate of GAP-mediated GTP hydrolysis and observed that the response of Ras ligated to Ubiquitin was identical"
  sent10 should "contain a ubiquitination NOT binding" in {
    val mentions = parseSentence(sent10)
    // per Ryan/Guang's comment this is Ubiq not Binding
    // we do not do hydrolysis, so we can ignore that
    hasEventWithArguments("Binding", List("Ras", "Ubiquitin"), mentions) should be (false)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent11 = "monoubiquitinated K-Ras is less sensitive than the unmodified protein to GAP-mediated GTP hydrolysis"
  sent11 should "contain a ubiquitination" in {
    val mentions = parseSentence(sent11)
    hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions) should be (true)
  }

  val sent12 = "Here we show that monoubiquitination decreases the sensitivity of Ras to GAP-mediated hydrolysis"
  sent12 should "contain a ubiquitination" in {
    val mentions = parseSentence(sent12)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent13 = "Indicating that p38 SAPK is not an efficient kinase for ASPP2 phosphorylation."
  sent13 should "contain a phosphorylation" in {
    val mentions = parseSentence(sent13)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent14a = "Experiments revealed ubiquitination at Lys residues 104 and 147 of K-Ras"
  sent14a should "contain 2 ubiquitinations" in {
    val mentions = parseSentence(sent14a)
    mentions.filter(_.label == "Ubiquitination") should have size (2)
  }

  val sent14b = "Experiments revealed ubiquitination at Lys residues 117, 147, and 170 for H-Ras."
  sent14b should "contain 3 ubiquitinations" in {
    val mentions = parseSentence(sent14b)
    mentions.filter(_.label == "Ubiquitination") should have size (3)
  }
}
