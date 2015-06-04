package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}

import TestUtils._

/**
 * Unit tests to ensure Activation event rules are matching correctly
 * User: mihais
 * Date: 5/19/15
 */
class TestActivationEvents extends FlatSpec with Matchers {
  val sent1 = "Nucleotide free Ras inhibits PI3KC2Beta activity."
  val sent1b = "Nucleotide free Ras inhibits PI3KC2Beta."
  val sent1c = "Nucleotide free Ras inhibits activation of PI3KC2Beta."
  val sent1d = "Addition of Ras inhibits PI3KC2Beta."
  val sent1e = "Increase of Ras dose inhibits PI3KC2Beta."
  sent1 should "contain negative activation patterns" in {
    var mentions = parseSentence(sent1)
    mentions.filter(_.label == "Negative_activation") should have size (1)

    mentions = parseSentence(sent1b)
    mentions.filter(_.label == "Negative_activation") should have size (1)

    mentions = parseSentence(sent1c)
    mentions.filter(_.label == "Negative_activation") should have size (1)

    mentions = parseSentence(sent1d)
    mentions.filter(_.label == "Negative_activation") should have size (1)

    mentions = parseSentence(sent1e)
    mentions.filter(_.label == "Negative_activation") should have size (1)
  }

  val sent2 = "Ubiquitinated Ras activates Raf and PI3K."
  val sent2b = "Ubiquitinated Ras increases Raf and PI3K activity."
  sent2 should "contain multiple different positive activations" in {
    var mentions = parseSentence(sent2)
    mentions.filter(_.label == "Positive_activation") should have size (2)

    mentions = parseSentence(sent2)
    mentions.filter(_.label == "Positive_activation") should have size (2)
  }

  val sent3 = "the phosphorylation of Ras promotes the ubiquitination of MEK"
  sent3 should "contain NO activation events, and a single positive regulation" in {
    val mentions = parseSentence(sent3)
    mentions.filter(_.label == "Positive_activation") should have size (0)
    mentions.filter(_.label == "Positive_regulation") should have size (1)
  }

  val sent4 = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D), and accordingly, MEK inhibition substantially increased tyrosine phosphorylated ERBB3 levels (Figure 1A)."
  sent4 should "contain 1 negative activation and NO positive activation events" in {
    val mentions = parseSentence(sent4)
    hasNegativeActivation("MEK", "ERBB3", mentions) should be(true)
    hasPositiveActivation("MEK", "ERBB3", mentions) should be(false)
    mentions.filter(_.label.contains("Positive_regulation")) should have size (0)
    mentions.filter(_.label.contains("Negative_regulation")) should have size (1)
    hasNegativeRegulationByEntity("MEK", "Binding", List("ERBB3", "PI3K"), mentions) should be(true)
  }

  val sent5 = "the suppression of ASPP1 decreases ASPP2."
  sent5 should "contain 1 positive activation and NO negative activation or regulation events" in {
    val mentions = parseSentence(sent5)
    hasNegativeActivation("ASPP1", "ASPP2", mentions) should be(false)
    hasPositiveActivation("ASPP1", "ASPP2", mentions) should be(true)
    mentions.filter(_.label.contains("regulation")) should have size (0)
  }

  val sent6 = "ASPP1 is an activator of ASPP2"
  sent6 should "contain 1 positive activation event" in {
    val mentions = parseSentence(sent6)
    hasNegativeActivation("ASPP1", "ASPP2", mentions) should be(false)
    hasPositiveActivation("ASPP1", "ASPP2", mentions) should be(true)
    mentions.filter(_.label.contains("regulation")) should have size (0)
  }

  val sent7 = "ASPP1 is an inhibitor of ASPP2"
  sent7 should "contain 1 negative activation event" in {
    val mentions = parseSentence(sent7)
    hasNegativeActivation("ASPP1", "ASPP2", mentions) should be(true)
    hasPositiveActivation("ASPP1", "ASPP2", mentions) should be(false)
    mentions.filter(_.label.contains("regulation")) should have size (0)
  }

  val sent8 = "The ASPP2-binding activity of CREB is, in most cases, constitutive."
  sent8 should "contain a binding but not an activation or regulation event" in {
    val mentions = parseSentence(sent8)
    mentions.filter(_.label.contains("activation")) should have size (0)
    mentions.filter(_.label.contains("regulation")) should have size (0)
    hasEventWithArguments("Binding", List("ASPP2", "CREB"), mentions) should be(true)
  }

  /*  val sent9 = "HOXB7 overexpression induced a decrease of c-FOS"
  sent9 should "contain 1 negative activation and 0 positive ones" in {
    val mentions = parseSentence(sent9)
    mentions.filter(_.label.contains("Transcription")) should have size (1)
    mentions.filter(_.label.contains("Negative_activation")) should have size (1)
  }*/

  val sent10 = "The suppression of ASPP1 increases the inhibition of ASPP2."
  sent10 should "contain 1 positive activation and 0 negative ones" in {
    val mentions = parseSentence(sent10)
    mentions.filter(_.label.contains("Positive_activation")) should have size (1)
    mentions.filter(_.label.contains("Negative_activation")) should have size (0)
  }

  // Controller and Controlled cannot be the same entity
  val sent11 = "MEK activates MEK."
  sent11 should "not contain a positive activation" in {
    val mentions = parseSentence(sent11)
    mentions.filter(_.label.contains("Positive_activation")) should have size (0)
  }

  val sent12 = "mTOR inhibitor Rapamycin"
  sent12 should "contain a negative activation" in {
    val mentions = parseSentence(sent12)
    hasNegativeActivation("Rapamycin", "mTOR", mentions) should be(true)
  }

  val sent13 = "mTOR activator Rapamycin"
  sent13 should "contain a positive activation" in {
    val mentions = parseSentence(sent13)
    hasPositiveActivation("Rapamycin", "mTOR", mentions) should be(true)
  }

  val sent14 = "Rapamycin, an inhibitor of the mTOR kinase,"
  sent14 should "contain a negative activation" in {
    val mentions = parseSentence(sent14)
    hasNegativeActivation("Rapamycin", "mTOR", mentions) should be(true)
  }

  val sent15 = "Rapamycin, an activator of the mTOR kinase,"
  sent15 should "contain a positive activation" in {
    val mentions = parseSentence(sent15)
    hasPositiveActivation("Rapamycin", "mTOR", mentions) should be(true)
  }

  val sent16 = "Inhibition of mTOR by rapamycin has been standard treatment"
  sent16 should "contain a negative activation (MARCO)" in {
    val mentions = parseSentence(sent16)
    // TODO: this works when the text is "Inhibition of mTOR by rapamycin" but not for the full text
    hasNegativeActivation("rapamycin", "mTOR", mentions) should be(true)
  }

  val sent17 = "XRCC1 stimulates DNA-PK enzymatic activity"
  sent17 should "contain 1 activation" in {
    val mentions = parseSentence(sent17)
    hasPositiveActivation("XRCC1", "DNA-PK", mentions) should be(true)
  }

  val sent18 = "Reciprocally, XRCC1 stimulates the kinase activity of DNA-PK on serine 15 of p53 in vitro"
  sent18 should "contain 1 activation" in {
    val mentions = parseSentence(sent18)
    hasPositiveActivation("XRCC1", "DNA-PK", mentions) should be(true)
  }

  val sent19 = "XRCC1 stimulates DNA-PK catalytic activity in vitro"
  sent19 should "contain 1 activation" in {
    val mentions = parseSentence(sent19)
    hasPositiveActivation("XRCC1", "DNA-PK", mentions) should be(true)
  }

  val sent20 = "Taken together, these data indicate that XRCC1 strongly stimulates DNA-PK activity and that this stimulatory effect is weakened in the mutant S371D that mimics a phosphorylated status of the BRCT1 domain."
  sent20 should "contain 1 activation" in {
    val mentions = parseSentence(sent20)
    hasPositiveActivation("XRCC1", "DNA-PK", mentions) should be(true)
  }

  val sent21 = "The phosphorylation of MEK activates K-Ras."
  sent21 should "contain 1 activation with a phosphorylation event as its controller" in {
    val mentions = parseSentence(sent21)
    val activations = mentions.filter(_ matches "ActivationEvent")
    activations.length should be(1)
    activations.head.arguments("controller").head matches "Phosphorylation" should be(true)
  }

  val sent22 = "The phosphorylation of MEK deactivates K-Ras."
  sent22 should "contain 1 Negative Activation with a phosphorylation event as its controller" in {
    val mentions = parseSentence(sent22)
    val negActs = mentions.filter(_ matches "Negative_activation")
    negActs.length should be(1)
    negActs.head.arguments("controller").head matches "Phosphorylation" should be(true)
    // We shouldn't pick up any Positive Activations
    mentions.count(_ matches "Positve_activation") should be(0)
  }

  val sent23 = "ASPP1 is common, as is its inhibition by ASPP2."
  sent23 should "contain 1 activation" in {
    val mentions = parseSentence(sent23)
    mentions.filter(_ matches "ActivationEvent") should have size (1)
    hasNegativeActivation("ASPP1", "ASPP2", mentions) should be(true)
  }
}