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
  sent2 should "contain multiple different positive activations" in {
    val mentions = parseSentence(sent2)
    mentions.filter(_.label == "Positive_activation") should have size (2)
  }

  val sent3 = "the phosphorylation of Ras promotes the ubiquitination of MEK"
  sent3 should "contain NO activation events; this a a positive regulation" in {
    val mentions = parseSentence(sent3)
    mentions.filter(_.label == "Positive_activation") should have size (0)
  }
}
