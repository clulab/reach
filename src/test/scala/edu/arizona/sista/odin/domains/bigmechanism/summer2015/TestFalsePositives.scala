package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest._
import TestResources._
import DarpaEvalUtils._

/**
 * Unit tests to make sure we do not over predict
 */
class TestFalsePositive extends FlatSpec with Matchers {
  val sentence1 = "(B) RAS activation enhances the binding of wild-type ASPP2 but not ASPP2 (S827A) to p53."
  val sentence2 = "Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis [2]."
  val sentence3 = "Phosphorylation of ASPP2 by MAPK is required for RAS induced increased binding to p53 and increased transactivation of pro-apoptotic genes."
  val sentence4 = "We measured the rate of GAP mediated GTP hydrolysis and observed that the response of Ras ligated to UbiquitinC77 was identical to Ras ligated to UbiquitinG76C."
  val sentence5 = "GTP bound Ras promotes the phosphorylation of Mek."
  val sentence6 = "Phosphorylated Mek binds to GTP."
  val sentence7 = "We found that Mek promotes the phosphorylation of hydrolyzed Ras."
  val sentence8 = "Ligation of ASPP2 to hydroxylated RAS-GTP promotes apoptosis."
  val sentence9 = "Optineurin regulates NF-kappaB activation by mediating interaction of CYLD with ubiquitinated RIP."

  sentence1 should "not contain a binding" in {
    val doc = reach.mkDoc(sentence1, "testdoc")
    val mentions = reach extractFrom doc
    assert(!mentions.exists(_.label == "Binding"))
  }

  sentence2 should "not include p53 in binding" in {
    val doc = reach.mkDoc(sentence2, "testdoc")
    val mentions = reach extractFrom doc
    val participants = Set("ASPP1", "ASPP2", "RAS-GTP")
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants).isEmpty))
  }

  sentence3 should "have an up-regulated phosphorylation" in {
    val doc = reach.mkDoc(sentence3, "testdoc")
    val mentions = reach extractFrom doc
    assert(hasPositiveRegulationByEntity("MAPK", "Phosphorylation", Seq("ASPP2", "MAPK"), mentions))
  }

  sentence4 should "have two bindings with correct arguments" in {
    val doc = reach.mkDoc(sentence4, "testdoc")
    val mentions = reach extractFrom doc
    val participants1 = Set("Ras", "UbiquitinC77")
    val participants2 = Set("Ras", "UbiquitinG76C")
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants1).isEmpty))
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants2).isEmpty))
  }

  sentence5 should "not contain a binding event (this is a PTM)" in {
    val doc = reach.mkDoc(sentence5, "testdoc")
    val mentions = reach extractFrom doc
    mentions.exists(_.label == "Binding") should be (false)
  }

  sentence6 should "not contain a phosphorylation event (this is a PTM)" in {
    val doc = reach.mkDoc(sentence6, "testdoc")
    val mentions = reach extractFrom doc
    mentions.exists(_.label == "Phosphorylation") should be (false)
  }

  sentence7 should "not contain a hydrolysis event (this is a PTM)" in {
    val doc = reach.mkDoc(sentence7, "testdoc")
    val mentions = reach extractFrom doc
    mentions.exists(_.label == "Hydrolysis") should be (false)
  }

  sentence8 should "not contain a hydroxylation event (this is a PTM)" in {
    val doc = reach.mkDoc(sentence8, "testdoc")
    val mentions = reach extractFrom doc
    mentions.exists(_.label == "Hydroxylation") should be (false)
  }

  sentence9 should "not contain a ubiquitination event (this is a PTM)" in {
    val doc = reach.mkDoc(sentence8, "testdoc")
    val mentions = reach extractFrom doc
    mentions.exists(_.label == "Ubiquitination") should be (false)
  }

}
