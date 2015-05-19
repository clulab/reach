package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
 * Unit tests to ensure Binding rules are matching correctly
 * Date: 5/19/15
 */
class TestBindingEvents extends FlatSpec with Matchers {

  // TODO: don't report AKT + ASPP2 as a binding
  val sent1 = "The ubiquitinated Ras binds AKT and ASPP2."
  sent1 should "contain only binary bindings" in {
    val mentions = parseSentence(sent1)

    // this MUST produce Binding(Ras, AKT) and Binding(Ras, ASPP2)
    // TODO: fails! Produces 3 bindings, instead of 2! (MARCO)

    val bindings = mentions.filter(_.label == "Binding")
    bindings should have size (2) // we must have exactly two bindings here

    for (b <- bindings) {
      b.arguments.get("theme").get should have size (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.find(_.text == "Ras").isDefined should be (true) // Ras is a theme in all these
    }
  }

  val sent2 = "The ubiquitinated Ras protein binds AKT."
  sent2 should "contain a binding when an entity modifies \"protein\"" in {
    val mentions = parseSentence(sent2)

    // this MUST produce Binding(Ras, AKT)
    val bindings = mentions.filter(_.label == "Binding")
    bindings should have size (1) // we must have exactly 1 binding2 here

    for (b <- bindings) {
      b.arguments.get("theme").get should have size (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.find(_.text == "Ras").isDefined should be (true) // Ras is a theme
    }
  }

  val sent3 = "AKT binds AKT."
  sent3 should "not contain a binding between two mentions of the same protein" in {
    val mentions = parseSentence(sent3)

    // this MUST produce no bindings!
    val binds = mentions.find(_.label == "Binding")
    binds.isDefined should be(false)
  }

  val sent3b = "binding to the L858R EGFR"
  sent3b should "not contain a binding between two mentions of the same protein" in {
    val mentions = parseSentence(sent3b)

    // this MUST produce no bindings!
    // something fishy because of PwS (GUS)

    val binds = mentions.find(_.label == "Binding")
    binds.isDefined should be (false)
  }

  val sent4 = "The AKT binding was successful."
  sent4 should "not contain unary bindings" in {
    val mentions = parseSentence(sent4)
    val bindings = mentions.find(_.label == "Binding")
    bindings.isDefined should be (false)
  }

  val sent5 = "Figure 3. Raf and PI3K bind to ubiquitinated Ras."
  val sent5b = "Figure 3. Raf and PI3K bind more to ubiquitinated Ras than to non-ubiquitinated Ras."
  it should "extract correct bindings with theme and theme2" in {
    var mentions = parseSentence(sent5)
    var bs = mentions.filter(_.label == "Binding")
    bs should have size (2)
    for (b <- bs) {
      b.arguments.get("theme").get should have size (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.find(_.text == "Ras").isDefined should be (true) // Ras is a theme in all these
    }

    mentions = parseSentence(sent5b)
    bs = mentions.filter(_.label == "Binding")
    bs should have size (2)
    for (b <- bs) {
      b.arguments.get("theme").get should have size (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.find(_.text == "Ras").isDefined should be (true) // Ras is a theme in all these
    }
  }

  val sent6 = "(B) RAS activation enhances the binding of wild-type ASPP2 but not ASPP2 (S827A) to p53."
  sent6 should "not contain a binding" in {
    val doc = testReach.mkDoc(sent6, "testdoc")
    val mentions = testReach extractFrom doc
    assert(!mentions.exists(_.label == "Binding"))
  }

  val sent7 = "Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis [2]."
  sent7 should "not include p53 in binding" in {
    val doc = testReach.mkDoc(sent7, "testdoc")
    val mentions = testReach extractFrom doc
    val participants = Set("ASPP1", "ASPP2", "RAS-GTP")
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants).isEmpty))
  }

  val sent8 = "We measured the rate of GAP mediated GTP hydrolysis and observed that the response of Ras ligated to UbiquitinC77 was identical to Ras ligated to UbiquitinG76C."
  sent8 should "have two bindings with correct arguments" in {
    val doc = testReach.mkDoc(sent8, "testdoc")
    val mentions = testReach extractFrom doc
    val participants1 = Set("Ras", "UbiquitinC77")
    val participants2 = Set("Ras", "UbiquitinG76C")
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants1).isEmpty))
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants2).isEmpty))
  }


}
