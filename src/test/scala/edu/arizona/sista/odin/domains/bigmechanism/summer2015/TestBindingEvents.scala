package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.TestUtils._

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

  val sent9 = "Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis"
  sent9 should "contain 2 binding events" in {
    val mentions = parseSentence(sent9)
    hasEntity("RAS-GTP", mentions) should be (true)
    hasEntity("RAS", mentions) should be (true)
    hasEntity("p53", mentions) should be (true)
    hasEntity("ASPP1", mentions) should be (true)
    hasEntity("ASPP2", mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP1", "RAS-GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP2", "RAS-GTP"), mentions) should be (true)
  }

  val sent10 = "Moreover, the RAS-ASPP interaction enhances the transcription function of p53 in cancer cells."
  sent10 should "contain 1 binding event" in {
    val mentions = parseSentence(sent10)
    // TODO: this requires the splitting of the complex, so it's probably Ok to miss for now
    hasEventWithArguments("Binding", List("RAS", "ASPP"), mentions)
  }

  val sent11 = "As expected based on previous studies, wild- type K-Ras bound primarily 32P-GDP, while G12V-Ras bound 32P-GTP (Fig.2, A and B)."
  sent11 should "contain 2 binding events" in {
    val mentions = parseSentence(sent11)
    hasEventWithArguments("Binding", List("K-Ras", "32P-GDP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("G12V-Ras", "32P-GTP"), mentions) should be (true)
  }

  val sent12 = "GTP loaded Ras induces multiple signaling pathways by binding to its numerous effectors such as Raf and PI3K."
  sent12 should "contain 2 binding events" in {
    val mentions = parseSentence(sent12)
    hasEventWithArguments("Binding", List("Raf", "Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "Raf"), mentions) should be (false)
  }

  val sent13 = "ERK negatively regulates the epidermal growth factor mediated interaction of Gab1 and the phosphatidylinositol 3-kinase."
  sent13 should "contain 1 binding event" in {
    val mentions = parseSentence(sent13)
    hasEventWithArguments("Binding", List("Gab1", "phosphatidylinositol 3-kinase"), mentions) should be (true)
  }

  val sent14 = "Raf and PI3K bind more to ubiquitinated Ras than to non- ubiquitinated Ras To examine whether the binding of ubiquitinated K-Ras to Raf and PI3K inhibits or can actually enhance their kinase activity, both total G12V-K-Ras and the ubiquitinated subfraction of G12V-K-Ras were purified from cell lysates and subjected to an in vitro kinase (I.V.K.) assay (Fig. 4A)."
  sent14 should "contain 2 binding events" in {
    val mentions = parseSentence(sent14)
    hasEventWithArguments("Binding", List("Raf", "K-Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "K-Ras"), mentions) should be (true)
  }

  val sent15 = "To address the effect of K-Ras ubiquitination on its binding to PI3K and Raf family members, either total G12V-K-Ras or the ubiquitinated subfraction of G12V-K-Ras was immunoprecipitated and the immunoprecipitates were probed with antibodies to detect associated Ras effector molecules."
  sent15 should "contain 2 binding events" in {
    val mentions = parseSentence(sent15)
    hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions) should be (true)
    // TODO: this requires coref!
    hasEventWithArguments("Binding", List("K-Ras", "Raf"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "K-Ras"), mentions) should be (true)
  }

  val sent16 = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D), and accordingly, MEK inhibition substantially increased tyrosine phosphorylated ERBB3 levels (Figure 1A)."
  sent16 should "contain 1 binding event" in {
    val mentions = parseSentence(sent16)
    hasEventWithArguments("Binding", List("PI3K", "ERBB3"), mentions) should be (true)
  }

  val sent17 = "We demonstrate that the RBD of PI3KC2β binds nucleotide-free Ras in vitro."
  sent17 should "containing a binding betweent PI3KC2β and Ras" in {

    val mentions = parseSentence(sent17)

    val f = mentions.filter(_.label == "Family")
    f should have size (1)
    val p = mentions.filter(_.label == "Gene_or_gene_product")
    p should have size (1)
    // This tests the whether the modification is present
    p.head.toBioMention.modifications should have size (1)
    // TODO: Dane
    val b = mentions.filter(_.label == "Binding")
    b should have size (1)
    // FIXME how should RBD be handled?  As a binding Site?
  }

  "testBindingDecl1" should "find 2 binding events" in {
    val mentions = parseSentence("Mechanistically, ASPP1 and ASPP2 bind RAS-GTP.")
    hasEventWithArguments("Binding", List("ASPP1", "RAS-GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP2", "RAS-GTP"), mentions) should be (true)
  }

  "testBindingDecl2" should "find 2 binding events" in {
    val mentions = parseSentence("Mechanistically, ASPP1 and ASPP2 bind with RAS-GTP.")
    hasEventWithArguments("Binding", List("ASPP1", "RAS-GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP2", "RAS-GTP"), mentions) should be (true)
  }

  "testBindingPass1" should "find 2 binding events" in {
    val mentions = parseSentence("Mechanistically, ASPP1 and ASPP2 are bound by RAS-GTP.")
    hasEventWithArguments("Binding", List("ASPP1", "RAS-GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP2", "RAS-GTP"), mentions) should be (true)
  }

  "testBindingPrepNom1" should "find 1 binding events" in {
    val mentions = parseSentence("We detected elevated binding of p53 to K-Ras.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingPrepNom2" should "find 1 binding events" in {
    val mentions = parseSentence("We detected elevated binding of p53 and K-Ras.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingPrepNom3" should "find 1 binding events" in {
    val mentions = parseSentence("We detected elevated binding of p53 with K-Ras.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingSubjNom1" should "find 1 binding events" in {
    val mentions = parseSentence("We detected elevated p53 binding to K-Ras.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingObjNom1" should "find 1 binding events" in {
    val mentions = parseSentence("We detected elevated K-Ras binding by p53.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingSubjRel1" should "find 1 binding events" in {
    val mentions = parseSentence("We detected elevated phosphorylation of K-Ras, a protein that subsequently binds p53.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingObjRel1" should "find 1 binding events" in {
    val mentions = parseSentence("We detected elevated phosphorylation of K-Ras, a protein that is subsequently bound by p53.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

}
