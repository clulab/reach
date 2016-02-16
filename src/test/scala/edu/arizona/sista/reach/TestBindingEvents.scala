package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.odin.State
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.reach.mentions._
import TestUtils._

/**
 * Unit tests to ensure Binding rules are matching correctly
 * Date: 5/19/15
 */
class TestBindingEvents extends FlatSpec with Matchers {

  val sent1 = "The ubiquitinated Ras binds AKT and ASPP2."
  sent1 should "contain only binary bindings" in {
    val mentions = getBioMentions(sent1)

    // this MUST produce Binding(Ras, AKT) and Binding(Ras, ASPP2)

    val bindings = mentions.filter(_ matches "Binding")
    bindings should have size (2) // we must have exactly two bindings here

    for (b <- bindings) {
      b.arguments.get("theme").get should have size (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.exists(_.text == "Ras") should be (true) // Ras is a theme in all these
    }
  }

  val sent2 = "The ubiquitinated Ras protein binds AKT."
  sent2 should "contain a binding when an entity modifies \"protein\"" in {
    val mentions = getBioMentions(sent2)

    // this MUST produce Binding(Ras, AKT)
    val bindings = mentions.filter(_ matches "Binding")
    bindings should have size (1) // we must have exactly 1 binding2 here

    for (b <- bindings) {
      b.arguments.get("theme").get should have size (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.exists(_.text == "Ras") should be (true) // Ras is a theme
    }
  }

  val sent3 = "AKT binds AKT."
  sent3 should "not contain a binding between two mentions of the same protein" in {
    val mentions = getBioMentions(sent3)

    // this MUST produce no bindings!
    val binds = mentions.find(_ matches "Binding")
    binds.isDefined should be(false)
  }

  val sent3b = "binding to the L858R EGFR"
  sent3b should "not contain a binding between two mentions of the same protein" in {
    val mentions = getBioMentions(sent3b)

    // this MUST produce no bindings!
    // something fishy because of PwS (GUS)

    val binds = mentions.find(_ matches "Binding")
    binds.isDefined should be (false)
  }

  val sent4 = "The AKT binding was successful."
  sent4 should "not contain unary bindings" in {
    val mentions = getBioMentions(sent4)
    val bindings = mentions.find(_ matches "Binding")
    bindings.isDefined should be (false)
  }

  val sent5 = "Figure 3. Raf and PI3K bind to ubiquitinated Ras."
  val sent5b = "Figure 3. Raf and PI3K bind more to ubiquitinated Ras than to non-ubiquitinated Ras."
  it should "extract correct bindings with theme and theme2" in {
    var mentions = getBioMentions(sent5)
    var bs = mentions.filter(_ matches "Binding")
    bs should have size (2)
    for (b <- bs) {
      b.arguments.get("theme").get should have size (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.exists(_.text == "Ras") should be (true) // Ras is a theme in all these
    }

    mentions = getBioMentions(sent5b)
    bs = mentions.filter(_ matches "Binding")
    bs should have size (2)
    for (b <- bs) {
      b.arguments.get("theme").get should have size (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.exists(_.text == "Ras") should be (true) // Ras is a theme in all these
    }
  }

  val sent6 = "(B) RAS activation enhances the binding of wild-type ASPP2 but not ASPP2 (S827A) to p53."
  sent6 should "not contain a binding" in {
    val doc = testReach.mkDoc(sent6, "testdoc")
    val mentions = testReach extractFrom doc
    assert(!mentions.exists(_ matches "Binding"))
  }

  val sent7 = "Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis [2]."
  sent7 should "not include p53 in binding" in {
    val doc = testReach.mkDoc(sent7, "testdoc")
    val mentions = testReach extractFrom doc
    val participants = Set("ASPP1", "ASPP2", "RAS-GTP")
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants).isEmpty))
  }

  val sent8 = "We measured the rate of GAP mediated GTP hydrolysis and observed that the response of Ras ligated to UbiquitinC77 was identical to Ras ligated to UbiquitinG76C."
  sent8 should "have two ubiquitinations with correct arguments" in {
    val doc = testReach.mkDoc(sent8, "testdoc")
    val mentions = testReach extractFrom doc
    val participants1 = Set("Ras", "UbiquitinC77")
    val participants2 = Set("Ras", "UbiquitinG76C")
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants1).isEmpty))
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants2).isEmpty))
  }

  val sent9 = "Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis"
  sent9 should "contain 2 binding events" in {
    val mentions = getBioMentions(sent9)
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
    val mentions = getBioMentions(sent10)
    // TODO: this requires the splitting of the complex, so it's probably Ok to miss for now
    hasEventWithArguments("Binding", List("RAS", "ASPP"), mentions)
  }

  val sent11 = "As expected based on previous studies, wild- type K-Ras bound primarily 32P-GDP, while G12V-Ras bound 32P-GTP (Fig.2, A and B)."
  sent11 should "contain 2 binding events" in {
    val mentions = getBioMentions(sent11)
    hasEventWithArguments("Binding", List("K-Ras", "32P-GDP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("G12V-Ras", "32P-GTP"), mentions) should be (true)
  }

  val sent12 = "GTP loaded Ras induces multiple signaling pathways by binding to its numerous effectors such as Raf and PI3K."
  sent12 should "contain 2 binding events" in {
    val mentions = getBioMentions(sent12)
    val b = mentions.filter(_ matches "Binding")
    b should have size (2)
    hasEventWithArguments("Binding", List("Ras", "Raf"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Ras", "PI3K"), mentions) should be (true)
    //hasEventWithArguments("Binding", List("PI3K", "Raf"), mentions) should be (false)
  }

  val sent13 = "ERK negatively regulates the epidermal growth factor mediated interaction of Gab1 and the phosphatidylinositol 3-kinase."
  sent13 should "contain 1 binding event" in {
    val mentions = getBioMentions(sent13)
    hasEventWithArguments("Binding", List("Gab1", "phosphatidylinositol 3-kinase"), mentions) should be (true)
  }

  val sent14 = "Raf and PI3K bind more to ubiquitinated Ras than to non- ubiquitinated Ras To examine whether the binding of ubiquitinated K-Ras to Raf and PI3K inhibits or can actually enhance their kinase activity, both total G12V-K-Ras and the ubiquitinated subfraction of G12V-K-Ras were purified from cell lysates and subjected to an in vitro kinase (I.V.K.) assay (Fig. 4A)."
  sent14 should "contain 2 binding events" in {
    val mentions = getBioMentions(sent14)
    hasEventWithArguments("Binding", List("Raf", "K-Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "K-Ras"), mentions) should be (true)
  }

  val sent16 = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D), and accordingly, MEK inhibition substantially increased tyrosine phosphorylated ERBB3 levels (Figure 1A)."
  sent16 should "contain 1 binding event" in {
    val mentions = getBioMentions(sent16)
    hasEventWithArguments("Binding", List("PI3K", "ERBB3"), mentions) should be (true)
  }

  val sent17 = "We demonstrate that the RBD of PI3KC2β binds nucleotide-free Ras in vitro."
  sent17 should "contain a binding between PI3KC2β and Ras" in {

    val mentions = getBioMentions(sent17)

    val f = mentions.filter(_ matches "Family")
    f should have size (1)
    val p = mentions.filter(_ matches "Gene_or_gene_product")
    p should have size (1)
    val b = mentions.filter(_ matches "Binding")
    b should have size (1)
    TestUtils.hasEventWithArguments("Binding", List("PI3KC2beta", "Ras", "RBD"), b) should be (true)
  }

  "testBindingDecl1" should "find 2 binding events" in {
    val mentions = getBioMentions("Mechanistically, ASPP1 and ASPP2 bind RAS-GTP.")
    hasEventWithArguments("Binding", List("ASPP1", "RAS-GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP2", "RAS-GTP"), mentions) should be (true)
  }

  "testBindingDecl2" should "find 2 binding events" in {
    val mentions = getBioMentions("Mechanistically, ASPP1 and ASPP2 bind with RAS-GTP.")
    hasEventWithArguments("Binding", List("ASPP1", "RAS-GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP2", "RAS-GTP"), mentions) should be (true)
  }

  "testBindingPass1" should "find 2 binding events" in {
    val mentions = getBioMentions("Mechanistically, ASPP1 and ASPP2 are bound by RAS-GTP.")
    hasEventWithArguments("Binding", List("ASPP1", "RAS-GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP2", "RAS-GTP"), mentions) should be (true)
  }

  "testBindingPrepNom1" should "find 1 binding events" in {
    val mentions = getBioMentions("We detected elevated binding of p53 to K-Ras.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingPrepNom2" should "find 1 binding events" in {
    val mentions = getBioMentions("We detected elevated binding of p53 and K-Ras.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingPrepNom3" should "find 1 binding events" in {
    val mentions = getBioMentions("We detected elevated binding of p53 with K-Ras.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingSubjNom1" should "find 1 binding events" in {
    val mentions = getBioMentions("We detected elevated p53 binding to K-Ras.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingObjNom1" should "find 1 binding events" in {
    val mentions = getBioMentions("We detected elevated K-Ras binding by p53.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingSubjRel1" should "find 1 binding events" in {
    val mentions = getBioMentions("We detected elevated phosphorylation of K-Ras, a protein that subsequently binds p53.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  "testBindingObjRel1" should "find 1 binding events" in {
    val mentions = getBioMentions("We detected elevated phosphorylation of K-Ras, a protein that is subsequently bound by p53.")
    hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions) should be (true)
  }

  val sent18 = "Nucleotide free Ras binds to MEK"
  sent18 should "not find a binding betwee Nucleotide and MEK" in {
    val doc = testReach.mkDoc(sent18, "testdoc")
    val state = State(Seq(
      new BioTextBoundMention(Seq("Simple_chemical", "BioChemicalEntity", "PossibleController"), Interval(0), 0, doc, true, "<test>")
    ))
    val mentions = testReach.extractFrom(doc)
    val bindings = mentions.filter(_ matches "Binding")
    bindings should have size (1)
    val binding = bindings.head
    binding.arguments should contain key ("theme")
    binding.arguments("theme") should have size (2)
    binding.arguments("theme") foreach { theme =>
      theme.text should not be ("Nucleotide")
    }
  }

  "testBindingGerund1" should "find 1 binding event" in {
    val mentions = getBioMentions("IKKgamma appears capable of binding linear polyubiquitin.")
    hasEventWithArguments("Binding", List("IKKgamma", "polyubiquitin"), mentions) should be (true)
  }

  val sent19 = "The dimerization of cRaf with BRaf helps something."
  sent19 should "contain 1 binding event" in {
    val mentions = getBioMentions(sent19)
    hasEventWithArguments("Binding", List("cRaf", "BRaf"), mentions) should be (true)
  }

  val sent20 = "MEK binds with MEK."
  sent20 should "not contain any binding events" in {
    val mentions = getBioMentions(sent20)
    mentions.count(_ matches "Binding") should be (0)
  }

  val sent21 = "Highly purified DNA-PKcs, Ku70/Ku80 heterodimer and the two documented XRCC1 binding partners LigIII and DNA polbeta were dot-blotted"
  sent21 should "contain 1 binding event between Ku70 and Ku80" in {
    val mentions = getBioMentions(sent21)
    hasEventWithArguments("Binding", List("Ku70", "Ku80"), mentions) should be (true)
  }
  val sent22 = "The heterodimer Ku70-DNA ligase IV is awesome"
  sent22 should "contain 1 binding event between Ku70 and DNA ligase IV" in {
    val mentions = getBioMentions(sent22)
    hasEventWithArguments("Binding", List("Ku70", "DNA ligase IV"), mentions) should be (true)
  }
  val sent23 = "The complex Ku70/Ku80 is awesome"
  sent23 should "contain 1 binding event between Ku70 and Ku80" in {
    val mentions = getBioMentions(sent23)
    hasEventWithArguments("Binding", List("Ku70", "Ku80"), mentions) should be (true)
  }
  val sent24 = "That Ku70/Ku80 complex is awesome"
  sent24 should "contain 1 binding event between Ku70 and Ku80" in {
    val mentions = getBioMentions(sent24)
    hasEventWithArguments("Binding", List("Ku70", "Ku80"), mentions) should be (true)
  }

  val sent25 = "Identification by mass spectroscopy of DNA-PKcs associated with XRCC1"
  sent25 should "contain 1 binding event" in {
    val mentions = getBioMentions(sent25)
    hasEventWithArguments("Binding", List("DNA-PKcs", "XRCC1"), mentions) should be (true)
  }
  val sent26 = "Our assumption is that DNA-PKcs is associated with  XRCC1"
  sent26 should "contain 1 binding event" in {
    val mentions = getBioMentions(sent26)
    hasEventWithArguments("Binding", List("DNA-PKcs", "XRCC1"), mentions) should be (true)
  }

  val sent27 = "Once bound to the DSB, the DNA-PK holoenzyme facilitates the recruitment..."
  sent27 should "contain 1 binding event (MARCO)" in {
    val mentions = getBioMentions(sent27)
    // TODO: this should be matched by the binding_oncebound rule, but it doesn't...
    hasEventWithArguments("Binding", List("DNA-PK holoenzyme", "DSB"), mentions) should be (true)
  }

  val sent28 = "To confirm whether XRCC1 and DNA-PK coexist in a common complex, we carried out co-immunoprecipitation experiments in HeLa nuclear extracts."
  sent28 should "contain 1 binding event" in {
    val mentions = getBioMentions(sent28)
    hasEventWithArguments("Binding", List("DNA-PK", "XRCC1"), mentions) should be (true)
  }

  val sent29 = "We found that the three subunits of DNA-PK co-purified only with BRCT1 containing XRCC1-fusion proteins             confirming that XRCC1 and DNA-PK are present in a complex. "
  sent29 should "contain 1 binding event" in {
    val mentions = getBioMentions(sent29)
    hasEventWithArguments("Binding", List("DNA-PK", "XRCC1"), mentions) should be (true)
  }

  val sent30 = "Figure 3. Raf and PI3K bind more to ubiquitinated Ras than to non-ubiquitinated Ras"
  sent30 should "contain 2 binding events" in {
    val mentions = getBioMentions(sent30)
    hasEventWithArguments("Binding", List("Raf", "Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "Ras"), mentions) should be (true)
  }

  val sent31 = "We observed that endogenous PLC, Afadin, Calmodulin and Tubulin co-immunoprecipitated with G12V-K-Ras (GUS)"
  sent31 should "contain 4 binding events for G12V-K-Ras" in {
    val mentions = getBioMentions(sent31)
    // TODO: this doesn't work because we don't recognize G12V-K-Ras as entity. Please add it to the model.
    hasEventWithArguments("Binding", List("PLC", "G12V-K-Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Afadin", "G12V-K-Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Calmodulin", "G12V-K-Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Tubulin", "G12V-K-Ras"), mentions) should be (true)
  }

  val sent32 = "Something increases the fraction of GTP-bound Ras"
  sent32 should "contain 1 binding event" in {
    val mentions = getBioMentions(sent32)
    hasEventWithArguments("Binding", List("GTP", "Ras"), mentions) should be (true)
  }

  val sent33 = "As expected based on previous studies, wild-type K-Ras bound primarily 32P-GDP, while G12V-Ras bound 32P-GTP (Fig.2, A and B)."
  sent33  should "contain 2 binding events" in {
    val mentions = getBioMentions(sent33)
    hasEventWithArguments("Binding", List("K-Ras", "32P-GDP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("G12V-Ras", "32P-GTP"), mentions) should be (true)
  }

  val sent34 = "Recruitment of p53 to the p21 or PUMA promoter was normalized to input."
  sent34 should "contain 2 binding events" in {
    val mentions = getBioMentions(sent34)
    mentions.filter(_ matches "Binding") should have size (2)
    hasEventWithArguments("Binding", List("p53", "p21"), mentions) should be (true)
    hasEventWithArguments("Binding", List("p53", "PUMA"), mentions) should be (true)
  }

  val sent35 = "LMTK2 signals via protein phosphatase-1C (PP1C) to increase inhibitory phosphorylation of GSK3β on serine-9 that reduces KLC2 phosphorylation and promotes binding of the known KLC2 cargo Smad2."
  sent35 should "not contain bindings" in {
    val mentions = getBioMentions(sent35)
    mentions.filter(_ matches "Binding") should be ('empty)
  }

  // original example not met because last entity is not detected
  // val sent36 = "Lower: purified wild-type and mutant p32, but not recombinant TFAM-His and GST proteins, bind to poly(A) oligonucleoside."
  val sent36 = "Lower: purified wild-type and mutant p32, but not recombinant ASPP2 and GST proteins, bind to Mek."
  sent36 should "not contain binding between p32 and ASPP2 or GST" in {
    val mentions = getBioMentions(sent36)
    mentions filter (_ matches "Binding") should have size (3)
    hasEventWithArguments("Binding", List("p32", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP2", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("GST", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("p32", "ASPP2"), mentions) should be (false)
    hasEventWithArguments("Binding", List("p32", "GST"), mentions) should be (false)
  }

  // Special example from assembly requirement doc
  val sent37a = "Src tyrosyl phosphorylates Ras that is GTP bound"
  val sent37b = "Ras that is GTP bound is phosphorylated by Src tyrosyl"
  sent37a should "contain a binding between GTP and Ras" in {
    val mentions = getBioMentions(sent37a)
    hasEventWithArguments("Binding", List("Ras", "GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Src", "GTP"), mentions) should be (false)
    hasEventWithArguments("Binding", List("Ras", "Src"), mentions) should be (false)
  }
  sent37b should "contain a binding between GTP and Ras" in {
    val mentions = getBioMentions(sent37b)
    hasEventWithArguments("Binding", List("Ras", "GTP"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Src", "GTP"), mentions) should be (false)
    hasEventWithArguments("Binding", List("Ras", "Src"), mentions) should be (false)
  }

}
